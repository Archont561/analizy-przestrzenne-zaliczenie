box::use(. / helpers)

# CONFIG & INIT

root <- ".."

logger <- helpers$PipelineLogger$new(root)
cache <- helpers$CacheManager$new(root, logger = logger)

terra_tmp <- file.path(root, ".cache", "tmp")
dir.create(terra_tmp, recursive = TRUE, showWarnings = FALSE)

terra::terraOptions(
  tempdir = terra_tmp,
  memfrac = 0.4,
  todisk = TRUE,
  progress = 1
)

try(terra::tmpFiles(remove = TRUE), silent = TRUE)

mapping_dir <- file.path(root, "data")
teryt <- "1216"
year <- 2021
sources <- c("CLC", "S2GLC", "BDOT10k")

invisible(helpers$fix_gdal_proj(logger))
set.seed(123)

logger$log(
  "pipeline_config",
  teryt = teryt,
  year = year,
  sources = paste(sources, collapse = ",")
)

unified_meta <- helpers$get_unified_meta(mapping_dir)

# LOCAL REPORT

as_border_v <- function(border_sf, target = NULL) {
  v <- terra::vect(border_sf)
  
  if (!is.null(target)) {
    v <- terra::project(v, terra::crs(target))
  }
  
  v
}

raster_freq_table <- function(r, mapping_dir = NULL, source = NULL) {
  r <- if (!inherits(r, "SpatRaster")) terra::rast(r) else r
  
  f <- as.data.frame(terra::freq(r))
  f <- f[!is.na(f$value), , drop = FALSE]
  
  if (nrow(f) == 0) {
    return(data.frame())
  }
  
  f$area_km2 <- f$count * prod(terra::res(r)) / 1e6
  
  if (!is.null(mapping_dir)) {
    meta <- helpers$get_unified_meta(mapping_dir)
    f$class_code <- meta$code[match(f$value, meta$value)]
    
    if ("name" %in% names(meta)) {
      f$class_name <- meta$name[match(f$value, meta$value)]
    }
  }
  
  if (!is.null(source)) {
    f$source <- source
  }
  
  f[,
    c(
      intersect("source", names(f)),
      "value",
      intersect(c("class_code", "class_name"), names(f)),
      "count",
      "area_km2"
    ),
    drop = FALSE
  ]
}

s2glc_freq_table <- function(r, mapping_dir) {
  f <- as.data.frame(terra::freq(r))
  f <- f[!is.na(f$value), , drop = FALSE]
  
  if (nrow(f) == 0) {
    return(data.frame())
  }
  
  meta <- helpers$get_s2glc_meta(mapping_dir)
  
  f$class_name <- meta$class_name[match(f$value, meta$class_id)]
  f$area_km2 <- f$count * prod(terra::res(r)) / 1e6
  
  f[, c("value", "class_name", "count", "area_km2"), drop = FALSE]
}

raster_basic_table <- function(r, name = "raster") {
  r <- if (!inherits(r, "SpatRaster")) terra::rast(r) else r
  
  data.frame(
    name = name,
    nlyr = terra::nlyr(r),
    nrow = terra::nrow(r),
    ncol = terra::ncol(r),
    ncell = terra::ncell(r),
    res_x = terra::res(r)[1],
    res_y = terra::res(r)[2],
    crs = terra::crs(r),
    stringsAsFactors = FALSE
  )
}

band_stats_table <- function(r) {
  r <- if (!inherits(r, "SpatRaster")) terra::rast(r) else r
  
  data.frame(
    band = names(r),
    min = as.numeric(terra::global(r, "min", na.rm = TRUE)[, 1]),
    mean = as.numeric(terra::global(r, "mean", na.rm = TRUE)[, 1]),
    max = as.numeric(terra::global(r, "max", na.rm = TRUE)[, 1]),
    non_na = as.numeric(terra::global(!is.na(r), "sum", na.rm = TRUE)[, 1]),
    stringsAsFactors = FALSE
  )
}

sample_distribution_table <- function(df, source) {
  tab <- as.data.frame(table(df$class_value), stringsAsFactors = FALSE)
  names(tab) <- c("class_value", "n")
  tab$source <- source
  tab[, c("source", "class_value", "n")]
}

model_summary_table <- function(models) {
  data.frame(
    Source = names(models),
    Model = sapply(models, function(x) x$model_type),
    Accuracy = as.numeric(sapply(models, function(x) x$accuracy)),
    Kappa = as.numeric(sapply(models, function(x) x$kappa)),
    OOB_Error = as.numeric(sapply(models, function(x) x$oob_error)),
    Train_N = as.integer(sapply(models, function(x) x$train_n)),
    Test_N = as.integer(sapply(models, function(x) x$test_n)),
    stringsAsFactors = FALSE
  )
}

importance_table <- function(model_obj, source) {
  imp <- model_obj$importance
  
  if (is.null(imp) || length(imp) == 0) {
    return(data.frame())
  }
  
  data.frame(
    source = source,
    variable = names(imp),
    importance = as.numeric(imp),
    stringsAsFactors = FALSE
  )
}

rgb_unique_sample_table <- function(rgb_raster, n = 3000) {
  samp <- terra::spatSample(
    rgb_raster,
    size = n,
    na.rm = TRUE,
    as.df = TRUE,
    warn = FALSE
  )
  
  if (ncol(samp) < 3) {
    return(data.frame())
  }
  
  names(samp)[1:3] <- c("R", "G", "B")
  u <- unique(samp[, 1:3, drop = FALSE])
  u$hex <- sprintf("#%02X%02X%02X", u$R, u$G, u$B)
  u[order(u$hex), , drop = FALSE]
}

area_comparison_table <- function(ref, pred, mapping_dir, source) {
  ref_area <- helpers$area_share(ref, mapping_dir = mapping_dir)
  pred_area <- helpers$area_share(pred, mapping_dir = mapping_dir)
  
  names(ref_area)[names(ref_area) == "count"] <- "ref_count"
  names(ref_area)[names(ref_area) == "area_km2"] <- "ref_area_km2"
  
  names(pred_area)[names(pred_area) == "count"] <- "pred_count"
  names(pred_area)[names(pred_area) == "area_km2"] <- "pred_area_km2"
  
  out <- merge(
    ref_area,
    pred_area,
    by = "class_code",
    all = TRUE
  )
  
  out[is.na(out)] <- 0
  
  out$area_diff_km2 <- out$pred_area_km2 - out$ref_area_km2
  out$area_diff_pct <- ifelse(
    out$ref_area_km2 > 0,
    100 * out$area_diff_km2 / out$ref_area_km2,
    NA_real_
  )
  
  out$source <- source
  out[, c("source", setdiff(names(out), "source")), drop = FALSE]
}

# BDOT10K DATA

logger$stage_start("1. BDOT10k")

index <- cache$cached(
  "file:bdot10k:index",
  quote(helpers$download_bdot10k_index())
)

url <- helpers$get_bdot10k_url(index, teryt)
zip_path <- helpers$download_bdot10k_zip(url, cache, teryt)
bdot_key <- helpers$bdot_zip_to_gpkg(cache, zip_path, teryt)
layer_info <- cache$layers(bdot_key)

logger$output("bdot_layers", layer_info)

# Non-spatial diagnostic plot: bar chart.
# Spatial plots below use terra::plot by default.
if (nrow(layer_info) > 0) {
  cache$plot(
    "plot:01_bdot_layers.png",
    quote({
      ord <- order(layer_info$features, decreasing = TRUE)
      
      graphics::barplot(
        layer_info$features[ord],
        names.arg = layer_info$name[ord],
        las = 2,
        cex.names = 0.5,
        col = "#5b9bd5",
        main = "BDOT10k — liczba obiektów per warstwa",
        ylab = "n"
      )
    }),
    width = 1400,
    height = 700,
    description = paste(
      "Non-spatial diagnostic chart showing the number of BDOT10k features",
      "per layer. The text report contains the same data as a table."
    ),
    report_expr = quote({
      sorted_layers <- layer_info[
        order(layer_info$features, decreasing = TRUE),
      ]
      
      logger$make_plot_report(
        title = "BDOT10k layer feature counts",
        description = "Feature counts for each BDOT10k layer extracted from the county package.",
        inputs = list(bdot_gpkg = bdot_key),
        body = list(
          teryt = teryt,
          total_layers = nrow(layer_info),
          layer_table = sorted_layers
        )
      )
    })
  )
}

logger$stage_done("1. BDOT10k")

# BORDER

logger$stage_start("2. Border")

border_sf <- helpers$get_bdot10k_borders(cache, teryt)
border_v <- terra::vect(border_sf)

logger$output("border_sf", border_sf)
logger$diagnostics("border_sf", border_sf)

cache$plot(
  "plot:02_border.png",
  quote({
    terra::plot(
      border_v,
      col = "#e0e0e0",
      border = "#333333",
      lwd = 2,
      main = paste("Granica powiatu", teryt)
    )
  }),
  description = "Spatial plot created with terra::plot. Shows the extracted county border polygon.",
  report_expr = quote({
    bbox <- sf::st_bbox(border_sf)
    
    bbox_table <- data.frame(
      xmin = bbox["xmin"],
      ymin = bbox["ymin"],
      xmax = bbox["xmax"],
      ymax = bbox["ymax"],
      crs = sf::st_crs(border_sf)$input,
      stringsAsFactors = FALSE
    )
    
    logger$make_plot_report(
      title = paste("County border —", teryt),
      description = "Border polygon extracted from BDOT10k and transformed to EPSG:2180.",
      outputs = list(border = border_sf),
      body = list(
        teryt = teryt,
        bbox_table = bbox_table
      )
    )
  })
)

logger$stage_done("2. Border")

# LANDSAT DATA

logger$stage_start("3. Landsat")

landsat_manifest <- helpers$download_landsat_raw_bands(
  cache = cache,
  border_sf = border_sf,
  year = year,
  area_id = teryt,
  strict_cloud = 2,
  relaxed_cloud = 10,
  min_coverage = 0.999,
  allowed_platforms = c("landsat-8", "landsat-9")
)

# Log scene summary
if (!is.null(landsat_manifest$pipeline_metadata)) {
  meta <- landsat_manifest$pipeline_metadata
  
  helpers$log_landsat_scene_summary(
    items_sf = meta$items_sf,
    border_sf = border_sf,
    selected_date = meta$selected_date,
    cache = cache
  )
  
  cache$plot(
    "plot:03_landsat_scene_footprints.png",
    quote({
      helpers$plot_landsat_scene_coverage(
        items_sf = meta$items_sf,
        border_sf = border_sf,
        selected_date = meta$selected_date,
        main = paste0(
          "Landsat scenes — ",
          meta$selected_date,
          " (n=",
          meta$n_scenes,
          ")"
        )
      )
    }),
    description = "Scene footprints overlaid on the AOI boundary.",
    report_expr = quote({
      logger$make_plot_report(
        title = paste("Landsat scene footprints —", meta$selected_date),
        description = "Visual check of multi-scene AOI coverage.",
        body = list(
          selected_date = meta$selected_date,
          n_scenes = meta$n_scenes,
          scene_ids = meta$scene_ids,
          coverage_table = meta$coverage_table
        )
      )
    })
  )
}

ls_raw_clipped <- cache$cached(
  paste0("raster:landsat:raw_clipped:", teryt, ":", year, ".tif"),
  quote({
    scene_rasters <- lapply(landsat_manifest$scenes, function(scene) {
      r <- terra::rast(unname(scene$paths))
      names(r) <- scene$band_names
      helpers$crop_mask(r, border_sf)
    })
    
    raw_clip <- if (length(scene_rasters) == 1) {
      scene_rasters[[1]]
    } else {
      terra::mosaic(terra::sprc(scene_rasters), fun = "first")
    }
    
    names(raw_clip) <- c("blue", "green", "red", "nir", "swir16")
    
    helpers$detect_landsat_mosaic_gaps(
      landsat_raster = raw_clip,
      border_sf = border_sf,
      max_gap_pct = 1.0,
      cache = cache
    )
    
    helpers$assert_landsat_valid_coverage(
      landsat_raster = raw_clip,
      border_sf = border_sf,
      min_valid_coverage = 0.99,
      cache = cache
    )
    
    raw_clip
  })
)

ls <- cache$cached(
  paste0("raster:landsat:scaled:", teryt, ":", year, ".tif"),
  quote({
    helpers$scale_landsat(
      ls_raw_clipped,
      clamp = TRUE,
      mask_fill = TRUE
    )
  })
)

logger$output("landsat_scaled", ls)
border_v_ls <- as_border_v(border_sf, ls)

cache$plot(
  "plot:03_landsat_rgb.png",
  quote({
    terra::plotRGB(
      ls,
      r = 3,
      g = 2,
      b = 1,
      stretch = "lin",
      main = paste0("Landsat RGB — scaled reflectance — ", year)
    )
    terra::lines(border_v_ls, col = "#ffffff", lwd = 1.5)
  }),
  description = paste(
    "Spatial RGB composite rendered with terra::plotRGB.",
    "Uses scaled Landsat reflectance bands red, green, and blue."
  ),
  report_expr = quote({
    scene_table <- data.frame(
      scene_id = names(landsat_manifest$scenes),
      date = vapply(landsat_manifest$scenes, function(x) x$date, character(1)),
      stringsAsFactors = FALSE
    )
    
    logger$make_plot_report(
      title = paste("Landsat RGB —", year),
      description = "True-color RGB composite using scaled Landsat red, green, and blue bands.",
      inputs = list(landsat_scaled = ls, border = border_sf),
      body = list(
        selected_date = landsat_manifest$best_date,
        scene_table = scene_table,
        raster_info = raster_basic_table(ls, "landsat_scaled"),
        band_statistics = band_stats_table(ls)
      )
    )
  })
)

cache$plot(
  "plot:03_landsat_ndvi.png",
  quote({
    ndvi <- (ls$nir - ls$red) / (ls$nir + ls$red)
    
    terra::plot(
      ndvi,
      main = "NDVI",
      col = terrain.colors(100)
    )
    terra::lines(border_v_ls, col = "#333333", lwd = 1)
  }),
  description = paste(
    "Spatial raster map rendered with terra::plot.",
    "NDVI was calculated as (NIR - RED) / (NIR + RED)."
  ),
  report_expr = quote({
    ndvi <- (ls$nir - ls$red) / (ls$nir + ls$red)
    
    logger$make_plot_report(
      title = "Landsat NDVI",
      description = "NDVI map derived from scaled Landsat reflectance.",
      inputs = list(landsat_scaled = ls),
      outputs = list(ndvi = ndvi),
      body = list(
        formula = "(NIR - RED) / (NIR + RED)",
        ndvi_statistics = band_stats_table(ndvi)
      )
    )
  })
)

# Non-spatial diagnostic plot: histogram.
cache$plot(
  "plot:03_landsat_bands_hist.png",
  quote({
    graphics::par(mfrow = c(2, 3), mar = c(3, 3, 2, 1))
    
    for (i in seq_len(terra::nlyr(ls))) {
      vals <- terra::values(ls[[i]], na.rm = TRUE)
      
      graphics::hist(
        vals,
        breaks = 60,
        main = names(ls)[i],
        col = "#5b9bd5",
        xlab = "Reflectance",
        ylab = ""
      )
    }
  }),
  width = 1400,
  height = 800,
  description = paste(
    "Non-spatial diagnostic histograms of scaled reflectance values.",
    "The text report contains per-band summary statistics."
  ),
  report_expr = quote({
    logger$make_plot_report(
      title = "Landsat band histograms",
      description = "Distribution of scaled reflectance values for each Landsat band.",
      inputs = list(landsat_scaled = ls),
      body = list(
        band_statistics = band_stats_table(ls)
      )
    )
  })
)

logger$stage_done("3. Landsat")

# CLC DATA

logger$stage_start("4. CLC")

clc_sf <- helpers$get_clc_vector(cache, border_sf, area_id = teryt)
clc_code_col <- "Code_18"

if (!clc_code_col %in% names(clc_sf)) {
  stop(
    "Expected CLC code column not found: ",
    clc_code_col,
    "\nAvailable columns: ",
    paste(names(clc_sf), collapse = ", ")
  )
}

clc_v <- terra::vect(clc_sf)
border_v_clc <- as_border_v(border_sf, clc_v)

logger$output("clc_vector", clc_sf)

cache$plot(
  "plot:04_clc_vector.png",
  quote({
    terra::plot(
      clc_v,
      clc_code_col,
      main = "CLC 2018 — wektor",
      plg = list(title = "Code_18", cex = 0.7)
    )
    terra::lines(border_v_clc, col = "#222222", lwd = 1.5)
  }),
  width = 1200,
  height = 1000,
  description = paste(
    "Spatial vector plot rendered with terra::plot.",
    "Shows CLC 2018 polygons clipped to the county border and colored by Code_18."
  ),
  report_expr = quote({
    clc_counts <- as.data.frame(
      table(clc_sf[[clc_code_col]]),
      stringsAsFactors = FALSE
    )
    names(clc_counts) <- c("Code_18", "polygon_count")
    clc_counts <- clc_counts[
      order(clc_counts$polygon_count, decreasing = TRUE),
    ]
    
    logger$make_plot_report(
      title = "CLC 2018 vector polygons",
      description = "CLC 2018 polygons clipped to the county border and displayed by Code_18.",
      inputs = list(clc = clc_sf, border = border_sf),
      body = list(
        code_column = clc_code_col,
        polygon_count = nrow(clc_sf),
        code_distribution = clc_counts
      )
    )
  })
)

# Non-spatial diagnostic plot.
cache$plot(
  "plot:04_clc_hist.png",
  quote({
    tab <- sort(table(clc_sf[[clc_code_col]]), decreasing = TRUE)
    
    graphics::barplot(
      tab,
      las = 2,
      cex.names = 0.6,
      col = "#e69f00",
      main = "CLC — liczba poligonów per kod",
      ylab = "n"
    )
  }),
  description = paste(
    "Non-spatial diagnostic chart of CLC polygon counts.",
    "The text report contains the same values as a table."
  ),
  report_expr = quote({
    clc_counts <- as.data.frame(
      table(clc_sf[[clc_code_col]]),
      stringsAsFactors = FALSE
    )
    names(clc_counts) <- c("Code_18", "polygon_count")
    clc_counts <- clc_counts[
      order(clc_counts$polygon_count, decreasing = TRUE),
    ]
    
    logger$make_plot_report(
      title = "CLC Code_18 histogram",
      description = "Number of CLC polygons per Code_18 value.",
      body = list(
        code_distribution = clc_counts
      )
    )
  })
)

logger$stage_done("4. CLC")