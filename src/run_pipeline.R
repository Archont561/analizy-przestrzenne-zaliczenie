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

# S2GLC DATA

logger$stage_start("5. S2GLC")

s2glc_covs <- helpers$get_s2glc_wcs_coverages(cache)

logger$log(
  "s2glc_coverages",
  ns = logger$namespaces$DATA,
  msg = paste0(
    "Available coverages: ",
    paste(s2glc_covs$coverage_id, collapse = ", ")
  )
)

s2glc_r <- helpers$get_s2glc_polsa(
  cache = cache,
  border_sf = border_sf,
  year = year,
  area_id = teryt,
  mapping_dir = mapping_dir
)

raw_key <- paste0(
  "file:s2glc:wcs_raw:",
  teryt,
  ":",
  year,
  ":Land_use_classification_",
  year,
  ".tif"
)

rgb_raster <- terra::rast(cache$path(raw_key))

logger$output("s2glc_raster", s2glc_r)

n_classified <- as.numeric(
  terra::global(!is.na(s2glc_r), "sum", na.rm = TRUE)[1, 1]
)
n_total <- terra::ncell(s2glc_r)

logger$metric(
  "s2glc_valid_pixel_pct",
  round(100 * n_classified / n_total, 2),
  ns = logger$namespaces$DATA
)

cache$plot(
  "plot:05_s2glc_rgb.png",
  quote({
    terra::plotRGB(
      rgb_raster,
      r = 1,
      g = 2,
      b = 3,
      main = paste0("S2GLC ", year, " — raw RGB")
    )
  }),
  description = paste(
    "Spatial RGB raster rendered with terra::plotRGB.",
    "This is the raw POLSA WCS TIFF before RGB-to-class decoding."
  ),
  report_expr = quote({
    logger$make_plot_report(
      title = paste("S2GLC raw RGB —", year),
      description = "Raw RGB land-cover image downloaded from POLSA WCS.",
      inputs = list(rgb_raster = rgb_raster),
      body = list(
        coverage_id = paste0("Land_use_classification_", year),
        raster_info = raster_basic_table(rgb_raster, "s2glc_rgb"),
        unique_rgb_sample = rgb_unique_sample_table(rgb_raster)
      )
    )
  })
)

cache$plot(
  "plot:05_s2glc_classes.png",
  quote({
    helpers$plot_s2glc_classes(
      s2glc_r,
      mapping_dir,
      main = paste0("S2GLC ", year, " — klasy POLSA")
    )
  }),
  description = paste(
    "Spatial categorical raster plot rendered through terra::plot.",
    "RGB pixels were decoded into original S2GLC class IDs and labelled with Polish class names."
  ),
  report_expr = quote({
    logger$make_plot_report(
      title = paste("S2GLC classes —", year),
      description = "Categorical S2GLC raster decoded from POLSA RGB colors.",
      inputs = list(s2glc = s2glc_r),
      body = list(
        valid_pixels = n_classified,
        total_pixels = n_total,
        valid_pixel_pct = round(100 * n_classified / n_total, 2),
        class_distribution = s2glc_freq_table(s2glc_r, mapping_dir)
      )
    )
  })
)

# Non-spatial diagnostic plot.
cache$plot(
  "plot:05_s2glc_hist.png",
  quote({
    freq <- s2glc_freq_table(s2glc_r, mapping_dir)
    
    if (nrow(freq) == 0) {
      graphics::plot.new()
      graphics::title("S2GLC — no valid pixels")
    } else {
      graphics::barplot(
        freq$count,
        names.arg = freq$value,
        las = 2,
        cex.names = 0.8,
        col = "#009e73",
        main = paste0("S2GLC ", year, " — pikseli per klasa"),
        ylab = "n"
      )
    }
  }),
  description = paste(
    "Non-spatial diagnostic chart of S2GLC pixel counts by class.",
    "The text report contains IDs, class names, counts, and area."
  ),
  report_expr = quote({
    logger$make_plot_report(
      title = paste("S2GLC class histogram —", year),
      description = "Pixel counts and area by S2GLC class.",
      body = list(
        class_distribution = s2glc_freq_table(s2glc_r, mapping_dir)
      )
    )
  })
)

logger$stage_done("5. S2GLC")

# RECLASSIFICATION

logger$stage_start("6. Reclassifying to Unified Schema")

ref_clc <- helpers$reclassify_clc(
  cache = cache,
  border_sf = border_sf,
  landsat_raster = ls,
  mapping_dir = mapping_dir,
  area_id = teryt
)

ref_s2glc <- helpers$reclassify_s2glc(
  cache = cache,
  landsat_raster = ls,
  mapping_dir = mapping_dir,
  year = year,
  area_id = teryt
)

ref_bdot <- helpers$reclassify_bdot(
  cache = cache,
  border_sf = border_sf,
  landsat_raster = ls,
  teryt = teryt,
  mapping_dir = mapping_dir
)

logger$output("ref_clc", ref_clc)
logger$output("ref_s2glc", ref_s2glc)
logger$output("ref_bdot", ref_bdot)

cache$plot(
  "plot:06_reference_comparison",
  quote({
    graphics::par(mfrow = c(1, 3), mar = c(1, 1, 3, 1))
    
    helpers$plot_unified(ref_clc, mapping_dir, main = "Ref: CLC")
    helpers$plot_unified(ref_s2glc, mapping_dir, main = "Ref: S2GLC")
    helpers$plot_unified(ref_bdot, mapping_dir, main = "Ref: BDOT")
  }),
  width = 1800,
  height = 600,
  description = paste(
    "Spatial raster comparison rendered with terra::plot through helpers$plot_unified.",
    "All three sources were reclassified to the unified land-cover schema at Landsat resolution."
  ),
  report_expr = quote({
    comparison_table <- rbind(
      raster_freq_table(ref_clc, mapping_dir, "CLC"),
      raster_freq_table(ref_s2glc, mapping_dir, "S2GLC"),
      raster_freq_table(ref_bdot, mapping_dir, "BDOT")
    )
    
    logger$make_plot_report(
      title = "Unified reference raster comparison",
      description = "CLC, S2GLC, and BDOT references reclassified to the same unified schema.",
      outputs = list(
        ref_clc = ref_clc,
        ref_s2glc = ref_s2glc,
        ref_bdot = ref_bdot
      ),
      body = list(
        unified_schema = unified_meta,
        class_distribution = comparison_table
      )
    )
  })
)

logger$stage_done("6. Reclassifying to Unified Schema")

# TRAINING SAMPLES

logger$stage_start("7. Preparing Training Samples")

refs <- list(
  CLC = ref_clc,
  S2GLC = ref_s2glc,
  BDOT = ref_bdot
)

samples <- list()

for (src in names(refs)) {
  samples[[src]] <- helpers$prepare_training_samples(
    cache = cache,
    source_name = src,
    landsat_raster = ls,
    ref_raster = refs[[src]],
    sample_n = 50000,
    min_per_class = 200,
    seed = 123
  )
  
  logger$output(paste0("samples_", src), samples[[src]])
  
  dist <- sample_distribution_table(samples[[src]], src)
  
  cache$set(
    paste0("table:dist:", src, ".csv"),
    dist
  )
}

cache$plot(
  "plot:07_sample_distributions",
  quote({
    graphics::par(mfrow = c(1, length(samples)), mar = c(6, 4, 3, 1))
    
    for (src in names(samples)) {
      tab <- table(samples[[src]]$class_value)
      
      graphics::barplot(
        tab,
        las = 2,
        cex.names = 0.8,
        col = "#5b9bd5",
        main = paste("Samples:", src),
        ylab = "n"
      )
    }
  }),
  width = 1800,
  height = 650,
  description = paste(
    "Non-spatial diagnostic chart showing class counts in stratified training samples.",
    "The text report contains the exact sample distribution table."
  ),
  report_expr = quote({
    dist_table <- do.call(
      rbind,
      lapply(names(samples), function(src) {
        sample_distribution_table(samples[[src]], src)
      })
    )
    
    logger$make_plot_report(
      title = "Training sample distributions",
      description = "Stratified training sample counts per source and unified class.",
      body = list(
        sample_distribution = dist_table
      )
    )
  })
)

logger$stage_done("7. Preparing Training Samples")

# MODEL TRAINING

logger$stage_start("8. Training Random Forest Models")

models <- list()

for (src in names(samples)) {
  models[[src]] <- helpers$train_landcover_model(
    cache = cache,
    source_name = src,
    df = samples[[src]],
    mapping_dir = mapping_dir,
    train_prop = 0.7,
    num_trees = 300,
    seed = 123
  )
  
  logger$metric(paste0(src, "_accuracy"), round(models[[src]]$accuracy, 4))
  logger$metric(paste0(src, "_kappa"), round(models[[src]]$kappa, 4))
  logger$metric(paste0(src, "_oob_error"), round(models[[src]]$oob_error, 4))
  
  cache$plot(
    paste0("plot:08_importance_", src),
    quote({
      imp <- models[[src]]$importance
      
      if (is.null(imp) || length(imp) == 0) {
        graphics::plot.new()
        graphics::title(paste("No importance available —", src))
      } else {
        imp <- sort(imp, decreasing = TRUE)
        
        graphics::par(mar = c(4, 7, 3, 1))
        
        graphics::barplot(
          rev(imp),
          horiz = TRUE,
          las = 1,
          col = "#5b9bd5",
          main = paste("Variable importance —", src),
          xlab = "Impurity decrease"
        )
      }
    }),
    width = 1200,
    height = 700,
    description = paste(
      "Non-spatial model diagnostic chart showing random forest variable importance for",
      src,
      ". The text report contains the same values as a table."
    ),
    report_expr = quote({
      logger$make_plot_report(
        title = paste("Variable importance —", src),
        description = paste(
          "Random forest variable importance for",
          src,
          "training source."
        ),
        body = list(
          model_metrics = data.frame(
            source = src,
            accuracy = models[[src]]$accuracy,
            kappa = models[[src]]$kappa,
            oob_error = models[[src]]$oob_error,
            train_n = models[[src]]$train_n,
            test_n = models[[src]]$test_n,
            stringsAsFactors = FALSE
          ),
          variable_importance = importance_table(models[[src]], src),
          confusion_matrix = as.data.frame.matrix(models[[src]]$confusion)
        )
      )
    })
  )
}

accuracy_summary <- model_summary_table(models)

cache$set("table:model_performance.csv", accuracy_summary)
logger$output("model_performance", accuracy_summary)

for (src in names(models)) {
  cm <- as.data.frame.matrix(models[[src]]$confusion)
  
  cache$set(
    paste0("table:confusion:", src, ".csv"),
    cm
  )
}

print(accuracy_summary)

logger$stage_done("8. Training Random Forest Models")

# CLASSIFICATION

logger$stage_start("9. Classification")

results <- list()

for (src in names(models)) {
  results[[src]] <- helpers$classify_raster(
    cache = cache,
    model = models[[src]]$model,
    landsat_raster = ls,
    ref_type = src,
    mapping_dir = mapping_dir
  )
  
  logger$output(paste0("pred_", src), results[[src]])
}

cache$plot(
  "plot:09_final_classification",
  quote({
    graphics::par(mfrow = c(1, length(results)), mar = c(1, 1, 3, 1))
    
    for (src in names(results)) {
      helpers$plot_unified(
        results[[src]],
        mapping_dir,
        main = paste("Pred:", src)
      )
    }
  }),
  width = 1800,
  height = 600,
  description = paste(
    "Spatial raster classification maps rendered with terra::plot through helpers$plot_unified.",
    "Each map was predicted by a random forest trained on a different reference source."
  ),
  report_expr = quote({
    pred_table <- do.call(
      rbind,
      lapply(names(results), function(src) {
        raster_freq_table(results[[src]], mapping_dir, src)
      })
    )
    
    logger$make_plot_report(
      title = "Final RF classifications",
      description = "Predicted land-cover maps from CLC-, S2GLC-, and BDOT-trained random forests.",
      outputs = results,
      body = list(
        prediction_class_distribution = pred_table
      )
    )
  })
)

logger$stage_done("9. Classification")

# COMPARISON

logger$stage_start("10. Comparison")

# REFERENCE VS PREDICTION

for (src in names(results)) {
  cache$plot(
    paste0("plot:10_ref_vs_pred_", src),
    quote({
      graphics::par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
      
      helpers$plot_unified(
        refs[[src]],
        mapping_dir,
        main = paste("Reference:", src)
      )
      
      helpers$plot_unified(
        results[[src]],
        mapping_dir,
        main = paste("Prediction:", src)
      )
    }),
    width = 1400,
    height = 650,
    description = paste(
      "Spatial raster side-by-side comparison rendered with terra::plot.",
      "Shows reference vs prediction for source:",
      src
    ),
    report_expr = quote({
      ref_freq <- raster_freq_table(
        refs[[src]],
        mapping_dir,
        paste0(src, "_reference")
      )
      pred_freq <- raster_freq_table(
        results[[src]],
        mapping_dir,
        paste0(src, "_prediction")
      )
      area_cmp <- area_comparison_table(
        refs[[src]],
        results[[src]],
        mapping_dir,
        src
      )
      
      logger$make_plot_report(
        title = paste("Reference vs prediction —", src),
        description = paste(
          "Reference raster and predicted raster comparison for",
          src
        ),
        inputs = list(reference = refs[[src]]),
        outputs = list(prediction = results[[src]]),
        body = list(
          reference_distribution = ref_freq,
          prediction_distribution = pred_freq,
          area_comparison = area_cmp
        )
      )
    })
  )
}

# AREA COMPARISON

area_tables <- list()

for (src in names(results)) {
  area_tables[[src]] <- area_comparison_table(
    ref = refs[[src]],
    pred = results[[src]],
    mapping_dir = mapping_dir,
    source = src
  )
  
  cache$set(
    paste0("table:area_comparison:", src, ".csv"),
    area_tables[[src]]
  )
  
  logger$output(paste0("area_comparison_", src), area_tables[[src]])
}

for (src in names(area_tables)) {
  cache$plot(
    paste0("plot:10_area_comparison_", src),
    quote({
      tab <- area_tables[[src]]
      
      ylim_max <- max(
        c(tab$ref_area_km2, tab$pred_area_km2),
        na.rm = TRUE
      )
      
      mat <- rbind(
        Reference = tab$ref_area_km2,
        Prediction = tab$pred_area_km2
      )
      
      graphics::barplot(
        mat,
        beside = TRUE,
        names.arg = tab$class_code,
        las = 2,
        col = c("#999999", "#5b9bd5"),
        main = paste("Area comparison —", src),
        ylab = "Area [km²]",
        ylim = c(0, ylim_max * 1.15)
      )
      
      graphics::legend(
        "topright",
        legend = c("Reference", "Prediction"),
        fill = c("#999999", "#5b9bd5"),
        bty = "n"
      )
    }),
    width = 1200,
    height = 750,
    description = paste(
      "Non-spatial diagnostic chart comparing reference and predicted area per class for",
      src,
      ". The text report contains the full comparison table."
    ),
    report_expr = quote({
      logger$make_plot_report(
        title = paste("Area comparison —", src),
        description = "Reference vs predicted area per unified class.",
        body = list(
          area_comparison = area_tables[[src]]
        )
      )
    })
  )
}

# AGREEMENT MAP

agreement_map <- cache$cached(
  "raster:agreement_map.tif",
  quote({
    pred_stack <- do.call(c, unname(results))
    names(pred_stack) <- names(results)
    
    terra::app(
      pred_stack,
      function(x) {
        x <- x[!is.na(x)]
        
        if (length(x) == 0) {
          return(NA_integer_)
        }
        
        length(unique(x))
      }
    )
  })
)

logger$output("agreement_map", agreement_map)

cache$plot(
  "plot:10_agreement_map",
  quote({
    terra::plot(
      agreement_map,
      col = c("#2c7bb6", "#ffffbf", "#d7191c"),
      main = "Model disagreement\n1 = all agree, 3 = all different"
    )
  }),
  width = 1000,
  height = 850,
  description = paste(
    "Spatial raster plot rendered with terra::plot.",
    "Each pixel stores the number of unique predictions among the three models."
  ),
  report_expr = quote({
    freq <- as.data.frame(terra::freq(agreement_map))
    freq <- freq[!is.na(freq$value), , drop = FALSE]
    names(freq) <- c("agreement_value", "pixel_count")
    freq$meaning <- c(
      "all models agree",
      "two unique predictions",
      "three unique predictions"
    )[match(freq$agreement_value, c(1, 2, 3))]
    
    logger$make_plot_report(
      title = "Model agreement map",
      description = "Agreement is measured as number of unique class predictions per pixel.",
      outputs = list(agreement_map = agreement_map),
      body = list(
        agreement_distribution = freq
      )
    )
  })
)

# MAJORITY-VOTE ENSEMBLE

ensemble_map <- cache$cached(
  "raster:ensemble_majority_vote.tif",
  quote({
    pred_stack <- do.call(c, unname(results))
    names(pred_stack) <- names(results)
    
    terra::app(
      pred_stack,
      function(x) {
        x <- x[!is.na(x)]
        
        if (length(x) == 0) {
          return(NA_integer_)
        }
        
        tab <- table(x)
        as.integer(names(tab)[which.max(tab)])
      }
    )
  })
)

logger$output("ensemble_map", ensemble_map)

cache$plot(
  "plot:10_ensemble_majority_vote",
  quote({
    helpers$plot_unified(
      ensemble_map,
      mapping_dir,
      main = "Ensemble majority vote"
    )
  }),
  width = 1000,
  height = 850,
  description = paste(
    "Spatial raster plot rendered with terra::plot through helpers$plot_unified.",
    "Each pixel is assigned the majority class among the three model predictions."
  ),
  report_expr = quote({
    logger$make_plot_report(
      title = "Ensemble majority vote",
      description = "Per-pixel majority vote from CLC, S2GLC, and BDOT random forest predictions.",
      outputs = list(ensemble_map = ensemble_map),
      body = list(
        ensemble_class_distribution = raster_freq_table(
          ensemble_map,
          mapping_dir,
          "ensemble"
        )
      )
    )
  })
)

logger$stage_done("10. Comparison")

# DONE

logger$pipeline_done("success")