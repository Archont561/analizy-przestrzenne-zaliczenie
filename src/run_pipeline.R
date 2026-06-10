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