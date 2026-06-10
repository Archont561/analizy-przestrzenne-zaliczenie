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