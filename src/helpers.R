# ============================================================
# 0. GDAL / PROJ FIX
# ============================================================

fix_gdal_proj <- function(logger = NULL) {
  proj_pixi_path <- ".pixi/envs/default/share/proj/proj.db"

  candidates <- c(
    file.path(getwd(), proj_pixi_path),
    file.path(dirname(getwd()), proj_pixi_path),
    Sys.getenv("PROJ_DATA"),
    Sys.getenv("PROJ_LIB")
  )

  proj_db <- candidates[file.exists(candidates)][1]

  if (!is.na(proj_db) && nzchar(proj_db)) {
    proj_dir <- dirname(proj_db)
    Sys.setenv(PROJ_LIB = proj_dir)
    Sys.setenv(PROJ_DATA = proj_dir)
    Sys.setenv(PROJ_DEBUG = "3")
    suppressMessages(sf::sf_proj_search_paths(proj_dir))

    if (!is.null(logger)) {
      logger$log(
        "proj_configured",
        ns = logger$namespaces$DATA,
        msg = paste0("PROJ configured: ", proj_dir)
      )
    }

    return(TRUE)
  }

  if (!is.null(logger)) {
    logger$log(
      "proj_not_found",
      ns = logger$namespaces$DATA,
      level = "WARN",
      msg = "PROJ database not found"
    )
  }

  FALSE
}

# ============================================================
# 1. R6 PIPELINE LOGGER
# ============================================================

PipelineLogger <- R6::R6Class(
  "PipelineLogger",
  public = list(
    log_dir = NULL,
    log_path = NULL,
    run_id = NULL,

    namespaces = list(
      STAGE = "geo.stage",
      CACHE = "geo.cache",
      DATA = "geo.data",
      MODEL = "geo.model",
      PLOT = "geo.plot"
    ),

    initialize = function(
      root = ".",
      threshold = logger::INFO,
      console = TRUE
    ) {
      private$pipeline_start_time <- Sys.time()
      self$log_dir <- file.path(root, ".cache", "logs")
      dir.create(self$log_dir, recursive = TRUE, showWarnings = FALSE)

      self$run_id <- private$make_run_id()
      self$log_path <- private$unique_log_path(self$log_dir, self$run_id)

      layout <- logger::layout_glue_generator(
        "{level} [{format(time, '%H:%M:%S')}] [{ns}] {msg}"
      )

      file_appender <- logger::appender_file(self$log_path)

      for (ns in unlist(self$namespaces, use.names = FALSE)) {
        if (console) {
          logger::log_threshold(threshold, namespace = ns, index = 1)
          logger::log_formatter(
            logger::formatter_glue_or_sprintf,
            namespace = ns,
            index = 1
          )
          logger::log_layout(layout, namespace = ns, index = 1)
          logger::log_appender(
            logger::appender_stdout,
            namespace = ns,
            index = 1
          )
        }

        logger::log_threshold(threshold, namespace = ns, index = 2)
        logger::log_formatter(
          logger::formatter_glue_or_sprintf,
          namespace = ns,
          index = 2
        )
        logger::log_layout(layout, namespace = ns, index = 2)
        logger::log_appender(file_appender, namespace = ns, index = 2)
      }

      self$log(
        "logging_initialized",
        ns = self$namespaces$STAGE,
        msg = paste0("Run ", self$run_id, " | log=", self$log_path)
      )
    },

    log = function(
      event,
      ...,
      level = "INFO",
      ns = self$namespaces$STAGE,
      msg = NULL
    ) {
      fields <- list(...)

      if (is.null(msg) || !nzchar(msg)) {
        msg <- private$make_log_message(event, fields)
      }

      fn <- private$get_log_fun(level)
      fn(msg, namespace = ns)

      invisible(self)
    },

    stage_start = function(stage, ...) {
      assign(stage, Sys.time(), envir = private$stage_times)
      self$log(
        "stage_start",
        ...,
        ns = self$namespaces$STAGE,
        msg = paste0("=== ", stage, " ===")
      )
    },

    stage_done = function(stage, ...) {
      started <- if (
        exists(stage, envir = private$stage_times, inherits = FALSE)
      ) {
        get(stage, envir = private$stage_times, inherits = FALSE)
      } else {
        NULL
      }

      elapsed <- if (!is.null(started)) {
        round(as.numeric(difftime(Sys.time(), started, units = "secs")), 2)
      } else {
        NA_real_
      }

      self$log(
        "stage_done",
        ...,
        level = "SUCCESS",
        ns = self$namespaces$STAGE,
        msg = paste0("DONE: ", stage, " | elapsed=", elapsed, "s")
      )
    },

    pipeline_done = function(status = "success") {
      elapsed <- round(
        as.numeric(difftime(
          Sys.time(),
          private$pipeline_start_time,
          units = "secs"
        )),
        2
      )

      lvl <- if (identical(status, "success")) "SUCCESS" else "ERROR"

      self$log(
        "pipeline_done",
        level = lvl,
        ns = self$namespaces$STAGE,
        msg = paste0(
          "Pipeline finished | status=",
          status,
          " | elapsed=",
          elapsed,
          "s"
        )
      )
    },

    input = function(name, object, ns = self$namespaces$DATA) {
      self$log(
        "input",
        ns = ns,
        msg = paste0("INPUT  ", name, " = ", self$summary(object))
      )
    },

    output = function(name, object, ns = self$namespaces$DATA) {
      self$log(
        "output",
        ns = ns,
        msg = paste0("OUTPUT ", name, " = ", self$summary(object))
      )
    },

    metric = function(name, value, ns = self$namespaces$MODEL) {
      self$log(
        "metric",
        ns = ns,
        msg = paste0("METRIC ", name, " = ", self$summary(value))
      )
    },

    diagnostics = function(name, object, ns = self$namespaces$DATA) {
      diag <- self$diagnose_object(object)

      lines <- vapply(
        names(diag),
        function(nm) paste0(nm, "=", self$summary(diag[[nm]])),
        character(1)
      )

      self$log(
        "diagnostics",
        ns = ns,
        msg = paste0("DIAG ", name, " | ", paste(lines, collapse = ", "))
      )

      invisible(diag)
    },

    summary = function(x, max_items = 6, max_chars = 140) {
      private$short_value(x, max_items, max_chars)
    },

    diagnose_object = function(x) {
      if (is.null(x)) {
        return(list(type = "NULL"))
      }
      if (inherits(x, "SpatRaster")) {
        return(private$diag_raster(x))
      }
      if (inherits(x, "SpatVector")) {
        return(private$diag_spatvector(x))
      }
      if (inherits(x, "sf")) {
        return(private$diag_sf(x))
      }
      if (is.data.frame(x)) {
        return(private$diag_dataframe(x))
      }
      if (inherits(x, "ranger")) {
        return(private$diag_ranger(x))
      }
      if (is.character(x) && length(x) == 1 && file.exists(x)) {
        return(private$diag_path(x))
      }

      list(
        type = paste(class(x), collapse = ","),
        preview = private$short_value(x)
      )
    },

    make_plot_report = function(
      title,
      description,
      inputs = NULL,
      outputs = NULL,
      body = NULL
    ) {
      list(
        title = title,
        description = description,
        inputs = inputs,
        outputs = outputs,
        body = body
      )
    },

    write_report_from_object = function(
      path,
      report,
      default_title = "Report"
    ) {
      if (is.null(report)) {
        private$write_text_report(path = path, title = default_title)
        return(invisible(path))
      }

      if (is.character(report)) {
        private$write_text_report(
          path = path,
          title = default_title,
          body = report
        )
        return(invisible(path))
      }

      if (is.list(report)) {
        private$write_text_report(
          path = path,
          title = report$title %||% default_title,
          description = report$description,
          inputs = report$inputs,
          outputs = report$outputs,
          body = report$body
        )
        return(invisible(path))
      }

      private$write_text_report(
        path = path,
        title = default_title,
        body = utils::capture.output(str(report))
      )

      invisible(path)
    }
  ),

  private = list(
    stage_times = new.env(parent = emptyenv()),
    pipeline_start_time = NULL,

    make_run_id = function() {
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    },

    unique_log_path = function(log_dir, run_id) {
      path <- file.path(log_dir, paste0(run_id, ".log"))

      if (!file.exists(path)) {
        return(path)
      }

      i <- 2L
      repeat {
        candidate <- file.path(log_dir, paste0(run_id, "_", i, ".log"))
        if (!file.exists(candidate)) {
          return(candidate)
        }
        i <- i + 1L
      }
    },

    get_log_fun = function(level) {
      switch(
        toupper(level),
        TRACE = logger::log_trace,
        DEBUG = logger::log_debug,
        INFO = logger::log_info,
        SUCCESS = logger::log_success,
        WARN = logger::log_warn,
        ERROR = logger::log_error,
        FATAL = logger::log_fatal,
        logger::log_info
      )
    },

    compact_fields = function(fields) {
      if (length(fields) == 0) {
        return(fields)
      }

      keep <- !vapply(
        fields,
        function(v) is.null(v) || (length(v) == 1 && is.na(v)),
        logical(1)
      )

      fields[keep]
    },

    make_log_message = function(event, fields) {
      fields <- private$compact_fields(fields)

      if (length(fields) == 0) {
        return(event)
      }

      nms <- names(fields)
      if (is.null(nms)) {
        nms <- paste0("field_", seq_along(fields))
      }

      parts <- vapply(
        seq_along(fields),
        function(i) paste0(nms[i], "=", private$short_value(fields[[i]])),
        character(1)
      )

      paste0(event, " | ", paste(parts, collapse = ", "))
    },

    short_value = function(x, max_items = 6, max_chars = 140) {
      if (is.null(x)) {
        return("NULL")
      }

      if (inherits(x, "SpatRaster")) {
        return(sprintf(
          "SpatRaster[%dlyr, %dx%d, res=%.2f]",
          terra::nlyr(x),
          terra::nrow(x),
          terra::ncol(x),
          terra::res(x)[1]
        ))
      }

      if (inherits(x, "SpatVector")) {
        return(sprintf("SpatVector[%d feats]", nrow(x)))
      }

      if (inherits(x, "sf")) {
        return(sprintf("sf[%d feats]", nrow(x)))
      }

      if (inherits(x, "ranger")) {
        return(sprintf(
          "ranger[%s trees, OOB=%s]",
          as.character(x$num.trees %||% NA),
          as.character(round(x$prediction.error %||% NA_real_, 4))
        ))
      }

      if (is.data.frame(x)) {
        return(sprintf("df[%dx%d]", nrow(x), ncol(x)))
      }

      if (is.list(x) && !is.data.frame(x)) {
        nms <- names(x)
        if (is.null(nms)) {
          nms <- paste0("item_", seq_along(x))
        }

        parts <- vapply(
          seq_along(utils::head(x, max_items)),
          function(i) {
            paste0(nms[i], "=", private$short_value(x[[i]], max_items = 3))
          },
          character(1)
        )

        txt <- paste(parts, collapse = ", ")
      } else {
        txt <- paste(utils::head(as.character(x), max_items), collapse = ", ")
      }

      if (nchar(txt) > max_chars) {
        txt <- paste0(substr(txt, 1, max_chars - 3), "...")
      }

      txt
    },

    diag_raster = function(r) {
      if (!inherits(r, "SpatRaster")) {
        r <- terra::rast(r)
      }

      non_na <- tryCatch(
        as.numeric(terra::global(!is.na(r[[1]]), "sum", na.rm = TRUE)[1, 1]),
        error = function(e) NA_real_
      )

      list(
        type = "SpatRaster",
        nlyr = terra::nlyr(r),
        nrow = terra::nrow(r),
        ncol = terra::ncol(r),
        ncell = terra::ncell(r),
        names = names(r),
        res = as.numeric(terra::res(r)),
        crs = terra::crs(r),
        extent = as.vector(terra::ext(r)),
        non_na_layer1 = non_na
      )
    },

    diag_spatvector = function(v) {
      list(
        type = "SpatVector",
        nfeat = nrow(v),
        names = names(v),
        geomtype = as.character(terra::geomtype(v)),
        crs = terra::crs(v),
        extent = as.vector(terra::ext(v))
      )
    },

    diag_sf = function(x) {
      list(
        type = "sf",
        nfeat = nrow(x),
        names = names(x),
        geomtype = unique(as.character(sf::st_geometry_type(
          x,
          by_geometry = TRUE
        ))),
        crs = sf::st_crs(x)$input %||% as.character(sf::st_crs(x)$epsg),
        bbox = as.numeric(sf::st_bbox(x))
      )
    },

    diag_dataframe = function(df) {
      out <- list(
        type = "data.frame",
        nrow = nrow(df),
        ncol = ncol(df),
        names = names(df),
        na_total = sum(is.na(df))
      )

      if ("class_value" %in% names(df)) {
        out$class_distribution <- as.list(sort(
          table(df$class_value),
          decreasing = TRUE
        ))
      }

      out
    },

    diag_ranger = function(model) {
      list(
        type = "ranger",
        num_trees = model$num.trees %||% NA_integer_,
        mtry = model$mtry %||% NA_integer_,
        prediction_error = model$prediction.error %||% NA_real_,
        importance_mode = model$importance.mode %||% NA_character_
      )
    },

    diag_path = function(path) {
      exists <- file.exists(path)

      list(
        type = "path",
        path = normalizePath(path, winslash = "/", mustWork = FALSE),
        exists = exists,
        size_bytes = if (exists) file.info(path)$size else NA_real_
      )
    },

    format_report_lines = function(x, indent = 2) {
      pad <- paste(rep(" ", indent), collapse = "")

      if (is.null(x) || length(x) == 0) {
        return(character(0))
      }

      if (is.character(x)) {
        return(paste0(pad, x))
      }

      if (is.data.frame(x)) {
        return(paste0(pad, utils::capture.output(print(utils::head(x, 30)))))
      }

      if (!is.list(x)) {
        return(paste0(pad, as.character(x)))
      }

      nms <- names(x)
      if (is.null(nms)) {
        nms <- paste0("item_", seq_along(x))
      }

      out <- character(0)

      for (i in seq_along(x)) {
        nm <- nms[i]
        val <- x[[i]]

        if (is.list(val) && !is.data.frame(val)) {
          out <- c(
            out,
            paste0(pad, nm, ":"),
            private$format_report_lines(val, indent + 2)
          )
        } else if (is.data.frame(val)) {
          out <- c(
            out,
            paste0(pad, nm, ":"),
            paste0(
              pad,
              "  ",
              utils::capture.output(print(utils::head(val, 30)))
            )
          )
        } else {
          out <- c(
            paste0(out, ""),
            paste0(pad, nm, ": ", private$short_value(val))
          )
        }
      }

      out
    },

    write_text_report = function(
      path,
      title,
      description = NULL,
      inputs = NULL,
      outputs = NULL,
      body = NULL
    ) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

      sep <- paste(rep("-", 60), collapse = "")

      lines <- c(
        title,
        paste(rep("=", nchar(title)), collapse = ""),
        "",
        paste("Created:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        paste("Run ID:", self$run_id),
        paste("Log file:", self$log_path)
      )

      if (!is.null(description) && nzchar(description)) {
        lines <- c(lines, "", "Description:", description)
      }

      if (!is.null(inputs) && length(inputs) > 0) {
        lines <- c(lines, "", sep, "Inputs:", sep)

        for (nm in names(inputs)) {
          lines <- c(
            lines,
            paste0("  ", nm, ":"),
            private$format_report_lines(self$diagnose_object(inputs[[nm]]), 4)
          )
        }
      }

      if (!is.null(outputs) && length(outputs) > 0) {
        lines <- c(lines, "", sep, "Outputs:", sep)

        for (nm in names(outputs)) {
          lines <- c(
            lines,
            paste0("  ", nm, ":"),
            private$format_report_lines(self$diagnose_object(outputs[[nm]]), 4)
          )
        }
      }

      if (!is.null(body) && length(body) > 0) {
        lines <- c(lines, "", sep, "Details:", sep)
        lines <- c(lines, private$format_report_lines(body, 2))
      }

      writeLines(enc2utf8(lines), path)
      invisible(path)
    }
  )
)

# ============================================================
# 2. R6 CACHE MANAGER
# ============================================================

CacheManager <- R6::R6Class(
  "CacheManager",
  public = list(
    root = NULL,
    dir = NULL,
    logger = NULL,

    initialize = function(root = ".", logger = NULL) {
      self$root <- root
      self$dir <- file.path(root, ".cache")
      self$logger <- logger
      dir.create(self$dir, recursive = TRUE, showWarnings = FALSE)
    },

    path = function(key) {
      file.path(self$dir, gsub(":", "/", key))
    },

    exists = function(key) {
      file.exists(self$path(key))
    },

    ensure_dir = function(key) {
      dir.create(
        dirname(self$path(key)),
        recursive = TRUE,
        showWarnings = FALSE
      )
    },

    get = function(key) {
      path <- self$path(key)

      if (!file.exists(path)) {
        private$log("cache_miss", level = "DEBUG", msg = paste0("MISS ", key))
        return(NULL)
      }

      ext <- tolower(tools::file_ext(path))

      out <- if (ext %in% c("tif", "tiff")) {
        terra::rast(path)
      } else if (ext == "csv") {
        utils::read.csv(path, stringsAsFactors = FALSE)
      } else if (ext %in% c("gpkg", "png")) {
        path
      } else {
        readRDS(path)
      }

      private$log(
        "cache_hit",
        level = "DEBUG",
        msg = paste0("GET ", key, " = ", private$summary(out))
      )

      out
    },

    layers = function(key) {
      path <- self$path(key)

      if (!file.exists(path)) {
        private$log(
          "layers_missing",
          level = "WARN",
          msg = paste0("Layer listing failed; missing file: ", key)
        )
        return(NULL)
      }

      info <- sf::st_layers(path)

      out <- data.frame(
        name = info$name,
        features = as.numeric(info$features),
        geomtype = as.character(info$geomtype),
        stringsAsFactors = FALSE
      )

      private$log(
        "layers_read",
        level = "DEBUG",
        msg = paste0("LAYERS ", key, " = ", nrow(out))
      )

      out
    },

    get_layer = function(key, layer) {
      path <- self$path(key)

      if (!file.exists(path)) {
        private$log(
          "layer_missing_file",
          level = "WARN",
          msg = paste0("Layer read failed; missing file: ", key)
        )
        return(NULL)
      }

      private$log(
        "layer_read",
        level = "DEBUG",
        msg = paste0("READ LAYER ", key, " / ", layer)
      )

      terra::vect(path, layer = layer)
    },

    get_layer_sf = function(key, layer) {
      path <- self$path(key)

      if (!file.exists(path)) {
        private$log(
          "layer_missing_file",
          level = "WARN",
          msg = paste0("SF layer read failed; missing file: ", key)
        )
        return(NULL)
      }

      private$log(
        "layer_read_sf",
        level = "DEBUG",
        msg = paste0("READ SF LAYER ", key, " / ", layer)
      )

      sf::st_make_valid(
        sf::st_read(path, layer = layer, quiet = TRUE, stringsAsFactors = FALSE)
      )
    },

    set = function(key, value) {
      path <- self$path(key)
      self$ensure_dir(key)
      ext <- tolower(tools::file_ext(path))

      private$log(
        "cache_set",
        msg = paste0("SET ", key, " (", ext, ") = ", private$summary(value))
      )

      if (ext == "rds") {
        saveRDS(value, path)
      } else if (ext == "csv") {
        if (!is.data.frame(value)) {
          value <- as.data.frame(value)
        }
        utils::write.csv(value, path, row.names = FALSE)
      } else if (inherits(value, "SpatRaster")) {
        terra::writeRaster(value, path, overwrite = TRUE)
      } else if (inherits(value, "SpatVector")) {
        terra::writeVector(value, path, filetype = "GPKG", overwrite = TRUE)
      } else if (
        is.list(value) &&
          length(value) > 0 &&
          all(vapply(value, inherits, logical(1), "SpatVector"))
      ) {
        if (file.exists(path)) {
          file.remove(path)
        }

        nms <- names(value)
        if (is.null(nms) || any(!nzchar(nms))) {
          nms <- paste0("layer_", seq_along(value))
        }

        terra::writeVector(
          value[[1]],
          path,
          filetype = "GPKG",
          layer = nms[1],
          overwrite = TRUE
        )

        if (length(value) > 1) {
          for (i in 2:length(value)) {
            terra::writeVector(
              value[[i]],
              path,
              filetype = "GPKG",
              layer = nms[i],
              insert = TRUE
            )
          }
        }
      } else if (inherits(value, "sf")) {
        write_gpkg(value, path, layer = "data")
      } else {
        saveRDS(value, path)
      }

      private$log(
        "cache_written",
        level = "DEBUG",
        msg = paste0(
          "WRITTEN ",
          key,
          " -> ",
          path,
          " (",
          file.info(path)$size,
          " bytes)"
        )
      )

      invisible(path)
    },

    cached = function(key, expr) {
      if (self$exists(key)) {
        private$log(
          "cache_reuse",
          msg = paste0("REUSE ", key)
        )
        return(self$get(key))
      }

      private$log(
        "cache_compute",
        msg = paste0("COMPUTE ", key)
      )

      started <- Sys.time()

      val <- withCallingHandlers(
        tryCatch(
          eval(expr, parent.frame()),
          error = function(e) {
            private$log(
              "cache_error",
              level = "ERROR",
              msg = paste0("ERROR ", key, " | ", conditionMessage(e))
            )
            stop(e)
          }
        ),
        warning = function(w) {
          private$log(
            "cache_warning",
            level = "WARN",
            msg = paste0("WARN ", key, " | ", conditionMessage(w))
          )
          invokeRestart("muffleWarning")
        }
      )

      self$set(key, val)

      elapsed <- round(
        as.numeric(difftime(Sys.time(), started, units = "secs")),
        2
      )

      private$log(
        "cache_done",
        msg = paste0("DONE ", key, " (", elapsed, "s) = ", private$summary(val))
      )

      val
    },

    plot = function(
      key,
      expr,
      width = 1200,
      height = 900,
      res = 130,
      description = NULL,
      report_expr = NULL
    ) {
      if (tolower(tools::file_ext(key)) != "png") {
        key <- paste0(key, ".png")
      }

      path <- self$path(key)
      report_path <- sub("\\.png$", ".txt", path)

      if (file.exists(path)) {
        private$log(
          "plot_reuse",
          ns = private$plot_ns(),
          msg = paste0("REUSE ", key)
        )

        if (!file.exists(report_path) && !is.null(self$logger)) {
          self$logger$write_report_from_object(
            path = report_path,
            report = self$logger$make_plot_report(
              title = basename(path),
              description = description %||%
                "Cached plot reused; original report was missing."
            ),
            default_title = basename(path)
          )
        }

        return(invisible(path))
      }

      self$ensure_dir(key)

      private$log(
        "plot_render",
        ns = private$plot_ns(),
        msg = paste0(
          "RENDER ",
          key,
          " (",
          width,
          "x",
          height,
          ", res=",
          res,
          ")"
        )
      )

      started <- Sys.time()

      grDevices::png(path, width = width, height = height, res = res)
      on.exit(grDevices::dev.off(), add = TRUE)

      result <- withCallingHandlers(
        tryCatch(
          eval(expr, parent.frame()),
          error = function(e) {
            private$log(
              "plot_error",
              ns = private$plot_ns(),
              level = "ERROR",
              msg = paste0("ERROR ", key, " | ", conditionMessage(e))
            )
            stop(e)
          }
        ),
        warning = function(w) {
          private$log(
            "plot_warning",
            ns = private$plot_ns(),
            level = "WARN",
            msg = paste0("WARN ", key, " | ", conditionMessage(w))
          )
          invokeRestart("muffleWarning")
        }
      )

      if (inherits(result, c("ggplot", "trellis", "patchwork"))) {
        print(result)
      }

      if (!is.null(self$logger)) {
        report <- NULL

        if (!is.null(report_expr)) {
          report <- tryCatch(
            eval(report_expr, parent.frame()),
            error = function(e) {
              private$log(
                "plot_report_error",
                ns = private$plot_ns(),
                level = "WARN",
                msg = paste0(
                  "Report generation failed for ",
                  key,
                  ": ",
                  conditionMessage(e)
                )
              )
              NULL
            }
          )
        }

        self$logger$write_report_from_object(
          path = report_path,
          report = report,
          default_title = basename(path)
        )
      }

      elapsed <- round(
        as.numeric(difftime(Sys.time(), started, units = "secs")),
        2
      )

      private$log(
        "plot_done",
        ns = private$plot_ns(),
        msg = paste0(
          "DONE ",
          key,
          " (",
          elapsed,
          "s) | png=",
          file.info(path)$size,
          "B | txt=",
          report_path
        )
      )

      invisible(path)
    }
  ),

  private = list(
    cache_ns = function() {
      if (!is.null(self$logger)) {
        return(self$logger$namespaces$CACHE)
      }
      "geo.cache"
    },

    plot_ns = function() {
      if (!is.null(self$logger)) {
        return(self$logger$namespaces$PLOT)
      }
      "geo.plot"
    },

    log = function(
      event,
      level = "INFO",
      ns = private$cache_ns(),
      msg = NULL,
      ...
    ) {
      if (!is.null(self$logger)) {
        self$logger$log(event, ..., level = level, ns = ns, msg = msg)
      }
      invisible(NULL)
    },

    summary = function(x) {
      if (!is.null(self$logger)) {
        return(self$logger$summary(x))
      }

      paste(class(x), collapse = ",")
    }
  )
)

cache_log <- function(
  cache,
  event,
  ...,
  level = "INFO",
  ns = NULL,
  msg = NULL
) {
  if (!is.null(cache) && !is.null(cache$logger)) {
    if (is.null(ns)) {
      ns <- cache$logger$namespaces$DATA
    }
    cache$logger$log(event, ..., level = level, ns = ns, msg = msg)
  }
  invisible(NULL)
}

# ============================================================
# 3. JSON HELPERS
# ============================================================

read_json <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

# ============================================================
# 4. UNIFIED LANDCOVER META
# ============================================================

get_unified_meta <- function(mapping_dir = "data") {
  obj <- read_json(file.path(mapping_dir, "unified_lc_schema.json"))
  df <- as.data.frame(obj$classes, stringsAsFactors = FALSE)
  df$value <- seq_len(nrow(df))

  palette <- c(
    ART = "#E60000",
    AGR = "#FFD400",
    GRA = "#FFAA00",
    FOR = "#005C00",
    SHR = "#A0D67A",
    BAR = "#A6A6A6",
    WET = "#7AB6F5",
    WAT = "#0046FF"
  )

  df$color <- unname(palette[df$code])
  df
}

# ============================================================
# 5. SMALL GEOSPATIAL HELPERS
# ============================================================

bbox_wgs84 <- function(sf_obj) {
  b <- sf::st_bbox(sf::st_transform(sf_obj, 4326))
  c(b["xmin"], b["ymin"], b["xmax"], b["ymax"])
}

write_gpkg <- function(sf_obj, path, layer = "data") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(path)) {
    sf::st_write(sf_obj, path, layer = layer, delete_layer = TRUE, quiet = TRUE)
  } else {
    sf::st_write(sf_obj, path, layer = layer, quiet = TRUE)
  }

  invisible(path)
}

scale_landsat <- function(r, clamp = TRUE, mask_fill = TRUE) {
  if (mask_fill) {
    r <- terra::ifel(r == 0, NA, r)
  }

  out <- r * 0.0000275 - 0.2

  if (clamp) {
    out <- terra::clamp(out, 0, 1, values = TRUE)
  }

  out
}

LANDSAT_FEATURES <- c(
  "blue",
  "green",
  "red",
  "nir",
  "swir16",
  "ndvi",
  "ndwi",
  "nbr"
)

add_indices <- function(ls) {
  if (!inherits(ls, "SpatRaster")) {
    stop("add_indices() expects a SpatRaster")
  }

  if (terra::nlyr(ls) < 5) {
    stop("add_indices() requires at least 5 Landsat bands")
  }

  ls <- ls[[1:5]]
  names(ls) <- c("blue", "green", "red", "nir", "swir16")

  ndvi <- (ls[["nir"]] - ls[["red"]]) / (ls[["nir"]] + ls[["red"]])
  ndwi <- (ls[["green"]] - ls[["nir"]]) / (ls[["green"]] + ls[["nir"]])
  nbr <- (ls[["nir"]] - ls[["swir16"]]) / (ls[["nir"]] + ls[["swir16"]])

  names(ndvi) <- "ndvi"
  names(ndwi) <- "ndwi"
  names(nbr) <- "nbr"

  out <- c(ls, ndvi, ndwi, nbr)
  names(out) <- LANDSAT_FEATURES
  out
}

crop_mask <- function(raster, sf_border) {
  v <- terra::project(terra::vect(sf_border), terra::crs(raster))
  terra::mask(terra::crop(raster, v), v)
}

# ============================================================
# 6. CROSSWALKS
# ============================================================

expand_range_key <- function(key) {
  if (grepl("-", key)) {
    p <- as.integer(strsplit(key, "-")[[1]])
    seq(p[1], p[2])
  } else {
    as.integer(key)
  }
}

load_clc_crosswalk <- function(mapping_dir) {
  meta <- get_unified_meta(mapping_dir)
  raw <- read_json(file.path(mapping_dir, "clc_to_unified.json"))

  do.call(
    rbind,
    lapply(names(raw), function(k) {
      data.frame(
        from = expand_range_key(k),
        to = meta$value[match(raw[[k]], meta$code)]
      )
    })
  )
}

load_s2glc_crosswalk <- function(mapping_dir) {
  unified <- get_unified_meta(mapping_dir)
  s2glc_meta <- get_s2glc_meta(mapping_dir)
  raw_map <- read_json(file.path(mapping_dir, "s2glc_to_unified.json"))

  to_unified_codes <- unname(unlist(raw_map[s2glc_meta$class_name]))

  data.frame(
    from = s2glc_meta$class_id,
    to = unified$value[match(to_unified_codes, unified$code)]
  )
}

load_bdot_crosswalk <- function(mapping_dir) {
  meta <- get_unified_meta(mapping_dir)
  raw <- read_json(file.path(mapping_dir, "bdot10k_to_unified.json"))

  data.frame(
    source_key = names(raw),
    unified_value = meta$value[match(unname(raw), meta$code)],
    stringsAsFactors = FALSE
  )
}

# ============================================================
# 7. BDOT DOWNLOAD
# ============================================================

download_bdot10k_index <- function() {
  wfs <- "https://mapy.geoportal.gov.pl/wss/service/PZGIK/BDOT/WFS/PobieranieBDOT10k"

  req <- httr2::request(wfs) |>
    httr2::req_url_query(
      service = "WFS",
      version = "2.0.0",
      request = "GetFeature",
      typename = "ms:BDOT10k_powiaty"
    )

  resp <- httr2::req_perform(req)
  xml <- xml2::read_xml(httr2::resp_body_string(resp))

  data.frame(
    TERYT = xml2::xml_text(xml2::xml_find_all(xml, ".//ms:TERYT")),
    URL_GML = xml2::xml_text(xml2::xml_find_all(xml, ".//ms:URL_GML")),
    stringsAsFactors = FALSE
  )
}

get_bdot10k_url <- function(index, teryt) {
  url <- index$URL_GML[index$TERYT == teryt]

  if (!length(url)) {
    stop("No TERYT: ", teryt)
  }

  url[[1]]
}

download_bdot10k_zip <- function(url, cache, teryt) {
  key <- paste0("file:bdot10k:", teryt, ".zip")

  if (cache$exists(key)) {
    cache_log(
      cache,
      "bdot_zip_reuse",
      msg = paste0("Reuse BDOT10k ZIP for TERYT ", teryt)
    )
    return(cache$path(key))
  }

  out <- cache$path(key)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

  cache_log(
    cache,
    "bdot_zip_download",
    msg = paste0("Downloading BDOT10k ZIP for TERYT ", teryt)
  )

  utils::download.file(url, out, mode = "wb", quiet = TRUE, timeout = 300)

  cache_log(
    cache,
    "bdot_zip_downloaded",
    msg = paste0("Downloaded BDOT10k ZIP: ", out)
  )

  out
}

bdot_zip_to_gpkg <- function(cache, zip_path, teryt) {
  key <- paste0("geo:bdot10k:", teryt, ".gpkg")

  if (cache$exists(key)) {
    cache_log(
      cache,
      "bdot_gpkg_reuse",
      msg = paste0("Reuse BDOT10k GPKG for TERYT ", teryt)
    )
    return(key)
  }

  zip_path <- normalizePath(zip_path, mustWork = TRUE)

  tmp <- ".tmp"
  unlink(tmp, recursive = TRUE, force = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  cache_log(
    cache,
    "bdot_unzip_start",
    msg = paste0("Extracting BDOT10k ZIP: ", zip_path)
  )

  utils::unzip(zip_path, exdir = tmp)

  files <- list.files(tmp, "\\.xml$", full.names = TRUE, recursive = TRUE)

  if (length(files) == 0) {
    stop("No XML files extracted — ZIP likely truncated/corrupt.")
  }

  vects <- list()

  for (f in files) {
    sf_layer <- tryCatch(
      sf::st_read(f, quiet = TRUE),
      error = function(e) NULL
    )

    if (is.null(sf_layer) || nrow(sf_layer) == 0) {
      next
    }

    nm <- tools::file_path_sans_ext(basename(f))
    vects[[nm]] <- terra::vect(sf_layer)
  }

  if (length(vects) == 0) {
    stop("All extracted BDOT layers were empty.")
  }

  cache_log(
    cache,
    "bdot_gpkg_build",
    msg = paste0("Writing BDOT10k GPKG with ", length(vects), " layers")
  )

  cache$set(key, vects)
  key
}

# ============================================================
# 8. LANDSAT API + RAW BAND DOWNLOAD
# ============================================================

# ============================================================
# 8. LANDSAT API + RAW BAND DOWNLOAD
# ============================================================

# ------------------------------------------------------------
# 8.1 Utilities
# ------------------------------------------------------------

safe_filename <- function(x) {
  gsub("[^A-Za-z0-9_\\-\\.]+", "_", x)
}

stac_feature_prop <- function(feature, name, default = NA) {
  val <- feature$properties[[name]]
  if (is.null(val)) default else val
}

subset_stac_items <- function(items, idx) {
  items$features <- items$features[idx]

  if (!is.null(items$context$returned)) {
    items$context$returned <- length(items$features)
  }

  items
}

# ------------------------------------------------------------
# 8.2 STAC items → sf table (geometry + cloud + platform)
# ------------------------------------------------------------

stac_items_to_sf <- function(items) {
  if (length(items$features) == 0) {
    stop("No STAC features to convert to sf.")
  }

  fc <- list(
    type = "FeatureCollection",
    features = lapply(items$features, function(f) {
      list(
        type = "Feature",
        properties = list(dummy = 1),
        geometry = f$geometry
      )
    })
  )

  txt <- jsonlite::toJSON(fc, auto_unbox = TRUE, null = "null")
  geom_sf <- sf::st_read(txt, quiet = TRUE)

  ids <- vapply(
    items$features,
    function(f) f$id %||% NA_character_,
    character(1)
  )

  datetimes <- vapply(
    items$features,
    function(f) stac_feature_prop(f, "datetime", NA_character_),
    character(1)
  )

  cloud_cover <- vapply(
    items$features,
    function(f) {
      cc <- stac_feature_prop(f, "eo:cloud_cover", NA_real_)
      as.numeric(cc)
    },
    numeric(1)
  )

  platform <- vapply(
    items$features,
    function(f) stac_feature_prop(f, "platform", NA_character_),
    character(1)
  )

  sf::st_sf(
    id = ids,
    datetime = datetimes,
    date = substr(datetimes, 1, 10),
    cloud_cover = cloud_cover,
    platform = platform,
    geometry = sf::st_geometry(geom_sf),
    crs = 4326
  )
}

# ------------------------------------------------------------
# 8.3 Coverage analysis per date
# ------------------------------------------------------------

landsat_date_coverage_table <- function(items_sf, border_sf) {
  aoi <- sf::st_make_valid(sf::st_transform(border_sf, 2180))
  aoi_geom <- sf::st_union(sf::st_geometry(aoi))
  aoi_area <- as.numeric(sf::st_area(aoi_geom))

  footprints <- sf::st_make_valid(sf::st_transform(items_sf, 2180))
  dates <- sort(unique(footprints$date))

  out <- lapply(dates, function(d) {
    idx <- which(footprints$date == d)

    union_geom <- sf::st_union(sf::st_geometry(footprints[idx, ]))

    inter <- suppressWarnings(
      sf::st_intersection(
        sf::st_sf(geometry = union_geom, crs = sf::st_crs(footprints)),
        sf::st_sf(geometry = aoi_geom, crs = sf::st_crs(aoi))
      )
    )

    inter_area <- if (nrow(inter) == 0) {
      0
    } else {
      as.numeric(sum(sf::st_area(inter)))
    }

    data.frame(
      date = d,
      n_scenes = length(idx),
      coverage = inter_area / aoi_area,
      mean_cloud = mean(footprints$cloud_cover[idx], na.rm = TRUE),
      max_cloud = max(footprints$cloud_cover[idx], na.rm = TRUE),
      platforms = paste(
        sort(unique(footprints$platform[idx])),
        collapse = ", "
      ),
      ids = paste(footprints$id[idx], collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

# ------------------------------------------------------------
# 8.4 Platform + cloud + coverage filter
# ------------------------------------------------------------

filter_landsat_by_platform <- function(
  items,
  items_sf,
  allowed_platforms,
  cache = NULL
) {
  if (is.null(allowed_platforms) || length(allowed_platforms) == 0) {
    return(list(items = items, items_sf = items_sf))
  }

  keep <- items_sf$platform %in% allowed_platforms

  cache_log(
    cache,
    "landsat_platform_filter",
    msg = paste0(
      "Platform filter | allowed=",
      paste(allowed_platforms, collapse = ","),
      " | kept ",
      sum(keep),
      "/",
      nrow(items_sf)
    )
  )

  list(
    items = subset_stac_items(items, keep),
    items_sf = items_sf[keep, ]
  )
}

try_landsat_threshold <- function(
  items,
  items_sf,
  border_sf,
  threshold,
  min_coverage,
  cache = NULL
) {
  keep_cloud <- !is.na(items_sf$cloud_cover) & items_sf$cloud_cover < threshold

  if (!any(keep_cloud)) {
    return(NULL)
  }

  items_cloud <- subset_stac_items(items, keep_cloud)
  sf_cloud <- items_sf[keep_cloud, ]

  cov_table <- landsat_date_coverage_table(sf_cloud, border_sf)
  cov_table <- cov_table[order(-cov_table$coverage, cov_table$mean_cloud), ]

  ok <- cov_table$coverage >= min_coverage

  if (!any(ok)) {
    return(list(
      success = FALSE,
      threshold = threshold,
      table = cov_table
    ))
  }

  chosen <- cov_table[which(ok)[1], ]
  keep_date <- sf_cloud$date == chosen$date

  list(
    success = TRUE,
    threshold = threshold,
    date = chosen$date,
    table = cov_table,
    items = subset_stac_items(items_cloud, keep_date),
    items_sf = sf_cloud[keep_date, ]
  )
}

attach_landsat_metadata <- function(
  items,
  result,
  allowed_platforms,
  threshold,
  min_coverage
) {
  criteria_id <- safe_filename(
    paste0(
      "platform_",
      paste(allowed_platforms, collapse = "-"),
      "_cc",
      threshold,
      "_cov",
      min_coverage
    )
  )

  result$items$pipeline_metadata <- list(
    criteria_id = criteria_id,
    selected_date = result$date,
    n_scenes = nrow(result$items_sf),
    scene_ids = result$items_sf$id,
    items_sf = result$items_sf,
    coverage_table = result$table
  )

  result$items
}

filter_landsat_by_cloud_and_coverage <- function(
  items,
  border_sf,
  strict_cloud = 2,
  relaxed_cloud = 10,
  min_coverage = 0.999,
  allowed_platforms = c("landsat-8", "landsat-9"),
  cache = NULL
) {
  if (length(items$features) == 0) {
    stop("No Landsat scenes found before filtering.")
  }

  items_sf <- stac_items_to_sf(items)

  filtered <- filter_landsat_by_platform(
    items = items,
    items_sf = items_sf,
    allowed_platforms = allowed_platforms,
    cache = cache
  )

  items <- filtered$items
  items_sf <- filtered$items_sf

  if (nrow(items_sf) == 0) {
    stop(
      "No Landsat scenes left after platform filtering. ",
      "Allowed platforms: ",
      paste(allowed_platforms, collapse = ", ")
    )
  }

  # Tier 1: strict
  strict <- try_landsat_threshold(
    items = items,
    items_sf = items_sf,
    border_sf = border_sf,
    threshold = strict_cloud,
    min_coverage = min_coverage,
    cache = cache
  )

  if (!is.null(strict) && isTRUE(strict$success)) {
    cache_log(
      cache,
      "landsat_selection_strict",
      msg = paste0(
        "Selected date ",
        strict$date,
        " | scenes=",
        nrow(strict$items_sf),
        " | cloud < ",
        strict_cloud,
        "%",
        " | AOI coverage=",
        round(100 * strict$table$coverage[strict$table$date == strict$date], 2),
        "%"
      )
    )

    return(attach_landsat_metadata(
      items,
      strict,
      allowed_platforms,
      strict_cloud,
      min_coverage
    ))
  }

  # Tier 2: relaxed
  relaxed <- try_landsat_threshold(
    items = items,
    items_sf = items_sf,
    border_sf = border_sf,
    threshold = relaxed_cloud,
    min_coverage = min_coverage,
    cache = cache
  )

  if (!is.null(relaxed) && isTRUE(relaxed$success)) {
    cache_log(
      cache,
      "landsat_selection_relaxed",
      level = "WARN",
      msg = paste0(
        "No full AOI coverage at cloud < ",
        strict_cloud,
        "%. ",
        "Falling back to date ",
        relaxed$date,
        " | scenes=",
        nrow(relaxed$items_sf),
        " | cloud < ",
        relaxed_cloud,
        "%",
        " | AOI coverage=",
        round(
          100 * relaxed$table$coverage[relaxed$table$date == relaxed$date],
          2
        ),
        "%"
      )
    )

    return(attach_landsat_metadata(
      items,
      relaxed,
      allowed_platforms,
      relaxed_cloud,
      min_coverage
    ))
  }

  # Tier 3: nothing usable
  best_table <- (relaxed %||% strict)$table

  msg <- paste0(
    "No Landsat date satisfies AOI coverage requirement. ",
    "Required coverage: ",
    min_coverage,
    ". Strict cloud: <",
    strict_cloud,
    "%",
    ". Relaxed cloud: <",
    relaxed_cloud,
    "%."
  )

  if (!is.null(best_table) && nrow(best_table) > 0) {
    msg <- paste0(
      msg,
      "\nBest available dates:\n",
      paste(
        utils::capture.output(print(utils::head(best_table, 10))),
        collapse = "\n"
      )
    )
  }

  stop(msg)
}

# ------------------------------------------------------------
# 8.5 STAC search (cached)
# ------------------------------------------------------------

get_landsat_stac <- function(
  cache,
  border_sf,
  year,
  area_id = "aoi",
  strict_cloud = 2,
  relaxed_cloud = 10,
  min_coverage = 0.999,
  allowed_platforms = c("landsat-8", "landsat-9")
) {
  criteria_id <- safe_filename(
    paste0(
      "platform_",
      paste(allowed_platforms, collapse = "-"),
      "_cc",
      strict_cloud,
      "_",
      relaxed_cloud,
      "_cov",
      min_coverage
    )
  )

  key <- paste0(
    "api:landsat:",
    area_id,
    ":",
    year,
    ":stac_search_",
    criteria_id,
    ".rds"
  )

  cache$cached(
    key,
    quote({
      bbx <- bbox_wgs84(border_sf)
      dt <- paste0(year, "-05-01T00:00:00Z/", year, "-09-15T23:59:59Z")

      items <- rstac::stac(
        "https://planetarycomputer.microsoft.com/api/stac/v1"
      ) |>
        rstac::stac_search(
          collections = "landsat-c2-l2",
          bbox = bbx,
          datetime = dt,
          limit = 500
        ) |>
        rstac::post_request()

      if (length(items$features) == 0) {
        stop("No Landsat scenes found for year ", year)
      }

      filter_landsat_by_cloud_and_coverage(
        items = items,
        border_sf = border_sf,
        strict_cloud = strict_cloud,
        relaxed_cloud = relaxed_cloud,
        min_coverage = min_coverage,
        allowed_platforms = allowed_platforms,
        cache = cache
      )
    })
  )
}

# ------------------------------------------------------------
# 8.6 Selection extraction
# ------------------------------------------------------------

select_landsat_items <- function(cache, items, year, area_id = "aoi") {
  criteria_id <- items$pipeline_metadata$criteria_id %||% "coverage_v2"

  key <- paste0(
    "api:landsat:",
    area_id,
    ":",
    year,
    ":selection_",
    criteria_id,
    ".rds"
  )

  cache$cached(
    key,
    quote({
      if (length(items$features) == 0) {
        stop("No Landsat scenes found for year ", year)
      }

      dates <- vapply(
        items$features,
        function(x) substr(x$properties$datetime, 1, 10),
        character(1)
      )

      unique_dates <- unique(dates)

      if (length(unique_dates) != 1) {
        stop(
          "Expected filtered Landsat items to contain exactly one date, got: ",
          paste(unique_dates, collapse = ", ")
        )
      }

      best_date <- unique_dates[1]
      sel <- seq_along(items$features)

      ids <- vapply(
        items$features[sel],
        function(x) {
          if (!is.null(x$id)) {
            x$id
          } else {
            safe_filename(x$properties$datetime)
          }
        },
        character(1)
      )

      list(
        best_date = best_date,
        indices = sel,
        ids = ids,
        asset_names = c("blue", "green", "red", "nir08", "swir16"),
        band_names = c("blue", "green", "red", "nir", "swir16")
      )
    })
  )
}

# ------------------------------------------------------------
# 8.7 Per-band downloader
# ------------------------------------------------------------

download_single_landsat_band <- function(
  cache,
  url,
  area_id,
  year,
  scene_id,
  band_name,
  overwrite = FALSE
) {
  file_key <- paste0(
    "file:landsat:raw:",
    area_id,
    ":",
    year,
    ":",
    scene_id,
    ":",
    band_name,
    ".tif"
  )

  out <- cache$path(file_key)
  cache$ensure_dir(file_key)

  if (!file.exists(out) || overwrite || file.info(out)$size == 0) {
    cache_log(
      cache,
      "landsat_band_download",
      msg = paste0("Downloading Landsat band: ", scene_id, " / ", band_name)
    )

    utils::download.file(
      url,
      destfile = out,
      mode = "wb",
      quiet = TRUE
    )

    if (!file.exists(out) || file.info(out)$size == 0) {
      stop("Failed to download Landsat band: ", scene_id, " / ", band_name)
    }
  }

  out
}

download_single_landsat_scene <- function(
  cache,
  signed_items,
  index,
  scene_id_raw,
  asset_names,
  band_names,
  area_id,
  year,
  overwrite = FALSE
) {
  scene_id <- safe_filename(scene_id_raw)

  urls <- signed_items |>
    rstac::items_select(index) |>
    rstac::assets_select(asset_names = asset_names) |>
    rstac::assets_url()

  band_paths <- character(length(asset_names))
  names(band_paths) <- band_names

  for (b in seq_along(asset_names)) {
    band_paths[[band_names[b]]] <- download_single_landsat_band(
      cache = cache,
      url = urls[[b]],
      area_id = area_id,
      year = year,
      scene_id = scene_id,
      band_name = band_names[b],
      overwrite = overwrite
    )
  }

  list(
    id = scene_id_raw,
    safe_id = scene_id,
    asset_names = asset_names,
    band_names = band_names,
    paths = band_paths
  )
}

# ------------------------------------------------------------
# 8.8 Main downloader
# ------------------------------------------------------------

download_landsat_raw_bands <- function(
  cache,
  border_sf,
  year,
  area_id = "aoi",
  overwrite = FALSE,
  strict_cloud = 2,
  relaxed_cloud = 10,
  min_coverage = 0.999,
  allowed_platforms = c("landsat-8", "landsat-9")
) {
  manifest_key <- paste0(
    "api:landsat:",
    area_id,
    ":",
    year,
    ":raw_manifest.rds"
  )

  if (cache$exists(manifest_key) && !overwrite) {
    manifest <- cache$get(manifest_key)

    all_paths <- unlist(
      lapply(manifest$scenes, function(s) s$paths),
      use.names = FALSE
    )

    if (length(all_paths) > 0 && all(file.exists(all_paths))) {
      cache_log(
        cache,
        "landsat_raw_reuse",
        msg = paste0("Reuse Landsat raw manifest for ", area_id, " / ", year)
      )
      return(manifest)
    }
  }

  items <- get_landsat_stac(
    cache = cache,
    border_sf = border_sf,
    year = year,
    area_id = area_id,
    strict_cloud = strict_cloud,
    relaxed_cloud = relaxed_cloud,
    min_coverage = min_coverage,
    allowed_platforms = allowed_platforms
  )

  selection <- select_landsat_items(cache, items, year, area_id)

  cache_log(
    cache,
    "landsat_selection",
    msg = paste0(
      "Landsat selected date=",
      selection$best_date,
      " | scenes=",
      paste(selection$ids, collapse = ", ")
    )
  )

  signed_items <- rstac::items_sign(
    items,
    rstac::sign_planetary_computer()
  )

  scenes <- vector("list", length(selection$indices))
  names(scenes) <- selection$ids

  for (j in seq_along(selection$indices)) {
    scene <- download_single_landsat_scene(
      cache = cache,
      signed_items = signed_items,
      index = selection$indices[j],
      scene_id_raw = selection$ids[j],
      asset_names = selection$asset_names,
      band_names = selection$band_names,
      area_id = area_id,
      year = year,
      overwrite = overwrite
    )

    scene$date <- selection$best_date
    scenes[[j]] <- scene
  }

  manifest <- list(
    area_id = area_id,
    year = year,
    best_date = selection$best_date,
    scenes = scenes,
    pipeline_metadata = items$pipeline_metadata
  )

  cache$set(manifest_key, manifest)
  manifest
}

# ------------------------------------------------------------
# 8.9 Diagnostics: scene footprint plot
# ------------------------------------------------------------

plot_landsat_scene_coverage <- function(
  items_sf,
  border_sf,
  selected_date,
  main = "Landsat scene coverage"
) {
  aoi <- sf::st_transform(border_sf, 4326)

  sel <- items_sf[items_sf$date == selected_date, ]

  if (nrow(sel) == 0) {
    stop("No scenes found for date: ", selected_date)
  }

  union_geom <- sf::st_union(sf::st_geometry(sel))

  graphics::plot(
    sf::st_geometry(aoi),
    col = NA,
    border = "#000000",
    lwd = 3,
    main = main,
    axes = TRUE
  )

  graphics::plot(
    sf::st_geometry(sel),
    col = grDevices::rgb(0.36, 0.61, 0.84, 0.3),
    border = "#5b9bd5",
    lwd = 1.5,
    add = TRUE
  )

  graphics::plot(
    union_geom,
    col = NA,
    border = "#e60000",
    lwd = 2,
    lty = 2,
    add = TRUE
  )

  graphics::plot(
    sf::st_geometry(aoi),
    col = NA,
    border = "#000000",
    lwd = 2,
    add = TRUE
  )

  centroids <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(sel)))

  graphics::text(
    centroids[, "X"],
    centroids[, "Y"],
    labels = sel$id,
    cex = 0.55,
    col = "#000000",
    font = 2
  )

  graphics::legend(
    "topright",
    legend = c(
      "AOI",
      paste0("Scenes (n=", nrow(sel), ")"),
      "Union footprint"
    ),
    col = c("#000000", "#5b9bd5", "#e60000"),
    lty = c(1, 1, 2),
    lwd = c(2, 1.5, 2),
    bty = "n",
    cex = 0.85
  )

  invisible(sel)
}

# ------------------------------------------------------------
# 8.10 Diagnostics: scene summary logger
# ------------------------------------------------------------

log_landsat_scene_summary <- function(
  items_sf,
  border_sf,
  selected_date,
  cache = NULL
) {
  sel <- items_sf[items_sf$date == selected_date, ]

  aoi <- sf::st_transform(border_sf, 2180)
  aoi_geom <- sf::st_union(sf::st_geometry(aoi))
  aoi_area <- as.numeric(sf::st_area(aoi_geom))

  footprints <- sf::st_transform(sel, 2180)
  union_geom <- sf::st_union(sf::st_geometry(footprints))

  inter <- suppressWarnings(
    sf::st_intersection(
      sf::st_sf(geometry = union_geom, crs = 2180),
      sf::st_sf(geometry = aoi_geom, crs = 2180)
    )
  )

  inter_area <- if (nrow(inter) == 0) 0 else sum(as.numeric(sf::st_area(inter)))
  coverage_pct <- 100 * inter_area / aoi_area

  cache_log(
    cache,
    "landsat_scene_summary",
    msg = paste0(
      "Date=",
      selected_date,
      " | scenes=",
      nrow(sel),
      " | mean_cloud=",
      round(mean(sel$cloud_cover, na.rm = TRUE), 2),
      "%",
      " | aoi_coverage=",
      round(coverage_pct, 2),
      "%",
      " | ids=",
      paste(sel$id, collapse = ", ")
    )
  )

  invisible(sel)
}

# ------------------------------------------------------------
# 8.11 Diagnostics: AOI valid-pixel coverage check
# ------------------------------------------------------------

assert_landsat_valid_coverage <- function(
  landsat_raster,
  border_sf,
  min_valid_coverage = 0.99,
  cache = NULL
) {
  tmpl <- landsat_raster[[1]]

  border_v <- terra::project(
    terra::vect(border_sf),
    terra::crs(tmpl)
  )

  aoi_mask <- terra::rasterize(
    border_v,
    tmpl,
    field = 1,
    touches = TRUE
  )

  valid_all_bands <- terra::app(
    landsat_raster,
    function(x) {
      as.integer(all(!is.na(x) & x != 0))
    }
  )

  valid_in_aoi <- terra::mask(valid_all_bands, aoi_mask)

  n_aoi <- as.numeric(
    terra::global(!is.na(aoi_mask), "sum", na.rm = TRUE)[1, 1]
  )

  n_valid <- as.numeric(
    terra::global(valid_in_aoi, "sum", na.rm = TRUE)[1, 1]
  )

  ratio <- n_valid / n_aoi

  cache_log(
    cache,
    "landsat_valid_coverage",
    msg = paste0(
      "Valid Landsat all-band AOI coverage: ",
      round(100 * ratio, 2),
      "% (",
      n_valid,
      "/",
      n_aoi,
      " cells)"
    )
  )

  if (is.na(ratio) || ratio < min_valid_coverage) {
    stop(
      "Landsat raster does not fully cover AOI with valid all-band pixels. ",
      "Required: ",
      round(100 * min_valid_coverage, 2),
      "%, ",
      "actual: ",
      round(100 * ratio, 2),
      "%. ",
      "Consider different date, relaxed cloud, or different platforms."
    )
  }

  invisible(ratio)
}

# ------------------------------------------------------------
# 8.12 Diagnostics: mosaic gap detection
# ------------------------------------------------------------

detect_landsat_mosaic_gaps <- function(
  landsat_raster,
  border_sf,
  max_gap_pct = 1.0,
  cache = NULL
) {
  border_v <- terra::project(
    terra::vect(border_sf),
    terra::crs(landsat_raster)
  )

  aoi_mask <- terra::rasterize(
    border_v,
    landsat_raster[[1]],
    field = 1,
    touches = TRUE
  )

  n_aoi <- as.numeric(
    terra::global(!is.na(aoi_mask), "sum", na.rm = TRUE)[1, 1]
  )

  band_gap_pct <- vapply(
    seq_len(terra::nlyr(landsat_raster)),
    function(i) {
      band <- landsat_raster[[i]]
      valid_in_aoi <- terra::mask(band, aoi_mask)

      n_valid <- as.numeric(
        terra::global(
          !is.na(valid_in_aoi) & valid_in_aoi != 0,
          "sum",
          na.rm = TRUE
        )[1, 1]
      )

      100 * (1 - n_valid / n_aoi)
    },
    numeric(1)
  )

  names(band_gap_pct) <- names(landsat_raster)
  max_gap <- max(band_gap_pct, na.rm = TRUE)
  worst_band <- names(band_gap_pct)[which.max(band_gap_pct)]

  level <- if (max_gap > max_gap_pct) "WARN" else "INFO"

  cache_log(
    cache,
    "landsat_mosaic_gaps",
    level = level,
    msg = paste0(
      "Landsat mosaic gap check | worst_band=",
      worst_band,
      " | gap=",
      round(max_gap, 2),
      "%",
      " | per_band=",
      paste(
        paste0(names(band_gap_pct), ":", round(band_gap_pct, 2), "%"),
        collapse = ", "
      )
    )
  )

  if (max_gap > max_gap_pct) {
    warning(
      "Landsat mosaic has gaps exceeding threshold. ",
      "Max gap: ",
      round(max_gap, 2),
      "% in band ",
      worst_band,
      " (threshold: ",
      max_gap_pct,
      "%)."
    )
  }

  invisible(band_gap_pct)
}

# ============================================================
# 9. ANALYSIS HELPERS
# ============================================================

area_share <- function(r, mapping_dir = "data") {
  if (!inherits(r, "SpatRaster")) {
    r <- terra::rast(r)
  }

  f <- as.data.frame(terra::freq(r))
  f <- f[!is.na(f$value), , drop = FALSE]

  meta <- get_unified_meta(mapping_dir)

  data.frame(
    class_code = meta$code[match(f$value, meta$value)],
    count = f$count,
    area_km2 = f$count * prod(terra::res(r)) / 1e6,
    stringsAsFactors = FALSE
  )
}

area_bias <- function(ref, pred, mapping_dir = "data") {
  r1 <- area_share(ref, mapping_dir)
  r2 <- area_share(pred, mapping_dir)

  merge(r1, r2, by = "class_code", all = TRUE)
}

agreement_raster <- function(paths) {
  terra::app(terra::rast(paths), function(x) length(unique(stats::na.omit(x))))
}

plot_unified <- function(r, mapping_dir = "data", main = "") {
  r <- if (!inherits(r, "SpatRaster")) terra::rast(r) else r
  meta <- get_unified_meta(mapping_dir)

  rf <- terra::as.factor(r)
  levels(rf) <- data.frame(
    value = meta$value,
    label = meta$code
  )

  terra::plot(
    rf,
    col = meta$color,
    type = "classes",
    main = main,
    plg = list(title = "Class", cex = 0.8)
  )
}

# ============================================================
# 10. BDOT BORDER EXTRACTION
# ============================================================

get_bdot10k_borders <- function(cache, teryt) {
  key <- paste0("geo:border:", teryt, ".rds")

  cache$cached(
    key,
    quote({
      bdot_key <- paste0("geo:bdot10k:", teryt, ".gpkg")

      if (!cache$exists(bdot_key)) {
        stop("BDOT data not found. Run bdot_zip_to_gpkg first.")
      }

      layer_meta <- cache$layers(bdot_key)
      all_layers <- layer_meta$name

      read_layer <- function(nm) {
        sf::st_make_valid(sf::st_as_sf(cache$get_layer(bdot_key, nm)))
      }

      border_sf <- NULL

      adja <- all_layers[
        grepl("OT_ADJA", all_layers, ignore.case = TRUE)
      ]

      if (length(adja) > 0) {
        admin <- tryCatch(
          read_layer(adja[1]),
          error = function(e) NULL
        )

        if (!is.null(admin) && nrow(admin) > 0) {
          teryt_cols <- names(admin)[grepl(
            "teryt|jpt_kod|kod_je|kod",
            names(admin),
            ignore.case = TRUE
          )]

          keep <- rep(FALSE, nrow(admin))

          for (tc in teryt_cols) {
            vals <- as.character(admin[[tc]])
            keep <- keep | vals == teryt | startsWith(vals, teryt)
          }

          if (any(keep, na.rm = TRUE)) {
            admin <- admin[keep, ]

            if ("rodzaj" %in% names(admin)) {
              pow <- admin[
                admin$rodzaj %in% c("Pow", "pow", "POW", "powiat"),
              ]
              if (nrow(pow) > 0) {
                admin <- pow
              }
            }

            border_sf <- admin
          }
        }
      }

      if (is.null(border_sf) || nrow(border_sf) == 0) {
        pt_lyrs <- all_layers[
          grepl("OT_PT.*_A$", all_layers, ignore.case = TRUE)
        ]

        if (length(pt_lyrs) == 0) {
          geomtype <- as.character(layer_meta$geomtype)

          poly_lyrs <- all_layers[
            grepl("Polygon", geomtype, ignore.case = TRUE)
          ]

          if (length(poly_lyrs) == 0) {
            poly_lyrs <- all_layers[
              grepl("_A$", all_layers, ignore.case = TRUE)
            ]
          }

          pt_lyrs <- poly_lyrs
        }

        if (length(pt_lyrs) == 0) {
          stop(
            "No polygon layers available for border reconstruction. Layers: ",
            paste(all_layers, collapse = ", ")
          )
        }

        pt_list <- list()

        for (lyr in pt_lyrs) {
          obj <- tryCatch(
            read_layer(lyr),
            error = function(e) NULL
          )

          if (!is.null(obj) && nrow(obj) > 0) {
            pt_list[[length(pt_list) + 1]] <- obj
          }
        }

        if (length(pt_list) == 0) {
          stop("Failed to read any BDOT polygon layer.")
        }

        all_pt <- do.call(rbind, pt_list)

        border_sf <- sf::st_sf(
          id = 1,
          geometry = sf::st_union(sf::st_geometry(all_pt)),
          crs = sf::st_crs(all_pt)
        )
      }

      geom <- sf::st_union(sf::st_geometry(sf::st_make_valid(border_sf)))

      out <- sf::st_sf(
        id = 1,
        geometry = geom,
        crs = sf::st_crs(border_sf)
      )

      sf::st_transform(out, 2180)
    })
  )
}

# ============================================================
# 11. CLC VECTOR DOWNLOAD
# ============================================================

CLC_BASE_URL <- paste0(
  "https://image.discomap.eea.europa.eu/arcgis/rest/services/",
  "Corine/CLC2018_WM/MapServer/0/query"
)

CLC_TIMEOUT <- 30
CLC_MAX_RETRIES <- 3
CLC_CHUNK_SIZE <- 200

safe_get <- function(
  url,
  query,
  timeout = CLC_TIMEOUT,
  retries = CLC_MAX_RETRIES
) {
  for (i in seq_len(retries)) {
    resp <- try(
      httr::GET(
        url,
        query = query,
        httr::timeout(timeout)
      ),
      silent = TRUE
    )

    if (!inherits(resp, "try-error") && httr::status_code(resp) == 200) {
      return(resp)
    }

    Sys.sleep(2^i)
  }

  stop("GET failed after ", retries, " retries")
}

get_clc_ids <- function(cache, border_sf, area_id = "aoi") {
  key <- paste0("api:clc:", area_id, ":ids.rds")

  cache$cached(
    key,
    quote({
      bbx <- bbox_wgs84(border_sf)

      resp <- safe_get(
        CLC_BASE_URL,
        query = list(
          where = "1=1",
          returnIdsOnly = "true",
          returnGeometry = "false",
          f = "json",
          geometry = paste(bbx, collapse = ","),
          geometryType = "esriGeometryEnvelope",
          inSR = 4326,
          spatialRel = "esriSpatialRelIntersects"
        )
      )

      obj <- jsonlite::fromJSON(
        httr::content(resp, "text", encoding = "UTF-8")
      )

      ids <- sort(unique(obj$objectIds))

      if (length(ids) == 0) {
        stop("No CLC IDs found")
      }

      ids
    })
  )
}

get_clc_chunk <- function(cache, id_chunk, chunk_idx, area_id = "aoi") {
  key <- paste0("api:clc:", area_id, ":chunk_", chunk_idx, ".rds")

  cache$cached(
    key,
    quote({
      resp <- safe_get(
        CLC_BASE_URL,
        query = list(
          objectIds = paste(id_chunk, collapse = ","),
          outFields = "*",
          returnGeometry = "true",
          f = "geojson",
          outSR = 4326
        )
      )

      txt <- httr::content(resp, "text", encoding = "UTF-8")
      sf::st_read(txt, quiet = TRUE)
    })
  )
}

get_clc_vector <- function(cache, border_sf, area_id = "aoi") {
  key <- paste0("geo:clc:", area_id, ".rds")

  cache$cached(
    key,
    quote({
      ids <- get_clc_ids(cache, border_sf, area_id)

      cache_log(
        cache,
        "clc_ids",
        msg = paste0("CLC IDs total: ", length(ids))
      )

      id_chunks <- split(ids, ceiling(seq_along(ids) / CLC_CHUNK_SIZE))

      clc_list <- lapply(seq_along(id_chunks), function(i) {
        cache_log(
          cache,
          "clc_chunk",
          msg = paste0("Downloading CLC chunk ", i, "/", length(id_chunks))
        )
        get_clc_chunk(cache, id_chunks[[i]], i, area_id)
      })

      clc <- do.call(rbind, clc_list)
      clc <- sf::st_make_valid(clc)
      clc <- clc[!duplicated(clc$OBJECTID), , drop = FALSE]

      clc <- sf::st_intersection(
        sf::st_transform(clc, sf::st_crs(border_sf)),
        border_sf
      )

      clc <- clc[!sf::st_is_empty(clc), , drop = FALSE]

      if (nrow(clc) == 0) {
        stop("CLC empty after clip")
      }

      clc
    })
  )
}

# ============================================================
# 12. S2GLC
# ============================================================

S2GLC_WCS_URL <- "https://mapy.geoportal.gov.pl/wss/service/POLSA/WCS/LandCover"

get_s2glc_meta <- function(mapping_dir = "../data") {
  schema <- read_json(file.path(mapping_dir, "s2glc_schema.json"))
  examples <- as.data.frame(schema$examples, stringsAsFactors = FALSE)

  data.frame(
    class_id = as.integer(examples$classId),
    class_name = as.character(examples$className),
    color_hex = as.character(examples$color),
    stringsAsFactors = FALSE
  )
}

get_s2glc_palette <- function(mapping_dir = "../data") {
  meta <- get_s2glc_meta(mapping_dir)

  hex <- sub("^#", "", meta$color_hex)

  meta$red <- strtoi(substr(hex, 1, 2), 16L)
  meta$green <- strtoi(substr(hex, 3, 4), 16L)
  meta$blue <- strtoi(substr(hex, 5, 6), 16L)

  meta[, c("class_id", "class_name", "color_hex", "red", "green", "blue")]
}

plot_s2glc_classes <- function(
  r,
  mapping_dir = "../data",
  main = "",
  lang = "pl"
) {
  r <- if (!inherits(r, "SpatRaster")) terra::rast(r) else r
  meta <- get_s2glc_meta(mapping_dir)

  if (lang == "en") {
    label_map <- c(
      "Tereny antropogeniczne" = "Anthropogenic areas",
      "Tereny rolne" = "Agricultural areas",
      "Lasy liściaste" = "Deciduous forests",
      "Lasy iglaste" = "Coniferous forests",
      "Roślinność trawiasta" = "Grassland vegetation",
      "Wrzosowiska i zakrzaczenia" = "Heathlands and shrubs",
      "Tereny podmokłe" = "Wetlands",
      "Torfowiska" = "Peatlands",
      "Tereny naturalne pozbawione roślinności" = "Natural bare areas",
      "Obszary wodne" = "Water bodies"
    )
    meta$class_name <- unname(label_map[meta$class_name])
    legend_title <- "S2GLC Class"
  } else {
    legend_title <- "S2GLC Klasa"
  }

  rf <- terra::as.factor(r)

  levels(rf) <- data.frame(
    value = meta$class_id,
    label = meta$class_name
  )

  terra::plot(
    rf,
    type = "classes",
    col = meta$color_hex,
    main = main,
    plg = list(title = legend_title, cex = 0.75)
  )
}

rgb_to_class <- function(r, mapping_dir = "../data") {
  palette <- get_s2glc_palette(mapping_dir)

  red <- as.integer(terra::values(r[[1]]))
  green <- as.integer(terra::values(r[[2]]))
  blue <- as.integer(terra::values(r[[3]]))

  n_pixels <- length(red)
  class_vals <- rep(NA_integer_, n_pixels)
  best_dist <- rep(Inf, n_pixels)

  for (i in seq_len(nrow(palette))) {
    d <- (red - palette$red[i])^2 +
      (green - palette$green[i])^2 +
      (blue - palette$blue[i])^2

    closer <- d < best_dist
    closer[is.na(closer)] <- FALSE

    best_dist[closer] <- d[closer]
    class_vals[closer] <- palette$class_id[i]
  }

  max_dist_sq <- 50^2 * 3
  class_vals[best_dist > max_dist_sq] <- NA_integer_

  tmpl <- r[[1]]
  terra::values(tmpl) <- class_vals
  names(tmpl) <- "class"
  tmpl
}

s2glc_to_class_raster <- function(r, mapping_dir = "../data") {
  if (terra::nlyr(r) >= 3) {
    names(r)[1:3] <- c("red", "green", "blue")
    rgb_to_class(r, mapping_dir)
  } else {
    out <- r[[1]]
    names(out) <- "class"
    out
  }
}

s2glc_coverage_id <- function(year) {
  paste0("Land_use_classification_", year)
}

get_s2glc_wcs_capabilities <- function(cache) {
  cache$cached(
    "api:s2glc:polsa:capabilities.rds",
    quote({
      resp <- httr::GET(
        S2GLC_WCS_URL,
        query = list(SERVICE = "WCS", REQUEST = "GetCapabilities"),
        httr::timeout(60)
      )

      httr::stop_for_status(resp)
      httr::content(resp, "text", encoding = "UTF-8")
    })
  )
}

get_s2glc_wcs_coverages <- function(cache) {
  cache$cached(
    "api:s2glc:polsa:coverages.rds",
    quote({
      xml <- xml2::read_xml(get_s2glc_wcs_capabilities(cache))

      ids <- xml2::xml_text(
        xml2::xml_find_all(
          xml,
          ".//*[local-name()='CoverageSummary']/*[local-name()='CoverageId']"
        )
      )

      data.frame(coverage_id = ids, stringsAsFactors = FALSE)
    })
  )
}

assert_s2glc_coverage <- function(cache, year) {
  coverage_id <- s2glc_coverage_id(year)
  available <- get_s2glc_wcs_coverages(cache)$coverage_id

  if (!coverage_id %in% available) {
    stop(
      "S2GLC coverage not available for year ",
      year,
      ". Available: ",
      paste(available, collapse = ", ")
    )
  }

  coverage_id
}

s2glc_request_grid <- function(border_sf, res = 10) {
  bbx <- sf::st_bbox(sf::st_transform(border_sf, 2180))

  list(
    bbox = bbx,
    width = ceiling((bbx["xmax"] - bbx["xmin"]) / res),
    height = ceiling((bbx["ymax"] - bbx["ymin"]) / res)
  )
}

is_xml_error_file <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  head_bytes <- readBin(con, "raw", n = 512)
  head_txt <- rawToChar(head_bytes[head_bytes != as.raw(0)], multiple = FALSE)

  grepl(
    "<\\?xml|Exception|ServiceException",
    head_txt,
    ignore.case = TRUE,
    useBytes = TRUE
  )
}

download_s2glc_wcs_tiff <- function(cache, border_sf, year, area_id) {
  coverage_id <- s2glc_coverage_id(year)

  raw_key <- paste0(
    "file:s2glc:wcs_raw:",
    area_id,
    ":",
    year,
    ":",
    coverage_id,
    ".tif"
  )

  raw_path <- cache$path(raw_key)

  if (file.exists(raw_path) && file.info(raw_path)$size > 0) {
    cache_log(
      cache,
      "s2glc_raw_reuse",
      msg = paste0("Reuse S2GLC raw WCS TIFF: ", raw_key)
    )
    return(raw_path)
  }

  cache$ensure_dir(raw_key)
  grid <- s2glc_request_grid(border_sf)
  bbx <- grid$bbox

  cache_log(
    cache,
    "s2glc_raw_download",
    msg = paste0("Downloading S2GLC ", year, " from POLSA WCS: ", coverage_id)
  )

  resp <- httr::GET(
    S2GLC_WCS_URL,
    query = list(
      SERVICE = "WCS",
      VERSION = "1.0.0",
      REQUEST = "GetCoverage",
      COVERAGE = coverage_id,
      CRS = "EPSG:2180",
      RESPONSE_CRS = "EPSG:2180",
      BBOX = paste(
        bbx["xmin"],
        bbx["ymin"],
        bbx["xmax"],
        bbx["ymax"],
        sep = ","
      ),
      WIDTH = as.integer(grid$width),
      HEIGHT = as.integer(grid$height),
      FORMAT = "image/tiff"
    ),
    httr::write_disk(raw_path, overwrite = TRUE),
    httr::timeout(180)
  )

  httr::stop_for_status(resp)

  if (is_xml_error_file(raw_path)) {
    txt <- paste(readLines(raw_path, warn = FALSE, n = 50), collapse = "\n")
    unlink(raw_path)
    stop("POLSA WCS returned XML/error instead of TIFF.\nFirst lines:\n", txt)
  }

  raw_path
}

get_s2glc_polsa <- function(
  cache,
  border_sf,
  year = 2021,
  area_id = "aoi",
  mapping_dir = "../data"
) {
  key <- paste0("raster:s2glc:", area_id, ":", year, ".tif")

  cache$cached(
    key,
    quote({
      assert_s2glc_coverage(cache, year)

      raw_path <- download_s2glc_wcs_tiff(cache, border_sf, year, area_id)
      r <- terra::rast(raw_path)

      cache_log(
        cache,
        "s2glc_wcs_loaded",
        msg = paste0("S2GLC WCS returned ", terra::nlyr(r), " band(s)")
      )

      r_class <- s2glc_to_class_raster(r, mapping_dir = mapping_dir)

      v <- terra::project(terra::vect(border_sf), terra::crs(r_class))
      terra::mask(r_class, v)
    })
  )
}

# ============================================================
# 13. RECLASSIFICATION
# ============================================================

reclassify_clc <- function(
  cache,
  border_sf,
  landsat_raster,
  mapping_dir,
  area_id = "aoi",
  code_col = "Code_18"
) {
  key <- paste0("raster:ref:clc:", area_id, ".tif")

  cache$cached(
    key,
    quote({
      clc <- cache$get(paste0("geo:clc:", area_id, ".rds"))

      if (is.null(clc)) {
        stop("CLC data not in cache. Run get_clc_vector first.")
      }

      if (!code_col %in% names(clc)) {
        stop(
          "Expected CLC code column not found: ",
          code_col,
          "\nAvailable columns: ",
          paste(names(clc), collapse = ", ")
        )
      }

      clc <- sf::st_intersection(
        sf::st_transform(clc, sf::st_crs(border_sf)),
        border_sf
      )

      clc$src <- as.integer(clc[[code_col]])
      xw <- load_clc_crosswalk(mapping_dir)

      clc <- merge(
        clc,
        xw,
        by.x = "src",
        by.y = "from",
        all.x = TRUE,
        sort = FALSE
      )

      clc <- clc[!is.na(clc$to), , drop = FALSE]

      tmpl <- landsat_raster[[1]]
      v <- terra::project(terra::vect(clc), terra::crs(tmpl))

      terra::rasterize(
        v,
        tmpl,
        field = "to",
        background = NA,
        touches = TRUE
      )
    })
  )
}

reclassify_s2glc <- function(
  cache,
  landsat_raster,
  mapping_dir,
  year = 2021,
  area_id = "aoi"
) {
  key <- paste0("raster:ref:s2glc:", area_id, ":", year, ".tif")

  cache$cached(
    key,
    quote({
      s2glc_r <- cache$get(paste0("raster:s2glc:", area_id, ":", year, ".tif"))

      if (is.null(s2glc_r)) {
        stop("S2GLC data not in cache. Run get_s2glc_polsa first.")
      }

      xw <- load_s2glc_crosswalk(mapping_dir)
      rcl <- as.matrix(xw[, c("from", "to")])

      terra::project(
        terra::classify(s2glc_r, rcl, others = NA),
        landsat_raster[[1]],
        method = "near"
      )
    })
  )
}

parse_bdot_key <- function(key) {
  base <- sub("\\(.*\\)$", "", key)

  attr <- if (grepl("\\(", key)) {
    sub("^.*\\((.*)\\)$", "\\1", key)
  } else {
    NA_character_
  }

  list(base = base, attr = attr)
}

find_bdot_layer <- function(layers, base) {
  pat <- paste0(base, "(_A|_L|_P)?$")

  cands <- layers[
    grepl(pat, layers, ignore.case = TRUE)
  ]

  if (length(cands) == 0) {
    return(NA_character_)
  }

  poly <- cands[
    grepl("_A$", cands, ignore.case = TRUE)
  ]

  if (length(poly) > 0) {
    return(poly[1])
  }

  cands[1]
}

find_bdot_layer_for_key <- function(
  cache,
  bdot_key,
  source_key,
  all_layers,
  verbose = TRUE
) {
  pk <- parse_bdot_key(source_key)
  lyr <- find_bdot_layer(all_layers, pk$base)

  if (is.na(lyr)) {
    cache_log(
      cache,
      "bdot_layer_not_found",
      level = "DEBUG",
      msg = paste0("BDOT layer not found for key: ", source_key)
    )
    return(NULL)
  }

  cache_log(
    cache,
    "bdot_layer_selected",
    level = "DEBUG",
    msg = paste0(
      "BDOT layer selected: ",
      lyr,
      if (!is.na(pk$attr)) paste0(" [", pk$attr, "]") else ""
    )
  )

  obj <- tryCatch(
    cache$get_layer(bdot_key, lyr),
    error = function(e) {
      cache_log(
        cache,
        "bdot_layer_read_failed",
        level = "WARN",
        msg = paste0("BDOT read failed for ", lyr, ": ", conditionMessage(e))
      )
      NULL
    }
  )

  if (is.null(obj) || nrow(obj) == 0) {
    return(NULL)
  }

  if (!is.na(pk$attr)) {
    attrs <- tryCatch(
      terra::values(obj),
      error = function(e) NULL
    )

    if (is.null(attrs)) {
      cache_log(
        cache,
        "bdot_attrs_missing",
        level = "WARN",
        msg = paste0("Attributes unavailable for BDOT layer: ", lyr)
      )
      return(NULL)
    }

    acol <- intersect(c("rodzaj", "kategoria"), names(attrs))

    if (length(acol) == 0) {
      cache_log(
        cache,
        "bdot_attr_column_missing",
        level = "DEBUG",
        msg = paste0("No rodzaj/kategoria column in layer: ", lyr)
      )
      return(NULL)
    }

    keep <- as.character(attrs[[acol[1]]]) == pk$attr
    keep[is.na(keep)] <- FALSE

    if (!any(keep)) {
      cache_log(
        cache,
        "bdot_attr_no_match",
        level = "DEBUG",
        msg = paste0("No matching features in ", lyr, " for attr ", pk$attr)
      )
      return(NULL)
    }

    obj <- obj[keep]

    if (nrow(obj) == 0) {
      return(NULL)
    }
  }

  obj
}

rasterize_and_mask_bdot <- function(obj, tmpl, border_v, cache = NULL) {
  if (is.null(obj) || nrow(obj) == 0) {
    return(NULL)
  }

  obj <- tryCatch(
    terra::project(obj, terra::crs(tmpl)),
    error = function(e) {
      cache_log(
        cache,
        "bdot_project_failed",
        level = "WARN",
        msg = paste0("BDOT project failed: ", conditionMessage(e))
      )
      NULL
    }
  )

  if (is.null(obj) || nrow(obj) == 0) {
    return(NULL)
  }

  rr <- tryCatch(
    terra::rasterize(
      obj,
      tmpl,
      field = "uval",
      background = NA,
      touches = TRUE
    ),
    error = function(e) {
      cache_log(
        cache,
        "bdot_rasterize_failed",
        level = "WARN",
        msg = paste0("BDOT rasterize failed: ", conditionMessage(e))
      )
      NULL
    }
  )

  if (is.null(rr)) {
    return(NULL)
  }

  terra::mask(rr, border_v)
}

reclassify_bdot_layer <- function(
  cache,
  bdot_key,
  source_key,
  unified_value,
  all_layers,
  tmpl,
  border_v
) {
  obj <- find_bdot_layer_for_key(
    cache = cache,
    bdot_key = bdot_key,
    source_key = source_key,
    all_layers = all_layers
  )

  if (is.null(obj)) {
    return(NULL)
  }

  obj$uval <- unified_value

  rasterize_and_mask_bdot(
    obj = obj,
    tmpl = tmpl,
    border_v = border_v,
    cache = cache
  )
}

reclassify_bdot <- function(
  cache,
  border_sf,
  landsat_raster,
  teryt,
  mapping_dir
) {
  key <- paste0("raster:ref:bdot:", teryt, ".tif")

  cache$cached(
    key,
    quote({
      bdot_key <- paste0("geo:bdot10k:", teryt, ".gpkg")

      if (!cache$exists(bdot_key)) {
        stop("BDOT data not in cache. Run bdot_zip_to_gpkg first.")
      }

      tmpl <- landsat_raster[[1]]
      border_v <- terra::project(terra::vect(border_sf), terra::crs(tmpl))

      all_layers <- cache$layers(bdot_key)$name
      xw <- load_bdot_crosswalk(mapping_dir)

      out <- tmpl
      terra::values(out) <- NA

      for (i in seq_len(nrow(xw))) {
        rr <- reclassify_bdot_layer(
          cache = cache,
          bdot_key = bdot_key,
          source_key = xw$source_key[i],
          unified_value = xw$unified_value[i],
          all_layers = all_layers,
          tmpl = tmpl,
          border_v = border_v
        )

        if (!is.null(rr)) {
          out <- terra::cover(out, rr)
        }
      }

      n_valid <- terra::global(!is.na(out), "sum", na.rm = TRUE)[1, 1]

      cache_log(
        cache,
        "bdot_reclassification_done",
        msg = paste0("BDOT classified valid pixels: ", n_valid)
      )

      if (is.na(n_valid) || n_valid == 0) {
        warning("BDOT reclassification produced 0 valid pixels.")
      }

      out
    })
  )
}

# ============================================================
# 14. TRAINING SAMPLE PREPARATION
# ============================================================

validate_feature_stack <- function(feats) {
  missing_feats <- setdiff(LANDSAT_FEATURES, names(feats))

  if (length(missing_feats) > 0) {
    stop(
      "Feature stack missing layers: ",
      paste(missing_feats, collapse = ", "),
      "\nAvailable layers: ",
      paste(names(feats), collapse = ", ")
    )
  }

  invisible(TRUE)
}

align_reference_to_landsat <- function(ref_raster, landsat_raster) {
  ref_resampled <- terra::project(
    ref_raster,
    landsat_raster,
    method = "near"
  )

  names(ref_resampled) <- "class_value"
  ref_resampled
}

build_training_stack <- function(landsat_raster, ref_resampled) {
  feats <- add_indices(landsat_raster)
  validate_feature_stack(feats)
  c(feats, ref_resampled)
}

get_reference_class_frequency <- function(ref_resampled) {
  freq_df <- as.data.frame(terra::freq(ref_resampled))
  freq_df <- freq_df[!is.na(freq_df$value) & freq_df$count > 0, , drop = FALSE]

  if (nrow(freq_df) == 0) {
    stop("Reference raster contains no valid class pixels.")
  }

  freq_df
}

class_sample_size <- function(class_count, per_class_target) {
  min(as.integer(class_count), as.integer(per_class_target))
}

mask_stack_to_class <- function(stack, ref_resampled, class_value) {
  mask_r <- terra::ifel(ref_resampled == class_value, 1, NA)
  terra::mask(stack, mask_r)
}

sample_single_class <- function(
  stack,
  ref_resampled,
  class_value,
  class_count,
  per_class_target,
  cache = NULL
) {
  n_take <- class_sample_size(class_count, per_class_target)

  if (n_take <= 0) {
    return(NULL)
  }

  cls_stack <- mask_stack_to_class(
    stack = stack,
    ref_resampled = ref_resampled,
    class_value = class_value
  )

  samp <- tryCatch(
    terra::spatSample(
      cls_stack,
      size = n_take,
      method = "random",
      na.rm = TRUE,
      as.df = TRUE,
      warn = FALSE
    ),
    error = function(e) {
      cache_log(
        cache,
        "sample_class_failed",
        level = "WARN",
        msg = paste0(
          "Sampling failed for class ",
          class_value,
          ": ",
          conditionMessage(e)
        )
      )
      NULL
    }
  )

  if (is.null(samp) || nrow(samp) == 0) {
    cache_log(
      cache,
      "sample_class_skipped",
      level = "DEBUG",
      msg = paste0("Class ", class_value, " skipped")
    )
    return(NULL)
  }

  cache_log(
    cache,
    "sample_class_done",
    level = "DEBUG",
    msg = paste0(
      "Class ",
      class_value,
      ": ",
      nrow(samp),
      " samples of ",
      class_count,
      " pixels"
    )
  )

  samp
}

combine_sample_tables <- function(samples_list) {
  samples_list <- samples_list[
    vapply(samples_list, function(x) !is.null(x) && nrow(x) > 0, logical(1))
  ]

  if (length(samples_list) == 0) {
    stop("No class samples were generated.")
  }

  do.call(rbind, samples_list)
}

clean_training_samples <- function(samp, source_name) {
  samp <- samp[stats::complete.cases(samp), , drop = FALSE]

  needed_cols <- c(LANDSAT_FEATURES, "class_value")
  missing_cols <- setdiff(needed_cols, names(samp))

  if (length(missing_cols) > 0) {
    stop(
      "Sample table for ",
      source_name,
      " is missing columns: ",
      paste(missing_cols, collapse = ", "),
      "\nAvailable columns: ",
      paste(names(samp), collapse = ", ")
    )
  }

  if (nrow(samp) == 0) {
    stop("No valid training samples for source: ", source_name)
  }

  samp
}

stratified_sample_stack <- function(
  stack,
  ref_resampled,
  sample_n = 50000,
  min_per_class = 200,
  seed = 123,
  cache = NULL
) {
  freq_df <- get_reference_class_frequency(ref_resampled)

  n_classes <- nrow(freq_df)
  per_class_target <- max(
    as.integer(min_per_class),
    floor(sample_n / n_classes)
  )

  cache_log(
    cache,
    "sampling_plan",
    msg = paste0(
      "Classes found: ",
      n_classes,
      " | target per class: ",
      per_class_target
    )
  )

  set.seed(seed)

  samples_list <- vector("list", n_classes)

  for (i in seq_len(n_classes)) {
    samples_list[[i]] <- sample_single_class(
      stack = stack,
      ref_resampled = ref_resampled,
      class_value = freq_df$value[i],
      class_count = freq_df$count[i],
      per_class_target = per_class_target,
      cache = cache
    )
  }

  combine_sample_tables(samples_list)
}

prepare_training_samples <- function(
  cache,
  source_name,
  landsat_raster,
  ref_raster,
  sample_n = 50000,
  min_per_class = 200,
  seed = 123
) {
  key <- paste0("table:samples:", source_name, ".rds")

  cache$cached(
    key,
    quote({
      cache_log(
        cache,
        "sampling_start",
        msg = paste0("Sampling pixels for: ", source_name)
      )

      ref_resampled <- align_reference_to_landsat(
        ref_raster = ref_raster,
        landsat_raster = landsat_raster
      )

      stack <- build_training_stack(
        landsat_raster = landsat_raster,
        ref_resampled = ref_resampled
      )

      samp <- stratified_sample_stack(
        stack = stack,
        ref_resampled = ref_resampled,
        sample_n = sample_n,
        min_per_class = min_per_class,
        seed = seed,
        cache = cache
      )

      samp <- clean_training_samples(
        samp = samp,
        source_name = source_name
      )

      cache_log(
        cache,
        "sampling_done",
        msg = paste0("Total samples kept for ", source_name, ": ", nrow(samp))
      )

      samp
    })
  )
}

# ============================================================
# 15. MODEL TRAINING — MODULAR RANDOM FOREST
# ============================================================

make_landcover_formula <- function() {
  stats::as.formula(
    paste("class_value ~", paste(LANDSAT_FEATURES, collapse = " + "))
  )
}

validate_training_table <- function(df, source_name) {
  needed <- c(LANDSAT_FEATURES, "class_value")
  missing_cols <- setdiff(needed, names(df))

  if (length(missing_cols) > 0) {
    stop(
      "Training table for ",
      source_name,
      " is missing columns: ",
      paste(missing_cols, collapse = ", "),
      "\nAvailable columns: ",
      paste(names(df), collapse = ", ")
    )
  }

  if (nrow(df) == 0) {
    stop("Training table is empty for: ", source_name)
  }

  invisible(TRUE)
}

factorize_landcover_target <- function(df, mapping_dir) {
  meta <- get_unified_meta(mapping_dir)

  df$class_value <- factor(
    df$class_value,
    levels = meta$value,
    labels = meta$code
  )

  df <- df[!is.na(df$class_value), , drop = FALSE]

  if (nrow(df) == 0) {
    stop("No valid rows after factorizing class_value.")
  }

  df
}

stratified_train_test_split <- function(df, train_prop = 0.7, seed = 123) {
  set.seed(seed)

  train_idx <- unlist(
    lapply(split(seq_len(nrow(df)), df$class_value), function(idx) {
      n <- length(idx)

      if (n <= 1) {
        return(idx)
      }

      sample(idx, size = max(1, floor(train_prop * n)))
    }),
    use.names = FALSE
  )

  train_data <- df[train_idx, , drop = FALSE]
  test_data <- df[-train_idx, , drop = FALSE]

  if (nrow(test_data) == 0) {
    warning("Test set is empty; using training set for evaluation.")
    test_data <- train_data
  }

  list(
    train = train_data,
    test = test_data
  )
}

fit_random_forest_landcover <- function(
  train_data,
  num_trees = 300,
  seed = 123
) {
  form <- make_landcover_formula()

  ranger::ranger(
    formula = form,
    data = train_data,
    num.trees = num_trees,
    importance = "impurity",
    classification = TRUE,
    probability = FALSE,
    seed = seed,
    respect.unordered.factors = "order"
  )
}

get_ranger_predict_method <- function() {
  fun <- utils::getS3method(
    f = "predict",
    class = "ranger",
    optional = TRUE
  )

  if (is.null(fun)) {
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop("Package 'ranger' is required for random forest prediction.")
    }

    fun <- get(
      "predict.ranger",
      envir = asNamespace("ranger"),
      inherits = FALSE
    )
  }

  fun
}

validate_prediction_data <- function(dat) {
  dat <- as.data.frame(dat)

  missing_cols <- setdiff(LANDSAT_FEATURES, names(dat))

  if (length(missing_cols) > 0) {
    stop(
      "Prediction data missing columns: ",
      paste(missing_cols, collapse = ", "),
      "\nAvailable columns: ",
      paste(names(dat), collapse = ", ")
    )
  }

  dat[, LANDSAT_FEATURES, drop = FALSE]
}

predict_ranger_labels <- function(model, newdata) {
  if (!inherits(model, "ranger")) {
    stop(
      "predict_ranger_labels() expected a ranger model, got: ",
      paste(class(model), collapse = ", ")
    )
  }

  newdata <- validate_prediction_data(newdata)
  predict_fun <- get_ranger_predict_method()

  pred_obj <- predict_fun(
    object = model,
    data = newdata
  )

  preds <- pred_obj$predictions

  if (is.matrix(preds) || is.data.frame(preds)) {
    preds <- colnames(preds)[max.col(preds, ties.method = "first")]
  }

  preds
}

predict_random_forest_classes <- function(model, newdata) {
  predict_ranger_labels(
    model = model,
    newdata = newdata
  )
}

evaluate_landcover_model <- function(model, test_data, class_levels) {
  preds <- predict_random_forest_classes(model, test_data)
  preds <- factor(preds, levels = class_levels)

  obs <- factor(test_data$class_value, levels = class_levels)

  cm <- caret::confusionMatrix(preds, obs)

  list(
    accuracy = cm$overall["Accuracy"],
    kappa = cm$overall["Kappa"],
    confusion = cm$table
  )
}

extract_rf_importance <- function(model) {
  imp <- model$variable.importance

  if (is.null(imp)) {
    return(numeric(0))
  }

  sort(imp, decreasing = TRUE)
}

train_landcover_model <- function(
  cache,
  source_name,
  df,
  mapping_dir,
  train_prop = 0.7,
  num_trees = 300,
  seed = 123
) {
  key <- paste0("model:rf:", source_name, ".rds")

  cache$cached(
    key,
    quote({
      cache_log(
        cache,
        "model_training_start",
        ns = if (!is.null(cache$logger)) {
          cache$logger$namespaces$MODEL
        } else {
          NULL
        },
        msg = paste0("Training random forest for: ", source_name)
      )

      validate_training_table(
        df = df,
        source_name = source_name
      )

      df <- factorize_landcover_target(
        df = df,
        mapping_dir = mapping_dir
      )

      split <- stratified_train_test_split(
        df = df,
        train_prop = train_prop,
        seed = seed
      )

      cache_log(
        cache,
        "model_split",
        ns = if (!is.null(cache$logger)) {
          cache$logger$namespaces$MODEL
        } else {
          NULL
        },
        msg = paste0(
          "Train: ",
          nrow(split$train),
          " | Test: ",
          nrow(split$test),
          " | Classes used: ",
          length(unique(split$train$class_value))
        )
      )

      model <- fit_random_forest_landcover(
        train_data = split$train,
        num_trees = num_trees,
        seed = seed
      )

      metrics <- evaluate_landcover_model(
        model = model,
        test_data = split$test,
        class_levels = levels(df$class_value)
      )

      importance <- extract_rf_importance(model)

      cache_log(
        cache,
        "model_training_done",
        ns = if (!is.null(cache$logger)) {
          cache$logger$namespaces$MODEL
        } else {
          NULL
        },
        msg = paste0(
          "Accuracy: ",
          round(metrics$accuracy, 4),
          " | Kappa: ",
          round(metrics$kappa, 4),
          " | OOB: ",
          round(model$prediction.error, 4)
        )
      )

      list(
        model = model,
        accuracy = metrics$accuracy,
        kappa = metrics$kappa,
        confusion = metrics$confusion,
        importance = importance,
        oob_error = model$prediction.error,
        train_n = nrow(split$train),
        test_n = nrow(split$test),
        model_type = "ranger_random_forest"
      )
    })
  )
}

# ============================================================
# 16. CLASSIFICATION — RANDOM FOREST
# ============================================================

landcover_labels_to_values <- function(labels, mapping_dir) {
  meta <- get_unified_meta(mapping_dir)

  labels_chr <- as.character(labels)
  values <- meta$value[match(labels_chr, meta$code)]

  bad <- is.na(values) & !is.na(labels_chr)

  if (any(bad)) {
    stop(
      "Unknown predicted land-cover labels: ",
      paste(unique(labels_chr[bad]), collapse = ", "),
      "\nKnown labels: ",
      paste(meta$code, collapse = ", ")
    )
  }

  as.integer(values)
}

predict_rf_pixels <- function(model, dat, mapping_dir) {
  labels <- predict_ranger_labels(
    model = model,
    newdata = dat
  )

  landcover_labels_to_values(
    labels = labels,
    mapping_dir = mapping_dir
  )
}

build_prediction_stack <- function(landsat_raster) {
  feats <- add_indices(landsat_raster)
  validate_feature_stack(feats)
  feats
}

make_rf_terra_predict_fun <- function(mapping_dir) {
  force(mapping_dir)

  function(model, dat, ...) {
    predict_rf_pixels(
      model = model,
      dat = dat,
      mapping_dir = mapping_dir
    )
  }
}

classify_raster <- function(
  cache,
  model,
  landsat_raster,
  ref_type,
  mapping_dir = "data"
) {
  key <- paste0("raster:pred:", ref_type, ".tif")

  cache$cached(
    key,
    quote({
      cache_log(
        cache,
        "classification_start",
        msg = paste0("Predicting raster: ", ref_type)
      )

      feats <- build_prediction_stack(landsat_raster)
      pred_fun <- make_rf_terra_predict_fun(mapping_dir)

      pred <- terra::predict(
        object = feats,
        model = model,
        fun = pred_fun,
        na.rm = TRUE
      )

      names(pred) <- "class_value"

      cache_log(
        cache,
        "classification_done",
        msg = paste0("Prediction complete: ", ref_type)
      )

      pred
    })
  )
}
