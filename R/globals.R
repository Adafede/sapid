utils::globalVariables(
  c(
    ".data",
    "chasselas",
    "cluster",
    "col",
    "color",
    "conc_num",
    "concentration",
    "concentration_afc",
    "correct_percent",
    "correlation",
    "delta",
    "dictionary_generic",
    "dictionary_napping",
    "dictionary_specific",
    "Dim1",
    "Dim2",
    "feature_id",
    "fraction",
    "fractions",
    "group",
    "groups",
    "id",
    "id_taste",
    "ids",
    "intensity",
    "intensity_ion",
    "intensity_new",
    "intensity_taste",
    "jury",
    "label",
    "m",
    "median",
    "n",
    "name",
    "napping_coordinates",
    "napping_descriptors",
    "NMRExperiment",
    "profiles",
    "rowname",
    "sample",
    "sample_short",
    "session",
    "size",
    "stars",
    "taste",
    "time",
    "value",
    "values",
    "word",
    "x",
    "x_from",
    "x_to",
    "xend",
    "xmax",
    "xmin",
    "y",
    "y_from",
    "y_to",
    "yend",
    "ymax",
    "ymin"
  )
)

.cascade_check_export_dir <- function(...) {
  utils::getFromNamespace("check_export_dir", "cascade")(...)
}
.cascade_keep_best_candidates <- function(df, ...) {
  if (
    is.data.frame(df) &&
      !"candidate_structure_inchikey_connectivity_layer" %in% names(df) &&
      "candidate_structure_inchikey_no_stereo" %in% names(df)
  ) {
    df$candidate_structure_inchikey_connectivity_layer <- df$candidate_structure_inchikey_no_stereo
  }
  utils::getFromNamespace("keep_best_candidates", "cascade")(df, ...)
}
.cascade_make_confident <- function(...) {
  utils::getFromNamespace("make_confident", "cascade")(...)
}
.cascade_prepare_comparison <- function(...) {
  utils::getFromNamespace("prepare_comparison", "cascade")(...)
}
.cascade_make_other <- function(...) {
  utils::getFromNamespace("make_other", "cascade")(...)
}
.cascade_no_other <- function(...) {
  utils::getFromNamespace("no_other", "cascade")(...)
}
.cascade_prepare_hierarchy <- function(...) {
  utils::getFromNamespace("prepare_hierarchy", "cascade")(...)
}
.cascade_prepare_plot <- function(...) {
  utils::getFromNamespace("prepare_plot", "cascade")(...)
}
.cascade_preprocess_chromatograms <- function(...) {
  utils::getFromNamespace("preprocess_chromatograms", "cascade")(...)
}
.cascade_load_chromatograms <- function(...) {
  utils::getFromNamespace("load_chromatograms", "cascade")(...)
}
.cascade_load_name <- function(...) {
  utils::getFromNamespace("load_name", "cascade")(...)
}

fct_rev_base <- function(f) {
  f <- as.factor(f)
  factor(f, levels = rev(levels(f)))
}

fct_reorder_base <- function(f, x, fun = stats::median, desc = FALSE) {
  f <- as.factor(f)
  scores <- tapply(x, f, FUN = fun)
  factor(
    f,
    levels = levels(f)[order(scores, decreasing = desc, na.last = TRUE)]
  )
}

sign_test_pvalue <- function(x, md = 0) {
  diffs <- x[!is.na(x)] - md
  nz <- diffs[diffs != 0]
  if (length(nz) == 0L) {
    return(1)
  }
  stats::binom.test(sum(nz > 0), length(nz), p = 0.5)$p.value
}

unescape_xml <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

read_xlsx_base <- function(path, sheet = 1L) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }
  tmp <- tempfile("xlsx_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = tmp)

  shared <- character(0)
  ss_file <- file.path(tmp, "xl", "sharedStrings.xml")
  if (file.exists(ss_file)) {
    ss_xml <- paste(
      readLines(ss_file, warn = FALSE, encoding = "UTF-8"),
      collapse = ""
    )
    si_matches <- regmatches(
      ss_xml,
      gregexpr("<si>.*?</si>", ss_xml, perl = TRUE)
    )[[1L]]
    shared <- vapply(
      si_matches,
      function(si) {
        t_m <- regmatches(
          si,
          gregexpr("<t[^>]*>(.*?)</t>", si, perl = TRUE)
        )[[1L]]
        unescape_xml(paste(
          sub("^<t[^>]*>(.*)</t>$", "\\1", t_m, perl = TRUE),
          collapse = ""
        ))
      },
      character(1L),
      USE.NAMES = FALSE
    )
  }

  sheet_file <- file.path(
    tmp,
    "xl",
    "worksheets",
    paste0("sheet", as.integer(sheet), ".xml")
  )
  if (!file.exists(sheet_file)) {
    stop("Sheet ", sheet, " not found in ", path, call. = FALSE)
  }
  xml <- paste(
    readLines(sheet_file, warn = FALSE, encoding = "UTF-8"),
    collapse = ""
  )

  rows_xml <- regmatches(
    xml,
    gregexpr("<row[^>]*>.*?</row>", xml, perl = TRUE)
  )[[1L]]
  if (length(rows_xml) == 0L) {
    return(data.frame())
  }

  col_to_int <- function(col_str) {
    chars <- utf8ToInt(col_str) - 64L
    as.integer(sum(chars * (26^rev(seq_along(chars) - 1L))))
  }

  parsed_rows <- lapply(rows_xml, function(r_xml) {
    cells <- regmatches(
      r_xml,
      gregexpr("<c [^>]*?(?:/>|>.*?</c>)", r_xml, perl = TRUE)
    )[[1L]]
    if (length(cells) == 0L) {
      return(list(cols = integer(0), vals = character(0)))
    }
    cols <- integer(length(cells))
    vals <- rep(NA_character_, length(cells))
    for (i in seq_along(cells)) {
      cell <- cells[[i]]
      ref <- sub(".* r=\"([A-Z]+)[0-9]+\".*", "\\1", cell, perl = TRUE)
      cols[[i]] <- col_to_int(ref)
      ctype <- if (grepl(" t=\"([^\"]+)\"", cell, perl = TRUE)) {
        sub(".* t=\"([^\"]+)\".*", "\\1", cell, perl = TRUE)
      } else {
        "n"
      }
      if (ctype == "inlineStr") {
        if (grepl("<t[^>]*>.*?</t>", cell, perl = TRUE)) {
          vals[[i]] <- unescape_xml(
            sub(".*<t[^>]*>(.*?)</t>.*", "\\1", cell, perl = TRUE)
          )
        }
      } else if (grepl("<v>(.*?)</v>", cell, perl = TRUE)) {
        v <- sub(".*<v>(.*?)</v>.*", "\\1", cell, perl = TRUE)
        if (ctype == "s") {
          vals[[i]] <- shared[[as.integer(v) + 1L]]
        } else if (ctype == "b") {
          vals[[i]] <- if (v == "1") "TRUE" else "FALSE"
        } else {
          vals[[i]] <- unescape_xml(v)
        }
      }
    }
    list(cols = cols, vals = vals)
  })

  max_col <- max(unlist(lapply(parsed_rows, `[[`, "cols")))
  mat <- matrix(NA_character_, nrow = length(parsed_rows), ncol = max_col)
  for (r in seq_along(parsed_rows)) {
    pr <- parsed_rows[[r]]
    if (length(pr$cols) > 0L) {
      mat[r, pr$cols] <- pr$vals
    }
  }

  hdr <- mat[1L, ]
  body <- if (nrow(mat) > 1L) {
    mat[-1L, , drop = FALSE]
  } else {
    matrix(character(0), nrow = 0L, ncol = max_col)
  }
  cols_list <- lapply(seq_len(max_col), function(j) {
    col_v <- utils::type.convert(body[, j], as.is = TRUE)
    if (is.integer(col_v)) as.numeric(col_v) else col_v
  })
  names(cols_list) <- hdr
  as.data.frame(cols_list, stringsAsFactors = FALSE, check.names = FALSE)
}

load_dataset_or_file <- function(input, dataset_name) {
  if (is.data.frame(input)) {
    return(as.data.frame(input, stringsAsFactors = FALSE))
  }
  if (
    is.character(input) &&
      length(input) == 1L &&
      nzchar(input) &&
      file.exists(input)
  ) {
    return(data.table::fread(input, data.table = FALSE))
  }
  if (is.null(input) || (is.character(input) && !nzchar(input))) {
    env <- new.env(parent = emptyenv())
    utils::data(list = dataset_name, package = "sapid", envir = env)
    return(as.data.frame(
      get(dataset_name, envir = env),
      stringsAsFactors = FALSE
    ))
  }
  data.table::fread(input, data.table = FALSE)
}

prepare_ion_intensities_df <- function(
  input_ions,
  min_width,
  min_area_ion,
  imputation_factor
) {
  raw_ions <- if (is.data.frame(input_ions)) {
    as.data.frame(input_ions, stringsAsFactors = FALSE)
  } else {
    data.table::fread(input_ions, data.table = FALSE)
  }

  area_cols <- grepv(":area", names(raw_ions), fixed = TRUE)
  if (length(area_cols) == 0L) {
    return(data.frame(
      fraction = integer(0),
      id_ion = integer(0),
      intensity_ion = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  keep_cols <- unique(c(
    intersect(c("id", "rt", "mz"), names(raw_ions)),
    area_cols
  ))
  sub_ions <- unique(raw_ions[, keep_cols, drop = FALSE])

  frac_names <- gsub(
    pattern = "datafile:210619_AR_[0-9]{2}_M_",
    replacement = "fraction_",
    x = gsub(
      pattern = "_01.mzML:area",
      replacement = "",
      x = area_cols,
      fixed = TRUE
    )
  )
  frac_nums <- as.integer(gsub("fraction_", "", frac_names, fixed = TRUE))

  n_ions <- nrow(sub_ions)
  n_fracs <- length(area_cols)
  val_mat <- as.matrix(sub_ions[, area_cols, drop = FALSE])
  storage.mode(val_mat) <- "double"

  keep_mask <- matrix(FALSE, nrow = n_ions, ncol = n_fracs)
  for (i in seq_len(n_ions)) {
    row_v <- val_mat[i, ]
    rle_v <- rle(!is.na(row_v))
    run_counts <- rep(rle_v$lengths * rle_v$values, rle_v$lengths)
    keep_mask[i, ] <- (run_counts >= min_width) &
      !is.na(row_v) &
      (row_v >= min_area_ion)
  }

  which_idx <- which(keep_mask, arr.ind = TRUE)
  if (nrow(which_idx) == 0L) {
    return(data.frame(
      fraction = integer(0),
      id_ion = integer(0),
      intensity_ion = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  vals <- val_mat[keep_mask]
  if (anyNA(vals)) {
    vals[is.na(vals)] <- imputation_factor * min(vals, na.rm = TRUE)
  }

  out <- data.frame(
    fraction = frac_nums[which_idx[, 2L]],
    id_ion = sub_ions$id[which_idx[, 1L]],
    intensity_ion = vals,
    stringsAsFactors = FALSE
  )
  out <- unique(out)
  out[order(out$fraction), , drop = FALSE]
}

prepare_taste_intensities_df <- function(
  input_tastes,
  min_jury,
  imputation_factor
) {
  raw_tastes <- load_dataset_or_file(input_tastes, "profiles")
  prof <- load_consistent_profiles(raw_tastes, min_jury = min_jury)
  if (nrow(prof) == 0L) {
    return(data.frame(
      fraction = integer(0),
      id_taste = character(0),
      intensity_taste = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  val <- as.numeric(prof$value)
  val[val == 0] <- NA_real_
  min_v <- min(val, na.rm = TRUE)
  val[is.na(val)] <- imputation_factor * min_v
  prof$value <- val

  frac_taste_key <- paste(prof$fraction, prof$taste, sep = "\r")
  sum_val <- stats::ave(prof$value, frac_taste_key, FUN = sum)
  out <- unique(data.frame(
    fraction = prof$fraction,
    id_taste = as.character(prof$taste),
    intensity_taste = as.numeric(sum_val),
    stringsAsFactors = FALSE
  ))
  out$fraction <- as.integer(gsub("fraction_", "", out$fraction, fixed = TRUE))
  out[order(out$fraction), , drop = FALSE]
}
