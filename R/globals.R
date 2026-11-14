utils::globalVariables(
  c(
    ".data",
    "chasselas",
    "cluster",
    "col",
    "color",
    "conc_facet",
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
    "weight",
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
    col_conn <- "candidate_structure_inchikey_connectivity_layer"
    df[[col_conn]] <- df$candidate_structure_inchikey_no_stereo
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

pratt_wilcox_pvalue <- function(x, md = 0) {
  diffs <- x[!is.na(x)] - md
  n <- length(diffs)
  if (n == 0L) {
    return(1)
  }
  n0 <- sum(diffs == 0)
  if (n0 == n) {
    return(1)
  }
  nz_idx <- which(diffs != 0)
  abs_nz <- abs(diffs[nz_idx])
  r_nz <- rank(abs_nz, ties.method = "average") + n0
  w_plus <- sum(r_nz[diffs[nz_idx] > 0])
  mu_w <- (n * (n + 1) - n0 * (n0 + 1)) / 4
  tab_ties <- table(abs_nz)
  tie_counts <- as.numeric(tab_ties[tab_ties > 1L])
  tie_corr <- if (length(tie_counts) > 0L) {
    sum(tie_counts * (tie_counts - 1) * (tie_counts + 1)) / 2
  } else {
    0
  }
  var_w <- (n *
    (n + 1) *
    (2 * n + 1) -
    n0 * (n0 + 1) * (2 * n0 + 1) -
    tie_corr) /
    24
  if (var_w <= 0) {
    return(1)
  }
  z <- (abs(w_plus - mu_w) - 0.5) / sqrt(var_w)
  if (z <= 0) {
    return(1)
  }
  2 * stats::pnorm(-z)
}

order_numeric_labels <- function(x) {
  chr <- as.character(x)
  num <- suppressWarnings(as.numeric(chr))
  if (length(chr) > 0L && !anyNA(num)) {
    u_idx <- !duplicated(chr)
    ord_lvls <- chr[u_idx][order(num[u_idx])]
    factor(chr, levels = ord_lvls)
  } else {
    chr
  }
}

fit_afc_psychometric <- function(
  conc,
  k,
  n,
  m = 3L,
  lambda = 0.05,
  n_grid = 200L
) {
  if (length(conc) != length(k) || length(conc) != length(n)) {
    stop("'conc', 'k', and 'n' must have the same length.", call. = FALSE)
  }
  if (!is.numeric(m) || length(m) != 1L || is.na(m) || m < 2) {
    stop("'m' must be a single numeric value >= 2.", call. = FALSE)
  }
  valid <- !is.na(conc) & !is.na(k) & !is.na(n) & (conc > 0) & (n > 0)
  conc <- as.numeric(conc[valid])
  k <- as.numeric(k[valid])
  n <- as.numeric(n[valid])
  if (length(conc) == 0L) {
    stop(
      "At least one valid positive concentration with n > 0 is required.",
      call. = FALSE
    )
  }
  if (any(k < 0 | k > n)) {
    stop("'k' must satisfy 0 <= k <= n.", call. = FALSE)
  }
  gamma <- 1 / as.numeric(m)
  x <- log10(conc)

  neg_ll <- function(par) {
    mu <- par[1L]
    log_slope <- par[2L]
    slope <- exp(log_slope)
    p <- gamma + (1 - gamma) * stats::plogis(slope * (x - mu))
    p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
    -sum(k * log(p) + (n - k) * log(1 - p)) + 0.5 * lambda * (log_slope^2)
  }

  opt <- stats::optim(
    par = c(mean(x), log(2)),
    fn = neg_ll,
    method = "BFGS",
    hessian = TRUE
  )
  mu_hat <- opt$par[1L]
  log_slope_hat <- opt$par[2L]
  slope_hat <- exp(log_slope_hat)

  cov_mat <- tryCatch(
    solve(opt$hessian),
    error = function(e) diag(c(0.05, 0.05))
  )

  x_grid <- seq(min(x), max(x), length.out = as.integer(n_grid))
  eta <- slope_hat * (x_grid - mu_hat)
  # Gradient of eta = exp(log_slope) * (x_grid - mu) w.r.t. (mu, log_slope):
  d_mu <- -slope_hat
  d_ls <- eta
  var_eta <- pmax(
    0,
    (d_mu^2) *
      cov_mat[1L, 1L] +
      2 * d_mu * d_ls * cov_mat[1L, 2L] +
      (d_ls^2) * cov_mat[2L, 2L]
  )
  se_eta <- sqrt(var_eta)

  y_hat <- gamma + (1 - gamma) * stats::plogis(eta)
  y_lo <- gamma + (1 - gamma) * stats::plogis(eta - 1.96 * se_eta)
  y_hi <- gamma + (1 - gamma) * stats::plogis(eta + 1.96 * se_eta)

  ec50 <- 10^mu_hat
  p_target_50 <- (0.5 - gamma) / (1 - gamma)
  c_p50 <- if (p_target_50 > 0 && p_target_50 < 1) {
    10^(mu_hat + stats::qlogis(p_target_50) / slope_hat)
  } else {
    ec50
  }

  list(
    curve = data.frame(
      x = 10^x_grid,
      y = y_hat,
      ymin = y_lo,
      ymax = y_hi,
      stringsAsFactors = FALSE
    ),
    mu = mu_hat,
    slope = slope_hat,
    ec50 = ec50,
    c_p50 = c_p50,
    gamma = gamma,
    p_threshold = (1 + gamma) / 2
  )
}

unescape_xml <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

resolve_xlsx_sheet_file <- function(tmp, sheet, path) {
  wb_file <- file.path(tmp, "xl", "workbook.xml")
  rels_file <- file.path(tmp, "xl", "_rels", "workbook.xml.rels")
  if (file.exists(wb_file) && file.exists(rels_file)) {
    wb_xml <- paste(
      readLines(wb_file, warn = FALSE, encoding = "UTF-8"),
      collapse = ""
    )
    rels_xml <- paste(
      readLines(rels_file, warn = FALSE, encoding = "UTF-8"),
      collapse = ""
    )
    sheet_tags <- regmatches(
      wb_xml,
      gregexpr("<sheet\\b[^>]*/?>", wb_xml, perl = TRUE)
    )[[1L]]
    if (length(sheet_tags) > 0L) {
      s_names <- vapply(
        sheet_tags,
        function(tag) {
          if (grepl("\\bname=\"([^\"]*)\"", tag, perl = TRUE)) {
            unescape_xml(
              sub(".*\\bname=\"([^\"]*)\".*", "\\1", tag, perl = TRUE)
            )
          } else {
            NA_character_
          }
        },
        character(1L),
        USE.NAMES = FALSE
      )
      s_rids <- vapply(
        sheet_tags,
        function(tag) {
          if (grepl("\\br:id=\"([^\"]*)\"", tag, perl = TRUE)) {
            sub(".*\\br:id=\"([^\"]*)\".*", "\\1", tag, perl = TRUE)
          } else {
            NA_character_
          }
        },
        character(1L),
        USE.NAMES = FALSE
      )
      rel_tags <- regmatches(
        rels_xml,
        gregexpr("<Relationship\\b[^>]*/?>", rels_xml, perl = TRUE)
      )[[1L]]
      rel_ids <- vapply(
        rel_tags,
        function(tag) {
          if (grepl("\\bId=\"([^\"]*)\"", tag, perl = TRUE)) {
            sub(".*\\bId=\"([^\"]*)\".*", "\\1", tag, perl = TRUE)
          } else {
            NA_character_
          }
        },
        character(1L),
        USE.NAMES = FALSE
      )
      rel_targets <- vapply(
        rel_tags,
        function(tag) {
          if (grepl("\\bTarget=\"([^\"]*)\"", tag, perl = TRUE)) {
            sub(".*\\bTarget=\"([^\"]*)\".*", "\\1", tag, perl = TRUE)
          } else {
            NA_character_
          }
        },
        character(1L),
        USE.NAMES = FALSE
      )
      idx <- if (is.character(sheet)) {
        match(sheet[1L], s_names)
      } else {
        s_idx <- as.integer(sheet[1L])
        if (!is.na(s_idx) && s_idx >= 1L && s_idx <= length(sheet_tags)) {
          s_idx
        } else {
          NA_integer_
        }
      }
      if (is.na(idx)) {
        stop("Sheet ", sheet, " not found in ", path, call. = FALSE)
      }
      target <- rel_targets[match(s_rids[[idx]], rel_ids)]
      if (is.na(target) || !nzchar(target)) {
        stop("Sheet ", sheet, " not found in ", path, call. = FALSE)
      }
      target <- sub("^/+", "", target)
      if (!grepl("^xl/", target)) {
        target <- paste0("xl/", target)
      }
      sheet_file <- file.path(tmp, target)
      if (!file.exists(sheet_file)) {
        stop("Sheet ", sheet, " not found in ", path, call. = FALSE)
      }
      return(sheet_file)
    }
  }

  if (!is.numeric(sheet)) {
    stop("Sheet ", sheet, " not found in ", path, call. = FALSE)
  }
  sheet_file <- file.path(
    tmp,
    "xl",
    "worksheets",
    paste0("sheet", as.integer(sheet[1L]), ".xml")
  )
  if (!file.exists(sheet_file)) {
    stop("Sheet ", sheet, " not found in ", path, call. = FALSE)
  }
  sheet_file
}

parse_xlsx_date_styles <- function(tmp) {
  styles_file <- file.path(tmp, "xl", "styles.xml")
  if (!file.exists(styles_file)) {
    return(logical(0))
  }
  s_xml <- paste(
    readLines(styles_file, warn = FALSE, encoding = "UTF-8"),
    collapse = ""
  )
  builtin_date_ids <- c(14:22, 27:36, 45:47, 50:58)
  custom_date_ids <- integer(0)

  if (grepl("<numFmts[^>]*>", s_xml, perl = TRUE)) {
    nf_block <- sub(
      ".*<numFmts[^>]*>([\\s\\S]*?)</numFmts>.*",
      "\\1",
      s_xml,
      perl = TRUE
    )
    nf_tags <- regmatches(
      nf_block,
      gregexpr("<numFmt\\b[^>]*/?>", nf_block, perl = TRUE)
    )[[1L]]
    for (tag in nf_tags) {
      if (
        grepl("\\bnumFmtId=\"([0-9]+)\"", tag, perl = TRUE) &&
          grepl("\\bformatCode=\"([^\"]*)\"", tag, perl = TRUE)
      ) {
        nf_id <- as.integer(
          sub(".*\\bnumFmtId=\"([0-9]+)\".*", "\\1", tag, perl = TRUE)
        )
        fmt <- unescape_xml(
          sub(".*\\bformatCode=\"([^\"]*)\".*", "\\1", tag, perl = TRUE)
        )
        clean_fmt <- gsub("\"[^\"]*\"", "", fmt, perl = TRUE)
        clean_fmt <- gsub("\\[[^\\]]*\\]", "", clean_fmt, perl = TRUE)
        if (
          !grepl("#", clean_fmt, fixed = TRUE) &&
            grepl("(y|m|d|h|s)", clean_fmt, ignore.case = TRUE)
        ) {
          custom_date_ids <- c(custom_date_ids, nf_id)
        }
      }
    }
  }

  date_ids <- unique(c(builtin_date_ids, custom_date_ids))
  if (!grepl("<cellXfs[^>]*>", s_xml, perl = TRUE)) {
    return(logical(0))
  }
  xfs_block <- sub(
    ".*<cellXfs[^>]*>([\\s\\S]*?)</cellXfs>.*",
    "\\1",
    s_xml,
    perl = TRUE
  )
  xf_tags <- regmatches(
    xfs_block,
    gregexpr("<xf\\b[^>]*?(?:/>|>[\\s\\S]*?</xf>)", xfs_block, perl = TRUE)
  )[[1L]]
  vapply(
    xf_tags,
    function(tag) {
      if (!grepl("\\bnumFmtId=\"([0-9]+)\"", tag, perl = TRUE)) {
        return(FALSE)
      }
      nf_id <- as.integer(
        sub(".*\\bnumFmtId=\"([0-9]+)\".*", "\\1", tag, perl = TRUE)
      )
      nf_id %in% date_ids
    },
    logical(1L),
    USE.NAMES = FALSE
  )
}

format_excel_serial_date <- function(num_v) {
  if (abs(num_v - round(num_v)) < 1e-6) {
    format(as.Date(round(num_v), origin = "1899-12-30"), "%Y-%m-%d")
  } else {
    format(
      as.POSIXct(num_v * 86400, origin = "1899-12-30", tz = "UTC"),
      "%Y-%m-%d %H:%M:%S"
    )
  }
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
      gregexpr("<si(?:\\s[^>]*)?>[\\s\\S]*?</si>", ss_xml, perl = TRUE)
    )[[1L]]
    shared <- vapply(
      si_matches,
      function(si) {
        si_clean <- gsub("<rPh[^>]*>[\\s\\S]*?</rPh>", "", si, perl = TRUE)
        t_m <- regmatches(
          si_clean,
          gregexpr("<t[^>]*>[\\s\\S]*?</t>", si_clean, perl = TRUE)
        )[[1L]]
        unescape_xml(paste(
          sub("^<t[^>]*>([\\s\\S]*)</t>$", "\\1", t_m, perl = TRUE),
          collapse = ""
        ))
      },
      character(1L),
      USE.NAMES = FALSE
    )
  }

  sheet_file <- resolve_xlsx_sheet_file(tmp, sheet, path)
  date_styles <- parse_xlsx_date_styles(tmp)
  xml <- paste(
    readLines(sheet_file, warn = FALSE, encoding = "UTF-8"),
    collapse = ""
  )

  rows_xml <- regmatches(
    xml,
    gregexpr("<row\\b[^>]*?(?:/>|>[\\s\\S]*?</row>)", xml, perl = TRUE)
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
      gregexpr("<c\\b[^>]*?(?:/>|>[\\s\\S]*?</c>)", r_xml, perl = TRUE)
    )[[1L]]
    if (length(cells) == 0L) {
      return(list(cols = integer(0), vals = character(0)))
    }
    cols <- integer(length(cells))
    vals <- rep(NA_character_, length(cells))
    col_cursor <- 1L
    for (i in seq_along(cells)) {
      cell <- cells[[i]]
      open_tag <- sub("^([^>]+)>.*$", "\\1", cell, perl = TRUE)
      if (grepl("\\br=\"([A-Z]+)[0-9]+\"", open_tag, perl = TRUE)) {
        ref <- sub(".*\\br=\"([A-Z]+)[0-9]+\".*", "\\1", open_tag, perl = TRUE)
        cols[[i]] <- col_to_int(ref)
      } else {
        cols[[i]] <- col_cursor
      }
      col_cursor <- cols[[i]] + 1L

      ctype <- if (grepl("\\bt=\"([^\"]+)\"", open_tag, perl = TRUE)) {
        sub(".*\\bt=\"([^\"]+)\".*", "\\1", open_tag, perl = TRUE)
      } else {
        "n"
      }
      s_idx <- if (grepl("\\bs=\"([0-9]+)\"", open_tag, perl = TRUE)) {
        as.integer(sub(".*\\bs=\"([0-9]+)\".*", "\\1", open_tag, perl = TRUE))
      } else {
        NA_integer_
      }

      if (ctype == "inlineStr") {
        if (grepl("<t[^>]*>[\\s\\S]*?</t>", cell, perl = TRUE)) {
          vals[[i]] <- unescape_xml(
            sub(".*<t[^>]*>([\\s\\S]*?)</t>.*", "\\1", cell, perl = TRUE)
          )
        }
      } else if (grepl("<v>([\\s\\S]*?)</v>", cell, perl = TRUE)) {
        v <- sub(".*<v>([\\s\\S]*?)</v>.*", "\\1", cell, perl = TRUE)
        if (ctype == "s") {
          vals[[i]] <- shared[[as.integer(v) + 1L]]
        } else if (ctype == "b") {
          vals[[i]] <- if (v == "1") "TRUE" else "FALSE"
        } else if (
          ctype == "n" &&
            !is.na(s_idx) &&
            (s_idx + 1L) <= length(date_styles) &&
            isTRUE(date_styles[[s_idx + 1L]])
        ) {
          num_v <- suppressWarnings(as.numeric(v))
          vals[[i]] <- if (!is.na(num_v)) {
            format_excel_serial_date(num_v)
          } else {
            unescape_xml(v)
          }
        } else {
          vals[[i]] <- unescape_xml(v)
        }
      }
    }
    list(cols = cols, vals = vals)
  })

  all_cols <- unlist(lapply(parsed_rows, `[[`, "cols"), use.names = FALSE)
  max_col <- if (length(all_cols) == 0L) 0L else max(all_cols)
  if (max_col == 0L) {
    return(data.frame())
  }
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
  out_df <- as.data.frame(
    cols_list,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out_df) <- hdr
  out_df
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
  imputation_factor = NULL
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

  has_m_tag <- grepl("_M_[0-9]+", area_cols)
  frac_chr <- ifelse(
    has_m_tag,
    sub("^.*_M_([0-9]+).*$", "\\1", area_cols),
    gsub("[^0-9]+", "", area_cols)
  )
  frac_nums <- suppressWarnings(as.integer(frac_chr))

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
  imputation_factor,
  complete_grid = FALSE,
  normalize_panelists = FALSE
) {
  raw_tastes <- load_dataset_or_file(input_tastes, "profiles")
  prof <- load_consistent_profiles(
    raw_tastes,
    min_jury = min_jury,
    normalize_panelists = normalize_panelists
  )
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
  baseline_v <- imputation_factor * min_v
  val[is.na(val)] <- baseline_v
  prof$value <- val

  frac_taste_key <- paste(prof$fraction, prof$taste, sep = "\r")
  sum_val <- stats::ave(prof$value, frac_taste_key, FUN = sum)
  out <- unique(data.frame(
    fraction = as.integer(gsub("fraction_", "", prof$fraction, fixed = TRUE)),
    id_taste = as.character(prof$taste),
    intensity_taste = as.numeric(sum_val),
    stringsAsFactors = FALSE
  ))

  if (complete_grid && nrow(out) > 0L) {
    u_fracs <- sort(unique(out$fraction))
    u_tastes <- unique(out$id_taste)
    if (length(u_fracs) > 1L && length(u_tastes) > 1L) {
      grid_df <- expand.grid(
        fraction = u_fracs,
        id_taste = u_tastes,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
      out <- merge(
        grid_df,
        out,
        by = c("fraction", "id_taste"),
        all.x = TRUE,
        sort = FALSE
      )
      out$intensity_taste[is.na(out$intensity_taste)] <- baseline_v
    }
  }

  out <- out[order(out$fraction), , drop = FALSE]
  rownames(out) <- NULL
  out
}
