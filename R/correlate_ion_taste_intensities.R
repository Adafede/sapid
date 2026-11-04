#' Correlate LC-MS ion intensities with sensory taste intensities
#'
#' Compute rolling-window correlations between LC-MS feature peak areas and
#' panel-consistent sensory taste ratings across contiguous chromatographic
#' fractions, automatically selecting Pearson or Kendall correlation based on
#' Shapiro-Wilk normality tests and applying Benjamini-Hochberg FDR adjustment.
#'
#' @concept EDAM:topic_3172 Metabolomics
#' @concept EDAM:topic_2269 Statistics and probability
#' @concept EDAM:operation_3465 Correlation
#' @concept EDAM:data_2536 Mass spectrometry data
#' @param input_ions Path or `data.frame` of mzmine feature areas across
#'   fractions.
#' @param input_tastes Optional path or `data.frame` of sensory profiles
#'   (defaults to built-in [profiles]).
#' @param output Path to write the resulting TSV correlation table.
#' @param min_jury Minimum number of distinct jurors required for a taste term
#'   (default `2L`).
#' @param min_area_ion Minimum feature peak area threshold (default `1L`).
#' @param imputation_factor Multiplicative factor applied to the minimum
#'   observed intensity when imputing missing values (default `0.5`).
#' @param widths Integer vector of rolling fraction window widths
#'   (default `5:9`).
#'
#' @return Path to the written `output` TSV file.
#' @export
#' @examples
#' \dontrun{
#' correlate_ion_taste_intensities()
#' }
correlate_ion_taste_intensities <- function(
  input_ions = "./data/fractions_mzmine/fractions.csv",
  input_tastes = NULL,
  output = "./data/correlations.tsv",
  # tastes = c(
  #   "BITTER",
  #   "FATTY",
  #   "ACID",
  #   "VOLUME",
  #   "FRESH",
  #   "ASTRINGENT",
  #   "MOUTHFILLING",
  #   "UMAMI",
  #   "SALTY",
  #   "SWEET",
  #   "PUNGENT"
  # ),
  min_jury = 2L,
  min_area_ion = 1L,
  imputation_factor = 0.5,
  widths = 5:9
) {
  min_width <- min(widths)
  # max_width <- widths |>
  #   max()

  # tidytable::slice_max(order_by = value, n = max_width, with_ties = FALSE) |>
  df_ion_intensities <- prepare_ion_intensities_df(
    input_ions = input_ions,
    min_width = min_width,
    min_area_ion = min_area_ion,
    imputation_factor = imputation_factor
  )

  # tidytable::filter(taste %in% tastes) |>
  df_taste_intensities <- prepare_taste_intensities_df(
    input_tastes = input_tastes,
    min_jury = min_jury,
    imputation_factor = imputation_factor
  )

  fractions <- unique(df_ion_intensities$fraction)
  tastes <- unique(df_taste_intensities$id_taste)
  u_ions <- sort(unique(df_ion_intensities$id_ion))

  ion_mat <- matrix(
    NA_real_,
    nrow = length(fractions),
    ncol = length(u_ions),
    dimnames = list(as.character(fractions), as.character(u_ions))
  )
  if (nrow(df_ion_intensities) > 0L) {
    r_idx <- match(df_ion_intensities$fraction, fractions)
    c_idx <- match(df_ion_intensities$id_ion, u_ions)
    ion_mat[cbind(r_idx, c_idx)] <- df_ion_intensities$intensity_ion
  }
  row_has_any_ion <- rowSums(!is.na(ion_mat)) > 0L

  generate_rolling_windows <- function(fractions, window_size) {
    n_win <- length(fractions) - window_size + 1L
    if (n_win < 1L) {
      return(list())
    }
    lapply(seq_len(n_win), function(i) fractions[i:(i + window_size - 1L)])
  }

  pkendall_lut <- function(n) {
    max_q <- as.integer(n * (n - 1L) / 2L)
    dp <- 1
    for (k in 2L:n) {
      new_len <- length(dp) + k - 1L
      cs <- c(0, cumsum(dp))
      idx_hi <- pmin(seq_len(new_len), length(dp))
      idx_lo <- pmax(0L, seq_len(new_len) - k)
      dp <- cs[idx_hi + 1L] - cs[idx_lo + 1L]
    }
    cdf <- cumsum(dp) / sum(dp)
    q_vals <- 0L:max_q
    half <- n * (n - 1L) / 4
    p <- ifelse(q_vals > half, 1 - c(0, cdf)[q_vals + 1L], cdf[q_vals + 1L])
    pmin(2 * p, 1)
  }
  kendall_luts <- new.env(parent = emptyenv())
  get_kendall_lut <- function(n) {
    key <- as.character(n)
    if (!exists(key, envir = kendall_luts, inherits = FALSE)) {
      assign(key, pkendall_lut(n), envir = kendall_luts)
    }
    get(key, envir = kendall_luts, inherits = FALSE)
  }

  tie_stats_vec <- function(v) {
    dup_v <- v[duplicated(v)]
    if (length(dup_v) == 0L) {
      return(c(0, 0, 0, 0))
    }
    xt <- as.numeric(table(dup_v) + 1L)
    xt1 <- xt * (xt - 1)
    c(1, sum(xt1), sum(xt1 * (2 * xt + 5)), sum(xt1 * (xt - 2)))
  }

  win_cache <- new.env(parent = emptyenv())
  get_win_info <- function(f_rows, need_shapiro = FALSE) {
    key <- if (length(f_rows) > 1L && all(diff(f_rows) == 1L)) {
      paste0(f_rows[1L], ":", f_rows[length(f_rows)])
    } else {
      paste(f_rows, collapse = ",")
    }
    if (!exists(key, envir = win_cache, inherits = FALSE)) {
      sub_mat <- ion_mat[f_rows, , drop = FALSE]
      complete_mask <- colSums(is.na(sub_mat)) == 0L
      if (!any(complete_mask)) {
        info <- list(empty = TRUE)
      } else {
        comp_ions <- as.integer(u_ions[complete_mask])
        comp_mat <- sub_mat[, complete_mask, drop = FALSE]
        n <- nrow(comp_mat)
        n_c <- ncol(comp_mat)
        x_const <- colSums(comp_mat != rep(comp_mat[1L, ], each = n)) == 0L
        t_mat <- matrix(0, nrow = 4L, ncol = n_c)
        for (j in seq_len(n_c)) {
          col_v <- comp_mat[, j]
          if (anyDuplicated(col_v) > 0L) {
            t_mat[, j] <- tie_stats_vec(col_v)
          }
        }
        info <- list(
          empty = FALSE,
          n = n,
          comp_ions = comp_ions,
          comp_mat = comp_mat,
          x_const = x_const,
          x_ties = t_mat[1L, ] > 0,
          x_s1 = t_mat[2L, ],
          x_s2 = t_mat[3L, ],
          x_s3 = t_mat[4L, ],
          x_norm = NULL
        )
      }
      assign(key, info, envir = win_cache)
    } else {
      info <- get(key, envir = win_cache, inherits = FALSE)
    }
    if (!info$empty && need_shapiro && is.null(info$x_norm)) {
      n <- info$n
      n_c <- ncol(info$comp_mat)
      xn <- rep(FALSE, n_c)
      if (n >= 3L && n <= 5000L) {
        non_c <- which(!info$x_const)
        for (j in non_c) {
          xn[[j]] <- stats::shapiro.test(info$comp_mat[, j])$p.value > 0.05
        }
      }
      info$x_norm <- xn
      assign(key, info, envir = win_cache)
    }
    info
  }

  fractions_lists <- unlist(
    lapply(widths, function(w) generate_rolling_windows(fractions, w)),
    recursive = FALSE
  )

  # fractions_lists <- list(seq(32, 39))
  # fractions_lists <- fractions_lists[160:170]
  # tastes <- c("BITTER", "VOLUME", "SWEET")

  correlate_intensities <- function(taste) {
    taste_sub <- df_taste_intensities[
      df_taste_intensities$id_taste == taste,
      c("fraction", "intensity_taste"),
      drop = FALSE
    ]
    taste_fracs <- taste_sub$fraction
    taste_vals <- taste_sub$intensity_taste

    results_list <- lapply(fractions_lists, function(fractions_list) {
      in_win <- taste_fracs %in% fractions_list
      f_rows <- match(taste_fracs[in_win], fractions)
      valid_f <- !is.na(f_rows) & row_has_any_ion[f_rows]
      if (!any(valid_f)) {
        warning(
          "No data found for fractions_list: ",
          paste(fractions_list, collapse = ", ")
        )
        return(NULL)
      }
      f_rows <- f_rows[valid_f]
      taste_vec <- taste_vals[in_win][valid_f]
      n <- length(taste_vec)
      if (n < 2L) {
        sub_mat <- ion_mat[f_rows, , drop = FALSE]
        complete_mask <- colSums(is.na(sub_mat)) == 0L
        for (j in seq_len(sum(complete_mask))) {
          warning(
            "Not enough finite observations for correlation. Returning NA."
          )
        }
        return(NULL)
      }

      y_const <- length(unique(taste_vec)) <= 1L
      y_norm <- !y_const &&
        n >= 3L &&
        n <= 5000L &&
        stats::shapiro.test(taste_vec)$p.value > 0.05

      info <- get_win_info(f_rows, need_shapiro = y_norm)
      if (info$empty) {
        return(NULL)
      }

      n_c <- length(info$comp_ions)
      cor_v <- rep(NA_real_, n_c)
      pval_v <- rep(NA_real_, n_c)
      meth_v <- rep("kendall", n_c)

      any_const <- info$x_const | y_const
      if (any(any_const)) {
        warning(
          "All values in x or y are identical. ",
          "Defaulting to Kendall correlation."
        )
      } else if (n < 3L) {
        warning(
          "Sample size too small for Shapiro-Wilk test. ",
          "Defaulting to Kendall correlation."
        )
      } else if (n > 5000L) {
        warning(
          "Sample size too large for Shapiro-Wilk test. ",
          "Defaulting to Kendall correlation."
        )
      }

      if (y_norm) {
        is_p <- info$x_norm & !any_const
        if (any(is_p)) {
          meth_v[is_p] <- "pearson"
          r_p <- as.numeric(stats::cor(
            info$comp_mat[, is_p, drop = FALSE],
            taste_vec,
            method = "pearson"
          ))
          df_p <- n - 2L
          t_p <- sqrt(df_p) * r_p / sqrt(pmax(0, 1 - r_p^2))
          cor_v[is_p] <- r_p
          pval_v[is_p] <- 2 *
            pmin(stats::pt(t_p, df_p), stats::pt(t_p, df_p, lower.tail = FALSE))
        }
      }

      is_k <- (meth_v == "kendall") & !any_const
      if (any(is_k)) {
        r_k <- as.numeric(stats::cor(
          info$comp_mat[, is_k, drop = FALSE],
          taste_vec,
          method = "kendall"
        ))
        cor_v[is_k] <- r_k
        k_idx <- which(is_k)
        y_ts <- tie_stats_vec(taste_vec)
        y_ties <- y_ts[1L] > 0
        has_ties <- info$x_ties[k_idx] | y_ties | (n >= 50L)

        if (!all(has_ties)) {
          ut_sub <- !has_ties
          r_ut <- r_k[ut_sub]
          q_ut <- round((r_ut + 1) * n * (n - 1L) / 4)
          lut <- get_kendall_lut(n)
          pval_v[k_idx[ut_sub]] <- lut[q_ut + 1L]
        }
        if (any(has_ties)) {
          if (n < 50L) {
            warning("cannot compute exact p-value with ties")
          }
          t_sub <- has_ties
          kt_idx <- k_idx[t_sub]
          r_t <- r_k[t_sub]
          t0 <- n * (n - 1) / 2
          t1 <- info$x_s1[kt_idx] / 2
          t2 <- y_ts[2L] / 2
          s_stat <- r_t * sqrt((t0 - t1) * (t0 - t2))
          v0 <- n * (n - 1) * (2 * n + 5)
          vt <- info$x_s2[kt_idx]
          vu <- y_ts[3L]
          v1 <- info$x_s1[kt_idx] * y_ts[2L]
          v2 <- info$x_s3[kt_idx] * y_ts[4L]
          var_s <- (v0 - vt - vu) /
            18 +
            v1 / (2 * n * (n - 1)) +
            v2 / (9 * n * (n - 1) * (n - 2))
          z <- s_stat / sqrt(var_s)
          pval_v[kt_idx] <- 2 *
            pmin(stats::pnorm(z), stats::pnorm(z, lower.tail = FALSE))
        }
      }

      data.frame(
        id_ion = info$comp_ions,
        id_taste = taste,
        fractions = paste(fractions_list, collapse = " "),
        correlation = cor_v,
        p_value = pval_v,
        p_adjusted = stats::p.adjust(pval_v, method = "BH"),
        method = meth_v,
        stringsAsFactors = FALSE
      )
    })

    non_null <- Filter(Negate(is.null), results_list)
    if (length(non_null) == 0L) {
      return(NULL)
    }
    as.data.frame(
      data.table::rbindlist(non_null),
      stringsAsFactors = FALSE
    )
  }

  all_corrs <- Filter(Negate(is.null), lapply(tastes, correlate_intensities))
  if (length(all_corrs) == 0L) {
    correlations <- data.frame(
      id_ion = integer(0),
      id_taste = character(0),
      fractions = character(0),
      correlation = numeric(0),
      p_value = numeric(0),
      p_adjusted = numeric(0),
      method = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    corr_dt <- data.table::rbindlist(all_corrs)
    corr_dt <- unique(corr_dt[!is.na(corr_dt$p_adjusted)])
    correlations <- as.data.frame(corr_dt, stringsAsFactors = FALSE)
  }

  .cascade_check_export_dir(dirname(output))
  data.table::fwrite(correlations, file = output, sep = "\t")
  output
}
