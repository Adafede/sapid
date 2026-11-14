#' Load consistent sensory profiles
#'
#' Filter sensory profile ratings to retain only taste descriptors used by at
#' least `min_jury` distinct panelists, optionally applying root-mean-square
#' (RMS) panelist scale normalization, and compute per-fraction and per-taste
#' summed intensities.
#'
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:operation_2428 Validation
#' @concept EDAM:data_2048 Report
#' @param input Input `data.frame` or file path to a sensory profiles table.
#' @param min_jury Integer minimum number of distinct jurors required to retain
#'   a harmonized taste descriptor (default `2L`).
#' @param normalize_panelists Logical; if `TRUE`, apply multiplicative RMS
#'   scaling per juror (default `FALSE`).
#'
#' @details
#' This function filters sensory data to keep only taste descriptors that appear
#' consistently across at least `min_jury` panelists. It removes descriptors
#' that are used by only one panelist (potentially idiosyncratic terms). When
#' `normalize_panelists = TRUE`, each juror's ratings \eqn{y_{ij}} are scaled by
#' \eqn{s_{\text{global}} / s_j}, where
#' \eqn{s_j = \sqrt{\frac{1}{N_j}\sum_i y_{ij}^2}}
#' is the juror's root-mean-square intensity.
#'
#' @return A `data.frame` with consistent sensory profiles and summary columns
#'   `sum`, `sum_taste`, and `group`.
#' @export
#' @examples
#' data(profiles)
#' consistent <- load_consistent_profiles(profiles, min_jury = 2L)
#' head(consistent)
load_consistent_profiles <- function(
  input,
  min_jury = 2L,
  normalize_panelists = FALSE
) {
  profiles_df <- if (is.character(input)) {
    data.table::fread(input, data.table = FALSE)
  } else {
    unique(as.data.frame(input, stringsAsFactors = FALSE))
  }

  keep_cols <- c(
    "fraction",
    "session",
    "jury",
    "taste_original",
    "taste_harmonized",
    "value"
  )
  profiles_df <- profiles_df[, keep_cols, drop = FALSE]
  names(profiles_df)[names(profiles_df) == "taste_harmonized"] <- "taste"

  valid_taste <- !is.na(profiles_df$taste) & nzchar(profiles_df$taste)
  jury_taste <- unique(profiles_df[
    valid_taste,
    c("jury", "taste"),
    drop = FALSE
  ])
  taste_counts <- table(jury_taste$taste)
  consistent_tastes <- names(taste_counts)[taste_counts >= min_jury]

  out <- profiles_df[profiles_df$taste %in% consistent_tastes, , drop = FALSE]
  if (nrow(out) == 0L) {
    out$sum <- numeric(0)
    out$sum_taste <- numeric(0)
    out$group <- integer(0)
    return(out)
  }

  out$value <- as.numeric(out$value)
  if (normalize_panelists) {
    pos_vals <- out$value[!is.na(out$value) & out$value > 0]
    global_rms <- if (length(pos_vals) > 0L) sqrt(mean(pos_vals^2)) else 1
    jury_rms <- stats::ave(
      out$value,
      out$jury,
      FUN = function(v) {
        vp <- v[!is.na(v) & v > 0]
        if (length(vp) == 0L) global_rms else sqrt(mean(vp^2))
      }
    )
    out$value <- out$value * (global_rms / jury_rms)
  }

  frac_taste_key <- paste(out$fraction, out$taste, sep = "\r")
  out$sum <- stats::ave(
    out$value,
    frac_taste_key,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  out$sum_taste <- stats::ave(
    out$value,
    out$taste,
    FUN = function(x) sum(x, na.rm = TRUE)
  )

  out <- out[order(out$sum_taste, decreasing = TRUE), , drop = FALSE]
  out$group <- match(out$sum_taste, unique(out$sum_taste))
  rownames(out) <- NULL
  out
}
