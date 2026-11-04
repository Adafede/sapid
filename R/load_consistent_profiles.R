#' Load consistent sensory profiles
#'
#' Filter sensory profile ratings to retain only taste descriptors used by at
#' least `min_jury` distinct panelists, and compute per-fraction and per-taste
#' summed intensities.
#'
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:operation_2428 Validation
#' @concept EDAM:data_2048 Report
#' @param input Input `data.frame` or file path to a sensory profiles table.
#' @param min_jury Integer minimum number of distinct jurors required to retain
#'   a harmonized taste descriptor (default `2L`).
#'
#' @details
#' This function filters sensory data to keep only taste descriptors that appear
#' consistently across at least `min_jury` panelists. It removes descriptors
#' that are used by only one panelist (potentially idiosyncratic terms).
#'
#' @return A `data.frame` with consistent sensory profiles and summary columns
#'   `sum`, `sum_taste`, and `group`.
#' @export
#' @examples
#' data(profiles)
#' consistent <- load_consistent_profiles(profiles, min_jury = 2L)
#' head(consistent)
load_consistent_profiles <- function(input, min_jury = 2L) {
  # Load data if file path given, otherwise use data frame
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
