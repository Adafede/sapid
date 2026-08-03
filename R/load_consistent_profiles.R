#' Load consistent profiles
#'
#' Filter profiles to keep only taste descriptors that are consistently
#' used across multiple panelists (minimum jury threshold). Uses fastmatch
#' for efficient ID lookups.
#'
#' @param input Input data frame or file path to profiles table
#' @param min_jury Minimum number of jurors who must use a taste descriptor
#'   for it to be kept. Default: 2.
#'
#' @details
#' This function filters sensory data to keep only taste descriptors that appear
#' consistently across at least `min_jury` panelists. It removes descriptors that
#' are used by only one panelist (potentially idiosyncratic terms).
#'
#' @return Data frame with consistent profiles and grouped statistics
#'
#' @examples
#' \dontrun{
#' data(profiles)
#' consistent <- load_consistent_profiles(profiles, min_jury = 2)
#' nrow(consistent)
#' }
#'
load_consistent_profiles <- function(input, min_jury = 2L) {
  # Load data if file path given, otherwise use data frame
  profiles <- if (is.character(input)) {
    tidytable::fread(input)
  } else {
    {
      input
    } |>
      as.data.frame(stringsAsFactors = FALSE) |>
      tidytable::distinct()
  }

  n_panelists <- profiles |>
    tidytable::distinct(fraction, jury) |>
    tidytable::group_by(fraction) |>
    tidytable::count() |>
    tidytable::ungroup()

  consistent_descriptors <- profiles |>
    tidytable::distinct(jury, taste = taste_harmonized) |>
    tidytable::group_by(taste) |>
    tidytable::count() |>
    tidytable::filter(n >= min_jury) |>
    tidytable::filter(!is.na(taste)) |>
    tidytable::filter(taste != "") |>
    tidytable::ungroup()

  # Use fastmatch::%fin% for faster taste descriptor filtering
  profiles_consistent <- profiles |>
    tidytable::left_join(n_panelists) |>
    tidytable::select(
      fraction,
      session,
      jury,
      taste_original,
      taste_harmonized,
      value
    ) |>
    tidytable::rename(taste = taste_harmonized) |>
    tidytable::filter(fastmatch::`%fin%`(
      taste,
      consistent_descriptors$taste
    )) |>
    tidytable::group_by(fraction, taste) |>
    tidytable::mutate(
      sum = value |>
        sum(na.rm = TRUE)
    ) |>
    tidytable::group_by(taste) |>
    tidytable::mutate(
      sum_taste = value |>
        sum(na.rm = TRUE)
    ) |>
    tidytable::arrange(tidytable::desc(sum_taste)) |>
    tidytable::group_by(sum_taste) |>
    tidytable::mutate(group = tidytable::cur_group_id()) |>
    tidytable::ungroup()
}
