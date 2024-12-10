#' Load consistent profiles
#'
#' @param input Input
#' @param min_jury Min jury. Default to 2.
#'
#' @return NULL
#'
#' @examples NULL
load_consistent_profiles <- function(input, min_jury = 2L) {
  profiles <- input |>
    tidytable::fread() |>
    # tidytable::mutate(value = tidytable::if_else(
    #   condition = session == "session_03",
    #   true = value / 500,
    #   false = value
    # )) |>
    # tidytable::mutate(value = tidytable::if_else(
    #   condition = session == "session_03",
    #   true = value / 500,
    #   false = value
    # )) |>
    tidytable::distinct()

  n_panelists <- profiles |>
    tidytable::distinct(fraction, jury) |>
    tidytable::group_by(fraction) |>
    tidytable::count()

  presence <- profiles |>
    tidytable::distinct(fraction, jury) |>
    tidytable::group_by(jury) |>
    tidytable::count(sort = TRUE)

  consistent_descriptors <- profiles |>
    tidytable::distinct(jury, taste = taste_harmonized) |>
    tidytable::group_by(taste) |>
    tidytable::count() |>
    tidytable::filter(n >= min_jury) |>
    tidytable::filter(!is.na(taste)) |>
    tidytable::filter(taste != "")

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
    tidytable::filter(taste %in% consistent_descriptors$taste) |>
    tidytable::group_by(fraction, taste) |>
    tidytable::mutate(median = value |>
      median()) |>
    tidytable::group_by(taste) |>
    tidytable::mutate(sum_taste = value |>
      sum()) |>
    tidytable::arrange(tidytable::desc(sum_taste)) |>
    tidytable::group_by(sum_taste) |>
    tidytable::mutate(group = tidytable::cur_group_id()) |>
    tidytable::ungroup()
}
