#' Harmonize terms in a data frame
#'
#' Apply harmonization to multiple taste terms in a data frame using
#' provided dictionaries. Optimized with fastmatch for ID lookups
#' and stringi for fixed-string replacements.
#'
#' @param df Data frame containing taste/descriptor columns
#' @param dictionary_generic_path Path to generic dict or data frame object
#' @param dictionary_napping_path Path to napping dict or data frame object
#' @param dictionary_specific_path Path to specific dict or data frame object
#'
#' @return Data frame with harmonized taste terms
#'
#' @examples NULL
#'
harmonize_terms_df <- function(
  df,
  dictionary_generic_path,
  dictionary_napping_path,
  dictionary_specific_path
) {
  file_text_cleaned <- df |>
    tidytable::pivot_longer(cols = 3:ncol(df)) |>
    tidytable::filter(!is.na(value)) |>
    tidytable::mutate(
      value_2 = value |>
        harmonize_terms(dictionary = dictionary_specific_path)
    ) |>
    tidytable::separate_longer_delim(cols = "value_2", delim = " ") |>
    tidytable::filter(value_2 != "") |>
    tidytable::mutate(
      value_3 = value_2 |>
        harmonize_terms(
          dictionary = dictionary_napping_path,
          mode = "substring",
          fallback = TRUE
        )
    ) |>
    tidytable::separate_wider_delim(
      cols = "value_3",
      delim = "_",
      cols_remove = FALSE
    ) |>
    tidytable::mutate(value_4 = value_31, intensity = value_32) |>
    tidytable::filter(!is.na(value_4)) |>
    tidytable::mutate(
      value_5 = value_4 |>
        harmonize_terms(dictionary = dictionary_generic_path)
    ) |>
    tidytable::mutate(
      newValue = value_5 |>
        harmonize_terms(dictionary = dictionary_specific_path)
    ) |>
    tidytable::mutate(
      intensity = tidytable::if_else(
        condition = intensity == "",
        true = NA_character_,
        false = intensity
      )
    ) |>
    tidytable::mutate(
      taste = tidytable::if_else(
        condition = !is.na(intensity),
        true = stringi::stri_paste(newValue, intensity, sep = "_"),
        false = newValue
      )
    ) |>
    tidytable::filter(!is.na(taste)) |>
    tidytable::relocate(taste, .after = name)

  return(file_text_cleaned)
}
