#' Harmonize terms df
#'
#' @param df Df
#' @param dictionary_generic_path Dictionary generic path
#' @param dictionary_napping_path Dictionary napping path
#' @param dictionary_specific_path Dictionary specific path
#'
#' @return NULL
#'
#' @examples NULL
harmonize_terms_df <- function(df, dictionary_generic_path, dictionary_napping_path, dictionary_specific_path) {
  file_text_cleaned <- df |>
    tidytable::pivot_longer(cols = 2:ncol(df)) |>
    tidytable::filter(!is.na(value)) |>
    tidytable::mutate(value_2 = value |>
      harmonize_terms(dictionary = dictionary_specific_path)) |>
    tidytable::mutate(
      value_3 = value_2 |>
        harmonize_terms(
          dictionary = dictionary_napping_path,
          mode = "substring",
          fallback = TRUE
        )
    ) |>
    tidytable::separate_longer_delim(cols = "value_3", delim = " ") |>
    tidytable::separate_wider_delim(cols = "value_3", delim = "_") |>
    tidytable::mutate(value_4 = value_31, intensity = value_32) |>
    tidytable::filter(!is.na(value_4)) |>
    tidytable::mutate(value_5 = value_4 |>
      harmonize_terms(dictionary = dictionary_generic_path)) |>
    tidytable::mutate(newValue = value_5 |>
      harmonize_terms(dictionary = dictionary_specific_path)) |>
    tidytable::mutate(
      intensity = tidytable::if_else(
        condition = intensity == "",
        true = NA_character_,
        false = intensity
      )
    ) |>
    tidytable::mutate(taste = tidytable::if_else(
      condition = !is.na(intensity),
      true = paste(newValue, intensity, sep = "_"),
      false = newValue
    )) |>
    tidytable::filter(!is.na(taste)) |>
    tidytable::relocate(taste, .after = name)

  return(file_text_cleaned)
}
