#' Clean terms
#'
#' Replace terms in a given string using a dictionary, with intelligent matching
#' and case-insensitive handling.
#'
#' @param dictionary Path to the dictionary file or a data frame containing
#'   replacement terms
#' @param x Character string to be cleaned
#' @param mode Replacement mode: 'word' for word boundary matching,
#'   'substring' for partial matching
#' @param fallback Logical, whether to fall back to original string if
#'   no replacements occur
#'
#' @return Character string with terms replaced
#'
#' @examples
#' \dontrun{
#' harmonize_terms(
#'   dictionary = "path/to/dictionary.csv",
#'   x = "Some text with specific terms"
#' )
#' }
#'
harmonize_terms <- function(dictionary,
                            x,
                            mode = "word",
                            fallback = FALSE) {
  # Prepare dictionary
  prepared_dict <- dictionary |>
    tidytable::fread() |>
    tidytable::mutate(n = original |>
      stringi::stri_length()) |>
    tidytable::arrange(tidytable::desc(n))

  replacement <- if ("translated_simple" %in% names(prepared_dict)) {
    prepared_dict$translated_simple
  } else {
    prepared_dict$translated
  }

  # Prepare pattern based on mode
  pattern <- if (mode == "word") {
    paste0("\\b", prepared_dict$original, "\\b")
  } else {
    prepared_dict$original
  }

  # Convert to uppercase for consistent matching
  processed_string <- toupper(x)

  # Perform replacements
  replaced <- stringi::stri_replace_all_regex(
    str = processed_string,
    pattern = pattern,
    replacement = replacement,
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )

  # Handle fallback if requested
  if (fallback) {
    replaced <- tidytable::if_else(
      condition = !is.na(replaced),
      true = replaced,
      false = processed_string
    )
  }

  return(replaced)
}
