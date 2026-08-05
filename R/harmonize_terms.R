#' Clean terms
#'
#' Replace terms in a given string using a dictionary, with intelligent matching
#' and case-insensitive handling.
#'
#' @param dictionary Path to the dictionary file, a data frame containing
#'   replacement terms, or NULL to use cached environment dictionaries
#' @param x Character string to be cleaned
#' @param mode Replacement mode: 'word' for word boundary matching,
#'   'substring' for partial matching, 'fixed' for fast fixed-string matching
#' @param fallback Logical, whether to fall back to original string if
#'   no replacements occur
#'
#' @details
#' For better performance when calling this function repeatedly on many strings,
#' pass a pre-loaded dictionary data frame instead of a file path. The function
#' uses base R replacement helpers for all modes to avoid an extra dependency.
#'
#' @return Character string with terms replaced
#'
#' @examples
#' \dontrun{
#' # Using data frame dictionary (faster)
#' data(dictionary_generic)
#' harmonize_terms(
#'   dictionary = dictionary_generic,
#'   x = "Some text with specific terms"
#' )
#'
#' # Using file path (legacy, slower)
#' harmonize_terms(
#'   dictionary = "path/to/dictionary.csv",
#'   x = "Some text with specific terms"
#' )
#' }
#'
harmonize_terms <- function(dictionary, x, mode = "word", fallback = FALSE) {
  # Prepare dictionary: accept data frame or file path
  prepared_dict <- if (is.character(dictionary)) {
    # Legacy: load from file path
    tidytable::fread(dictionary) |>
      tidytable::mutate(
        n = original |>
          as.character() |>
          nchar()
      ) |>
      tidytable::arrange(tidytable::desc(n))
  } else {
    # Modern: use pre-loaded data frame (already tibble, just ensure sorted by length)
    dictionary |>
      as.data.frame(stringsAsFactors = FALSE) |>
      tidytable::mutate(
        n = original |>
          as.character() |>
          nchar()
      ) |>
      tidytable::arrange(tidytable::desc(n))
  }

  replacement <- if ("translated_simple" %in% names(prepared_dict)) {
    prepared_dict$translated_simple
  } else {
    prepared_dict$translated
  }

  # Convert to uppercase for consistent matching
  processed_string <- toupper(x)

  # Perform replacements using appropriate method
  replaced <- if (mode == "fixed") {
    # Fast fixed-string replacement for literal matches
    out <- processed_string
    for (i in seq_along(prepared_dict$original)) {
      out <- gsub(
        pattern = prepared_dict$original[[i]],
        replacement = replacement[[i]],
        x = out,
        fixed = TRUE
      )
    }
    out
  } else if (mode == "word") {
    # Word boundary matching (regex)
    pattern <- paste0("\\b", prepared_dict$original, "\\b")
    out <- processed_string
    for (i in seq_along(pattern)) {
      out <- gsub(
        pattern = pattern[[i]],
        replacement = replacement[[i]],
        x = out,
        perl = TRUE
      )
    }
    out
  } else {
    # Substring matching (regex without word boundaries)
    out <- processed_string
    for (i in seq_along(prepared_dict$original)) {
      out <- gsub(
        pattern = prepared_dict$original[[i]],
        replacement = replacement[[i]],
        x = out,
        perl = TRUE
      )
    }
    out
  }

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
