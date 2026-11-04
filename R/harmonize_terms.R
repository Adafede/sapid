#' Harmonize sensory descriptor terms
#'
#' Replace terms in a character vector using a dictionary, ordering matches by
#' decreasing pattern length with case-insensitive normalization.
#'
#' @concept EDAM:operation_2428 Validation
#' @concept EDAM:data_2048 Report
#' @param dictionary Path to a dictionary TSV/CSV file or a `data.frame`
#'   containing `original` and `translated` (or `translated_simple`) columns.
#' @param x Character vector to be harmonized.
#' @param mode Replacement mode: `"word"` for word-boundary matching,
#'   `"substring"` for regex partial matching, or `"fixed"` for literal
#'   matching.
#' @param fallback Logical; if `TRUE`, replace `NA` values with the uppercased
#'   input string.
#'
#' @details
#' For better performance when calling this function repeatedly on many strings,
#' pass a pre-loaded dictionary data frame instead of a file path. The function
#' uses base R replacement helpers for all modes to avoid an extra dependency.
#'
#' @return Character vector with harmonized terms.
#' @export
#' @examples
#' data(dictionary_generic)
#' harmonize_terms(dictionary = dictionary_generic, x = "amer")
harmonize_terms <- function(dictionary, x, mode = "word", fallback = FALSE) {
  # Prepare dictionary: accept data frame or file path
  dict_df <- if (is.character(dictionary)) {
    # Legacy: load from file path
    data.table::fread(dictionary, data.table = FALSE)
  } else {
    # Modern: use pre-loaded data frame
    as.data.frame(dictionary, stringsAsFactors = FALSE)
  }

  orig_chr <- as.character(dict_df$original)
  ord <- order(nchar(orig_chr), decreasing = TRUE)
  prepared_dict <- dict_df[ord, , drop = FALSE]
  patterns <- as.character(prepared_dict$original)

  replacement <- if ("translated_simple" %in% names(prepared_dict)) {
    as.character(prepared_dict$translated_simple)
  } else {
    as.character(prepared_dict$translated)
  }

  # Convert to uppercase for consistent matching
  processed_string <- toupper(x)
  u_str <- unique(processed_string)
  u_out <- u_str

  # Perform replacements using appropriate method
  if (mode == "fixed") {
    # Fast fixed-string replacement for literal matches
    for (i in seq_along(patterns)) {
      u_out <- gsub(
        pattern = patterns[[i]],
        replacement = replacement[[i]],
        x = u_out,
        fixed = TRUE
      )
    }
  } else if (mode == "word") {
    # Word boundary matching (regex)
    word_patterns <- paste0("\\b", patterns, "\\b")
    for (i in seq_along(word_patterns)) {
      u_out <- gsub(
        pattern = word_patterns[[i]],
        replacement = replacement[[i]],
        x = u_out,
        perl = TRUE
      )
    }
  } else {
    # Substring matching (regex without word boundaries)
    for (i in seq_along(patterns)) {
      u_out <- gsub(
        pattern = patterns[[i]],
        replacement = replacement[[i]],
        x = u_out,
        perl = TRUE
      )
    }
  }
  out <- u_out[match(processed_string, u_str)]

  # Handle fallback if requested
  if (fallback) {
    na_idx <- is.na(out)
    out[na_idx] <- processed_string[na_idx]
  }

  out
}
