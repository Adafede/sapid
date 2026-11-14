#' Harmonize sensory descriptor terms
#'
#' Replace terms in a character vector using a dictionary, ordering matches by
#' decreasing pattern length with case-insensitive normalization and two-pass
#' placeholder protection to prevent cascading overwrites.
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
#' Patterns are sorted in strictly decreasing order of character length so that
#' multi-word expressions (e.g., `"MINERAL AMER"`) are matched before their
#' constituent single-word tokens (e.g., `"AMER"`). Matched spans are first
#' replaced by sentinel tokens (`\x01<idx>\x02`) and expanded in a second pass
#' so that shorter patterns later in the dictionary cannot accidentally match
#' substrings inside already-translated target terms.
#'
#' @return Character vector with harmonized terms.
#' @export
#' @examples
#' data(dictionary_generic)
#' harmonize_terms(dictionary = dictionary_generic, x = "amer")
harmonize_terms <- function(dictionary, x, mode = "word", fallback = FALSE) {
  mode <- match.arg(mode, c("word", "substring", "fixed"))

  # Prepare dictionary: accept data frame or file path
  dict_df <- if (is.character(dictionary)) {
    # Legacy: load from file path
    data.table::fread(dictionary, data.table = FALSE)
  } else {
    # Modern: use pre-loaded data frame
    as.data.frame(dictionary, stringsAsFactors = FALSE)
  }

  valid_rows <- !is.na(dict_df$original) &
    nzchar(as.character(dict_df$original))
  dict_df <- dict_df[valid_rows, , drop = FALSE]

  # Convert to uppercase for consistent matching
  processed_string <- toupper(x)
  if (length(x) == 0L || nrow(dict_df) == 0L) {
    return(processed_string)
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

  u_str <- unique(processed_string)
  u_out <- u_str
  placeholders <- paste0("\x01", seq_along(patterns), "\x02")

  # Pass 1: Replace matched spans with protected placeholders
  if (mode == "fixed") {
    for (i in seq_along(patterns)) {
      u_out <- gsub(
        pattern = patterns[[i]],
        replacement = placeholders[[i]],
        x = u_out,
        fixed = TRUE
      )
    }
  } else if (mode == "word") {
    escaped <- gsub(
      pattern = "([][{}()+*^$|\\\\?.])",
      replacement = "\\\\\\1",
      x = patterns,
      perl = TRUE
    )
    word_patterns <- paste0(
      "(*UCP)(?<=^|[^\\p{L}\\p{N}_])",
      escaped,
      "(?=$|[^\\p{L}\\p{N}_])"
    )
    for (i in seq_along(word_patterns)) {
      u_out <- gsub(
        pattern = word_patterns[[i]],
        replacement = placeholders[[i]],
        x = u_out,
        perl = TRUE
      )
    }
  } else {
    for (i in seq_along(patterns)) {
      u_out <- gsub(
        pattern = patterns[[i]],
        replacement = placeholders[[i]],
        x = u_out,
        perl = TRUE
      )
    }
  }

  # Pass 2: Expand placeholders to target replacements
  for (i in seq_along(patterns)) {
    if (is.na(replacement[[i]])) {
      has_ph <- !is.na(u_out) & grepl(placeholders[[i]], u_out, fixed = TRUE)
      u_out[has_ph] <- NA_character_
    } else {
      u_out <- gsub(
        pattern = placeholders[[i]],
        replacement = replacement[[i]],
        x = u_out,
        fixed = TRUE
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
