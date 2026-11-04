#' Harmonize sensory terms across a data frame
#'
#' Apply multi-stage dictionary harmonization (specific, napping, generic) to
#' panelist descriptor columns in a sensory data frame.
#'
#' @concept EDAM:operation_2428 Validation
#' @concept EDAM:data_2048 Report
#' @param df Data frame with metadata in columns 1-2 and descriptor columns
#'   from column 3 onward.
#' @param dictionary_generic_path Path or `data.frame` for generic dictionary.
#' @param dictionary_napping_path Path or `data.frame` for napping dictionary.
#' @param dictionary_specific_path Path or `data.frame` for specific dictionary.
#'
#' @return A `data.frame` with harmonized taste terms in long format.
#' @export
#' @examples
#' data(dictionary_generic)
#' data(dictionary_napping)
#' data(dictionary_specific)
#' df <- data.frame(
#'   Produit = "17",
#'   session = "session_01",
#'   J01 = "amer",
#'   stringsAsFactors = FALSE
#' )
#' harmonize_terms_df(
#'   df,
#'   dictionary_generic,
#'   dictionary_napping,
#'   dictionary_specific
#' )
harmonize_terms_df <- function(
  df,
  dictionary_generic_path,
  dictionary_napping_path,
  dictionary_specific_path
) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (ncol(df) < 3L) {
    stop(
      "There are only ",
      ncol(df),
      " columns in df; at least 3 are required.",
      call. = FALSE
    )
  }

  id_cols <- names(df)[1:2]
  measure_cols <- names(df)[3:ncol(df)]
  n_rows <- nrow(df)

  long_list <- lapply(measure_cols, function(col_nm) {
    res <- df[, id_cols, drop = FALSE]
    res$name <- rep(col_nm, n_rows)
    res$value <- as.character(df[[col_nm]])
    res[!is.na(res$value), , drop = FALSE]
  })
  long_df <- do.call(rbind, long_list)
  if (is.null(long_df) || nrow(long_df) == 0L) {
    return(data.frame())
  }

  long_df$value_2 <- harmonize_terms(
    dictionary = dictionary_specific_path,
    x = long_df$value
  )

  tokens_list <- strsplit(as.character(long_df$value_2), " ", fixed = TRUE)
  lens <- lengths(tokens_list)
  expanded <- long_df[rep(seq_len(nrow(long_df)), lens), , drop = FALSE]
  expanded$value_2 <- unlist(tokens_list, use.names = FALSE)
  expanded <- expanded[
    !is.na(expanded$value_2) & nzchar(expanded$value_2),
    ,
    drop = FALSE
  ]
  if (nrow(expanded) == 0L) {
    return(data.frame())
  }

  expanded$value_3 <- harmonize_terms(
    dictionary = dictionary_napping_path,
    x = expanded$value_2,
    mode = "substring",
    fallback = TRUE
  )

  parts <- strsplit(as.character(expanded$value_3), "_", fixed = TRUE)
  expanded$value_31 <- vapply(
    parts,
    function(p) if (length(p) >= 1L) p[[1L]] else NA_character_,
    character(1L)
  )
  expanded$value_32 <- vapply(
    parts,
    function(p) if (length(p) >= 2L) p[[2L]] else NA_character_,
    character(1L)
  )
  expanded$value_4 <- expanded$value_31
  expanded$intensity <- expanded$value_32
  expanded <- expanded[!is.na(expanded$value_4), , drop = FALSE]

  expanded$value_5 <- harmonize_terms(
    dictionary = dictionary_generic_path,
    x = expanded$value_4
  )
  expanded$newValue <- harmonize_terms(
    dictionary = dictionary_specific_path,
    x = expanded$value_5
  )

  expanded$intensity[
    !is.na(expanded$intensity) & !nzchar(expanded$intensity)
  ] <-
    NA_character_

  expanded$taste <- ifelse(
    !is.na(expanded$intensity),
    paste(expanded$newValue, expanded$intensity, sep = "_"),
    expanded$newValue
  )
  expanded <- expanded[!is.na(expanded$taste), , drop = FALSE]

  name_pos <- which(names(expanded) == "name")[1L]
  other_cols <- setdiff(names(expanded), "taste")
  col_order <- append(other_cols, "taste", after = name_pos)
  rownames(expanded) <- NULL
  expanded[, col_order, drop = FALSE]
}
