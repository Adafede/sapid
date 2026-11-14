#' Prepare harmonized sensory profiles dataset
#'
#' Load flash profile worksheets across sessions, harmonize French panelist
#' taste terms using specific and generic dictionaries, correct out-of-range
#' scale export artifacts, and write the combined TSV table.
#'
#' @details
#' Out-of-range FIZZ export values (`value > 10`) are rescaled by a factor of
#' 10 *prior* to deduplicating identical rating vectors across repeated export
#' columns (`dedup_key`), and synonymous descriptors rated by the same panelist
#' on the same sample (`ProductName`, `CJ`, `newName`) are aggregated via
#' `max(value, na.rm = TRUE)` rather than arbitrary first-column selection.
#'
#' @include get_session_info.R
#' @include load_session.R
#' @include harmonize_terms.R
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:operation_2428 Validation
#' @concept EDAM:data_2048 Report
#' @param input_dir Path to the raw sensory sessions directory.
#' @param dictionary_generic_path Optional path or `data.frame` for generic
#'   dictionary (defaults to built-in [dictionary_generic]).
#' @param dictionary_specific_path Optional path or `data.frame` for specific
#'   dictionary (defaults to built-in [dictionary_specific]).
#' @param sessions Integer vector of session numbers (default `1:7`).
#' @param output Output TSV file path.
#'
#' @return Output file path (`output`).
#' @export
#' @examples
#' \dontrun{
#' prepare_profiles()
#' }
prepare_profiles <- function(
  input_dir = "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory",
  dictionary_generic_path = NULL,
  dictionary_specific_path = NULL,
  sessions = seq(1, 7),
  output = "inst/extdata/profiles.tsv"
) {
  dict_gen <- load_dataset_or_file(
    dictionary_generic_path,
    "dictionary_generic"
  )
  dict_spec <- load_dataset_or_file(
    dictionary_specific_path,
    "dictionary_specific"
  )

  session_infos <- lapply(sessions, get_session_info)
  session_dfs <- lapply(
    session_infos,
    load_session,
    input_dir = input_dir,
    tab = "profiles"
  )
  df <- as.data.frame(
    data.table::rbindlist(session_dfs, use.names = TRUE, fill = TRUE),
    stringsAsFactors = FALSE
  )
  if (!"CJ" %in% names(df)) {
    stop("Column 'CJ' not found in session data.", call. = FALSE)
  }

  df$ProductName <- as.character(df$ProductName)
  df$name <- toupper(as.character(df$name))
  df <- df[!is.na(df$value), , drop = FALSE]
  df <- df[order(df$CJ), , drop = FALSE]

  cj_idx <- match(df$CJ, sort(unique(df$CJ)))
  df$CJ <- paste0("jury_", sprintf("%02d", cj_idx))
  df <- df[, c("ProductName", "session", "CJ", "name", "value"), drop = FALSE]

  df$name_2 <- harmonize_terms(dictionary = dict_spec, x = df$name)
  tokens <- strsplit(as.character(df$name_2), " ", fixed = TRUE)
  expanded <- df[rep(seq_len(nrow(df)), lengths(tokens)), , drop = FALSE]
  expanded$name_2 <- unlist(tokens, use.names = FALSE)
  expanded$name_21 <- sub("_.*$", "", expanded$name_2)
  expanded$newName <- harmonize_terms(
    dictionary = dict_gen,
    x = expanded$name_21
  )

  ## problem due to FIZZ export
  ## replace by the median of correct values or divide by 10
  pn_key <- paste(expanded$ProductName, expanded$name, sep = "\r")
  med_fallback <- stats::ave(
    expanded$value,
    pn_key,
    FUN = function(v) {
      valid_v <- v[!is.na(v) & v <= 10]
      if (length(valid_v) == 0L) NA_real_ else stats::median(valid_v)
    }
  )
  scaled_val <- expanded$value / 10
  replacement_val <- ifelse(
    !is.na(scaled_val) & scaled_val <= 10,
    scaled_val,
    ifelse(!is.na(med_fallback), med_fallback, scaled_val)
  )
  expanded$value <- ifelse(
    !is.na(expanded$value) & expanded$value > 10,
    replacement_val,
    expanded$value
  )

  term_key <- ifelse(
    is.na(expanded$newName),
    paste0("RAW:", expanded$name),
    paste0("HARM:", expanded$newName)
  )
  dedup_key <- paste(
    expanded$ProductName,
    expanded$CJ,
    term_key,
    sep = "\r"
  )
  expanded$value <- stats::ave(
    expanded$value,
    dedup_key,
    FUN = function(v) {
      if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
    }
  )
  expanded <- expanded[!duplicated(dedup_key), , drop = FALSE]

  ## Due to dilutions
  ## Session 03 was diluted 50 times
  ## Session 07 was concentrated 2 times
  # tidytable::mutate(value = tidytable::if_else(
  #   condition = session == "session_03",
  #   true = 50 * value,
  #   false = value
  # )) |>
  # tidytable::mutate(value = tidytable::if_else(
  #   condition = session == "session_07",
  #   true = 0.5 * value,
  #   false = value
  # )) |>
  out <- unique(data.frame(
    fraction = paste0("fraction_", expanded$ProductName),
    session = expanded$session,
    jury = expanded$CJ,
    taste_original = expanded$name,
    taste_harmonized = expanded$newName,
    value = expanded$value,
    stringsAsFactors = FALSE
  ))
  out <- out[order(out$session, out$fraction, out$jury), , drop = FALSE]

  .cascade_check_export_dir(dirname(output))
  data.table::fwrite(out, file = output, sep = "\t")
  output
}
