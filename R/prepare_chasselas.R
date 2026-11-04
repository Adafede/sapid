#' Prepare Chasselas sensory panel dataset
#'
#' Load and standardize raw Chasselas sensory panel Excel worksheets across
#' sessions, mapping French taste attributes to standardized English labels
#' and writing the combined TSV dataset.
#'
#' @include get_session_info.R
#' @include load_session.R
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:operation_2428 Validation
#' @concept EDAM:data_2048 Report
#' @param input_dir Path to the raw sensory sessions directory.
#' @param sessions Integer vector of session numbers to process (default `1:7`).
#' @param output Output TSV file path.
#'
#' @return Output file path (`output`).
#' @export
#' @examples
#' \dontrun{
#' prepare_chasselas()
#' }
prepare_chasselas <- function(
  input_dir = "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory",
  sessions = seq(1, 7),
  output = "inst/extdata/chasselas.tsv"
) {
  session_infos <- lapply(sessions, get_session_info)
  session_dfs <- lapply(
    session_infos,
    load_session,
    input_dir = input_dir,
    tab = "chasselas"
  )
  df <- as.data.frame(
    data.table::rbindlist(session_dfs, use.names = TRUE, fill = TRUE),
    stringsAsFactors = FALSE
  )
  if (!"CJ" %in% names(df)) {
    stop("Column 'CJ' not found in session data.", call. = FALSE)
  }

  df <- df[order(df$CJ), , drop = FALSE]
  cj_idx <- match(df$CJ, sort(unique(df$CJ)))
  df$CJ <- paste0("jury_", sprintf("%02d", cj_idx))

  is_num <- vapply(df, is.numeric, logical(1L))
  num_cols <- names(df)[is_num]
  id_cols <- names(df)[!is_num]

  long_list <- lapply(num_cols, function(col_nm) {
    sub_df <- df[, id_cols, drop = FALSE]
    sub_df$name <- rep(col_nm, nrow(df))
    sub_df$value <- df[[col_nm]]
    sub_df[!is.na(sub_df$value), , drop = FALSE]
  })
  long_df <- do.call(rbind, long_list)

  taste_pats <- c(
    "acide.*",
    "amer.*",
    "sucr\u00e9.*",
    "sal\u00e9.*",
    "gras.*",
    "\u00e9quilibre.*",
    "fraicheur.*",
    "longueur.*",
    "salivant.*"
  )
  taste_repls <- c(
    "sourness",
    "bitterness",
    "sweetness",
    "saltiness",
    "fatness, volume",
    "balance",
    "freshness",
    "persistency",
    "mouthwatering"
  )
  for (i in seq_along(taste_pats)) {
    long_df$name <- gsub(
      pattern = taste_pats[[i]],
      replacement = taste_repls[[i]],
      x = long_df$name
    )
  }

  out <- data.frame(
    date = long_df$Date,
    jury = long_df$CJ,
    product = long_df$ProductName,
    session = long_df$session,
    taste = long_df$name,
    value = long_df$value,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$date, out$jury), , drop = FALSE]

  .cascade_check_export_dir(dirname(output))
  data.table::fwrite(out, file = output, sep = "\t")
  output
}
