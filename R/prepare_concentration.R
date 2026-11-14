#' Prepare Ascending Forced Choice (AFC) concentration dataset
#'
#' Extract and join intensity ratings, AFC correct/total counts, and qualitative
#' taste attributes from the raw concentration determination Excel workbook.
#'
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:operation_2428 Validation
#' @concept EDAM:data_2048 Report
#' @param input_xlsx Path to the raw concentration `.xlsx` file.
#' @param output Output TSV file path.
#'
#' @return Output file path (`output`).
#' @export
#' @examples
#' \dontrun{
#' prepare_concentration()
#' }
prepare_concentration <- function(
  input_xlsx = paste0(
    "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory/",
    "20210329_raw-extract/03_files/20210329_raw-extract.xlsx"
  ),
  output = "data/processed/concentration_afc.tsv"
) {
  fmt_conc <- function(df) {
    if (!"concentration" %in% names(df)) {
      stop("Column 'concentration' not found in worksheet.", call. = FALSE)
    }
    df$concentration <- format(
      round(as.numeric(df$concentration), digits = 2),
      nsmall = 2
    )
    df
  }

  pipol <- fmt_conc(read_xlsx_base(input_xlsx, sheet = 4L))
  pipol_afc <- fmt_conc(read_xlsx_base(input_xlsx, sheet = 3L))
  cleaned <- fmt_conc(read_xlsx_base(input_xlsx, sheet = 5L))

  merged_pipol <- merge(
    pipol,
    pipol_afc,
    by = intersect(names(pipol), names(pipol_afc)),
    all.x = TRUE
  )
  joined <- unique(
    merged_pipol[,
      c(
        "judge",
        "concentration",
        "intensity",
        "correct.responses",
        "Total.responses"
      ),
      drop = FALSE
    ]
  )

  attr_cols <- grepv("attribut", names(cleaned), fixed = TRUE)
  id_cols <- setdiff(names(cleaned), attr_cols)
  long_parts <- lapply(attr_cols, function(col_nm) {
    sub_df <- cleaned[, id_cols, drop = FALSE]
    sub_df$name <- rep(col_nm, nrow(cleaned))
    sub_df$value <- gsub("_.*$", "", as.character(cleaned[[col_nm]]))
    sub_df[!is.na(sub_df$value), , drop = FALSE]
  })
  cleaned_long <- do.call(rbind, long_parts)

  full_df <- merge(
    cleaned_long,
    joined,
    by = intersect(names(cleaned_long), names(joined))
  )

  out <- data.frame(
    concentration = full_df$concentration,
    jury = full_df$judge,
    taste = full_df$value,
    value = full_df$intensity,
    afc_correct = full_df$correct.responses,
    afc_total = full_df$Total.responses,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$jury), , drop = FALSE]
  jury_idx <- match(out$jury, sort(unique(out$jury)))
  out$jury <- paste0("jury_", sprintf("%02d", jury_idx))

  .cascade_check_export_dir(dirname(output))
  data.table::fwrite(out, file = output, sep = "\t")
  output
}
