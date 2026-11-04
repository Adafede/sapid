#' Load sensory panel session data
#'
#' Load and normalize a specific worksheet from a single sensory panel session
#' Excel workbook.
#'
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:data_2048 Report
#' @param input_dir Path to the base sensory raw-data directory.
#' @param session_info Named list of session metadata (`date`, `cluster`,
#'   `product_name`) as returned by [get_session_info()].
#' @param tab Character worksheet identifier: `"chasselas"`, `"napping_coord"`,
#'   `"napping_words"`, or `"profiles"`.
#'
#' @return A `data.frame` containing the loaded and standardized session data.
#' @export
#' @examples
#' info <- get_session_info(1)
load_session <- function(input_dir, session_info, tab) {
  sheet <- switch(
    tab,
    "chasselas" = 1L,
    "napping_coord" = 2L,
    "napping_words" = 3L,
    "profiles" = 4L
  )

  xlsx_files <- list.files(
    path = file.path(
      input_dir,
      paste0(session_info$date, "_cluster", session_info$cluster),
      "03_files"
    ),
    pattern = "\\.xlsx$",
    full.names = TRUE
  )
  if (length(xlsx_files) == 0L) {
    stop("No .xlsx file found in session directory.", call. = FALSE)
  }

  df <- read_xlsx_base(xlsx_files[[1L]], sheet = sheet)

  j_cols <- grep("^J", names(df))
  if (length(j_cols) > 0L) {
    names(df)[j_cols] <- gsub(
      "^J(\\d)([A-Z])",
      "J0\\1\\2",
      names(df)[j_cols],
      perl = TRUE
    )
  }

  session_val <- paste0(
    "session_",
    if (is.numeric(session_info$cluster)) {
      sprintf("%02d", as.integer(session_info$cluster))
    } else {
      session_info$cluster
    }
  )
  df$session <- rep(session_val, nrow(df))

  other_cols <- setdiff(names(df), "session")
  col_order <- if (length(other_cols) >= 1L) {
    append(other_cols, "session", after = 1L)
  } else {
    "session"
  }
  df <- df[, col_order, drop = FALSE]

  if (tab == "napping_coord") {
    names(df)[names(df) == "Produit"] <- "fraction"
  } else if (tab == "chasselas") {
    df$ProductName <- ifelse(
      df$ProductName == session_info$product_name,
      "product_1before",
      "product_2after"
    )
  } else if (tab == "profiles") {
    df$ProductName <- as.character(df$ProductName)
    is_logi <- vapply(df, is.logical, logical(1L))
    df <- df[, !is_logi, drop = FALSE]

    is_num <- vapply(df, is.numeric, logical(1L))
    num_cols <- names(df)[is_num]
    id_cols <- names(df)[!is_num]

    long_parts <- lapply(num_cols, function(col_nm) {
      sub_df <- df[, id_cols, drop = FALSE]
      sub_df$name <- rep(col_nm, nrow(df))
      sub_df$value <- df[[col_nm]]
      sub_df[!is.na(sub_df$value), , drop = FALSE]
    })
    df <- do.call(rbind, long_parts)
    rownames(df) <- NULL
  }

  df
}
