load_session <- function(input_dir, session_info, tab) {
  sheet <- switch(tab,
    "chasselas" = 1,
    "napping_coord" = 2,
    "napping_words" = 3,
    "profile" = 4
  )
  df <- list.files(
    path =
      file.path(
        input_dir,
        paste0(session_info$date, "_cluster", session_info$cluster),
        "03_files"
      ),
    pattern = ".xlsx",
    full.names = TRUE
  ) |>
    readxl::read_xlsx(sheet = sheet)

  if (tab != "napping_coord") {
    df <- df |>
      tidytable::mutate(session = paste0(
        "session_",
        session_info$cluster |>
          stringi::stri_pad(pad = "0", width = 2)
      ))
  }

  if (tab == "chasselas") {
    df <- df |>
      tidytable::mutate(
        ProductName = tidytable::if_else(
          condition = ProductName == session_info$product_name,
          true = "product_1before",
          false = "product_2after"
        )
      )
  }
  return(df)
}
