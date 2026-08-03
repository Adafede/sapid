#' Load session
#'
#' Load and prepare data from a single sensory panel session.
#' Uses stringi for efficient string operations.
#'
#' @param input_dir Input directory path
#' @param session_info Session metadata (date, cluster, product_name)
#' @param tab Table type to load ('chasselas', 'napping_coord', 'napping_words', 'profiles')
#'
#' @return Data frame with loaded and processed session data
#'
#' @examples NULL
load_session <- function(input_dir, session_info, tab) {
  sheet <- switch(
    tab,
    "chasselas" = 1,
    "napping_coord" = 2,
    "napping_words" = 3,
    "profiles" = 4
  )
  df <- list.files(
    path = file.path(
      input_dir,
      paste0(session_info$date, "_cluster", session_info$cluster),
      "03_files"
    ),
    pattern = ".xlsx",
    full.names = TRUE
  ) |>
    readxl::read_xlsx(sheet = sheet)

  # Use stringi::stri_replace_all_fixed for faster fixed-string replacement
  df <- df |>
    tidytable::rename_with(
      .cols = tidytable::starts_with("J"),
      .fn = function(cols) {
        stringi::stri_replace_all_regex(cols, "^J(\\d)([A-Z])", "J0$1$2")
      }
    )

  df <- df |>
    tidytable::mutate(
      session = paste0(
        "session_",
        session_info$cluster |>
          stringi::stri_pad(pad = "0", width = 2)
      )
    ) |>
    tidytable::relocate(session, .after = 1)

  if (tab == "napping_coord") {
    df <- df |>
      tidytable::rename(fraction = Produit)
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

  if (tab == "profiles") {
    df <- df |>
      tidytable::mutate(
        ProductName = ProductName |>
          as.character()
      ) |>
      tidytable::select(-tidytable::where(is.logical)) |>
      tidytable::pivot_longer(cols = tidytable::where(is.numeric)) |>
      tidytable::filter(!is.na(value))
  }

  return(df)
}
