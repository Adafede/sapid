#' Prepare chasselas
#'
#' @param input_dir Input dir
#' @param output_dir output_dir
#' @param sessions Sessions
#'
#' @return NULL
#'
#' @examples NULL
prepare_chasselas <-
  function(input_dir = "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory",
           output_dir = "~/switchdrive/SAPERE/03_analysis/04_fractions-sensory/03_output",
           sessions = seq(1, 7)) {
    table <- sessions |>
      furrr::future_map(.f = get_session_info) |>
      furrr::future_map(.f = load_session, tab = "chasselas") |>
      tidytable::bind_rows() |>
      tidytable::relocate(session, .after = ProductName) |>
      tidytable::arrange(CJ) |>
      tidytable::group_by(CJ) |>
      tidytable::mutate(CJ = paste0(
        "jury_",
        tidytable::cur_group_id() |>
          stringi::stri_pad(pad = "0", width = 2)
      )) |>
      tidytable::ungroup() |>
      data.frame()

    table_pivoted <- table |>
      tidytable::group_by(CJ, ProductName) |>
      tidytable::pivot_longer(tidytable::where(is.numeric)) |>
      tidytable::ungroup() |>
      tidytable::filter(!is.na(value)) |>
      tidytable::mutate(name = name |>
        gsub(pattern = "acide.*", replacement = "sourness")) |>
      tidytable::mutate(name = name |>
        gsub(pattern = "amer.*", replacement = "bitterness")) |>
      tidytable::mutate(name = name |>
        gsub(pattern = "sucré.*", replacement = "sweetness")) |>
      tidytable::mutate(name = name |>
        gsub(pattern = "salé.*", replacement = "saltiness")) |>
      tidytable::mutate(name = name |>
        gsub(pattern = "gras.*", replacement = "fatness, volume")) |>
      tidytable::mutate(name = name |>
        gsub(pattern = "équilibre.*", replacement = "balance")) |>
      tidytable::mutate(name = name |>
        gsub(pattern = "fraicheur.*", replacement = "freshness")) |>
      tidytable::mutate(name = name |>
        gsub(pattern = "longueur.*", replacement = "persistency")) |>
      tidytable::mutate(name = name |>
        gsub(pattern = "salivant.*", replacement = "mouthwatering")) |>
      tidytable::rename(
        date = Date,
        jury = CJ,
        product = ProductName,
        session = session,
        taste = name,
        value = value
      )

    tidytable::fwrite(
      x = table_pivoted,
      file = file.path(output_dir, "chasselas_prepared.tsv"),
      sep = "\t"
    )
  }
