start <- Sys.time()

pkgload::load_all()

message("This program prepares Chasselas data.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Prepare chasselas
#'
#' @include get_session_info.R
#' @include load_session.R
#'
#' @param input_dir Input dir
#' @param sessions Sessions
#' @param output output
#'
#' @return NULL
#'
#' @examples NULL
prepare_chasselas <-
  function(input_dir = "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory",
           sessions = seq(1, 7),
           output = "inst/extdata/chasselas.tsv") {
    sessions |>
      purrr::map(.f = get_session_info) |>
      purrr::map(
        .f = load_session,
        input_dir = input_dir,
        tab = "chasselas"
      ) |>
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
      data.frame() |>
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
      ) |>
      tidytable::arrange(jury) |>
      tidytable::arrange(date) |>
      tidytable::fwrite(file = output, sep = "\t")

    return(output)
  }

prepare_chasselas()

end <- Sys.time()

message("Script finished in ", format(end - start))
