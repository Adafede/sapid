#' Prepare profiles
#'
#' @param input_dir Input dir
#' @param output output
#' @param sessions Sessions
#' @param dictionary_generic_path Dictionary generic path
#' @param dictionary_specific_path Dictionary specific path
#'
#' @return NULL
#'
#' @examples NULL
prepare_profiles <-
  function(input_dir = "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory",
           output = "~/git/sapid/inst/extdata/profiles.tsv",
           sessions = seq(1, 7),
           dictionary_generic_path = "inst/extdata/dictionary_generic.tsv",
           dictionary_specific_path = "inst/extdata/dictionary_specific.tsv") {
    sessions |>
      furrr::future_map(.f = get_session_info) |>
      furrr::future_map(
        .f = load_session,
        input_dir = input_dir,
        tab = "profiles"
      ) |>
      tidytable::bind_rows() |>
      tidytable::mutate(ProductName = ProductName |>
        as.character()) |>
      tidytable::group_by(CJ, ProductName) |>
      tidytable::select(-tidytable::where(is.logical)) |>
      tidytable::pivot_longer(cols = tidytable::where(is.numeric)) |>
      tidytable::filter(!is.na(value)) |>
      tidytable::arrange(CJ) |>
      tidytable::group_by(CJ) |>
      tidytable::mutate(CJ = paste0(
        "jury_",
        tidytable::cur_group_id() |>
          stringi::stri_pad(pad = "0", width = 2)
      )) |>
      tidytable::ungroup() |>
      tidytable::select(ProductName, session, CJ, name, value) |>
      tidytable::mutate(name_2 = harmonize_terms(x = name, dictionary = dictionary_specific_path)) |>
      tidytable::separate_longer_delim(cols = "name_2", delim = " ") |>
      tidytable::separate_wider_delim(cols = "name_2", delim = "_") |>
      tidytable::mutate(newName = harmonize_terms(x = name_21, dictionary = dictionary_generic_path)) |>
      tidytable::distinct(ProductName, CJ, newName, .keep_all = TRUE) |>
      tidytable::group_by(ProductName, name) |>
      ## problem due to FIZZ export
      ## replace by the median of correct values or divide by 10
      tidytable::mutate(median = tidytable::coalesce(value / 10, median(value[value <= 10]))) |>
      tidytable::ungroup() |>
      tidytable::mutate(value = tidytable::if_else(
        condition = value > 10,
        true = median,
        false = value
      )) |>
      ## Due to dilutions
      ## Session 03 was diluted 50 times
      ## Session 07 was concentrated 2 times
      tidytable::mutate(value = tidytable::if_else(
        condition = session == "session_03",
        true = 50 * value,
        false = value
      )) |>
      tidytable::mutate(value = tidytable::if_else(
        condition = session == "session_07",
        true = 0.5 * value,
        false = value
      )) |>
      tidytable::select(
        fraction = ProductName,
        session = session,
        jury = CJ,
        taste_original = name,
        taste_harmonized = newName,
        value = value
      ) |>
      tidytable::distinct() |>
      tidytable::arrange(fraction)

    return(output)
  }
