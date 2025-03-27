start <- Sys.time()

pkgload::load_all()

message("This program prepares napping data.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Prepare napping
#'
#' @param input_dir Input directory
#' @param dictionary_generic_path Dictionary generic path
#' @param dictionary_napping_path Dictionary napping path
#' @param dictionary_specific_path Dictionary specific path
#' @param sessions Sessions
#' @param output_coordinates Output coordinates
#' @param output_descriptors Output descriptors
#'
#' @return NULL
#'
#' @examples NULL
prepare_napping <- function(
  input_dir = "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory",
  dictionary_generic_path = system.file(
    "extdata",
    "dictionary_generic.tsv",
    package = "sapid"
  ),
  dictionary_napping_path = system.file(
    "extdata",
    "dictionary_napping.tsv",
    package = "sapid"
  ),
  dictionary_specific_path = system.file(
    "extdata",
    "dictionary_specific.tsv",
    package = "sapid"
  ),
  sessions = seq(1, 8),
  output_coordinates = "inst/extdata/napping_coordinates.tsv",
  output_descriptors = "inst/extdata/napping_descriptors.tsv"
) {
  session_infos <- sessions |>
    purrr::map(.f = get_session_info)

  session_infos |>
    purrr::map(
      .f = load_session,
      input_dir = input_dir,
      tab = "napping_coord"
    ) |>
    tidytable::bind_rows() |>
    tidytable::arrange(fraction) |>
    tidytable::arrange(session) |>
    tidytable::fwrite(file = output_coordinates, sep = "\t")

  session_infos |>
    purrr::map(
      .f = load_session,
      input_dir = input_dir,
      tab = "napping_words"
    ) |>
    purrr::map(
      .f = harmonize_terms_df,
      dictionary_generic_path = dictionary_generic_path,
      dictionary_napping_path = dictionary_napping_path,
      dictionary_specific_path = dictionary_specific_path
    ) |>
    tidytable::bind_rows() |>
    tidytable::mutate(fraction = paste0("fraction_", Produit)) |>
    tidytable::mutate(
      value = value |>
        toupper()
    ) |>
    tidytable::select(
      fraction = fraction,
      session = session,
      jury = name,
      # taste_original = value,
      taste_original = value_2,
      taste_intermediate = taste,
      taste_harmonized = newValue
    ) |>
    tidytable::arrange(jury) |>
    tidytable::arrange(fraction) |>
    tidytable::arrange(session) |>
    tidytable::fwrite(file = output_descriptors, sep = "\t")

  return(list(
    coordinates = output_coordinates,
    descriptors = output_descriptors
  ))
}

prepare_napping()

end <- Sys.time()

message("Script finished in ", format(end - start))
