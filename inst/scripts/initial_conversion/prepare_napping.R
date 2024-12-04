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
prepare_napping <- function(input_dir = "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory",
                            dictionary_generic_path = system.file("extdata", "dictionary_generic.tsv", package = "sapid"),
                            dictionary_napping_path = system.file("extdata", "dictionary_napping.tsv", package = "sapid"),
                            dictionary_specific_path = system.file("extdata", "dictionary_specific.tsv", package = "sapid"),
                            sessions = seq(1, 8),
                            output_coordinates = system.file("extdata", "napping_coordinates.tsv", package = "sapid"),
                            output_descriptors = system.file("extdata", "napping_descriptors.tsv", package = "sapid")) {
  session_infos <- sessions |>
    furrr::future_map(.f = get_session_info)

  session_infos |>
    furrr::future_map(
      .f = load_session,
      input_dir = input_dir,
      tab = "napping_coord"
    ) |>
    tidytable::bind_rows() |>
    tidytable::arrange(fraction) |>
    tidytable::arrange(session) |>
    tidytable::fwrite(file = output_coordinates, sep = "\t")

  session_infos |>
    furrr::future_map(
      .f = load_session,
      input_dir = input_dir,
      tab = "napping_words"
    ) |>
    furrr::future_map(
      .f = harmonize_terms_df,
      dictionary_generic_path = dictionary_generic_path,
      dictionary_napping_path = dictionary_napping_path,
      dictionary_specific_path = dictionary_specific_path
    ) |>
    tidytable::bind_rows() |>
    tidytable::select(
      fraction = Produit,
      session = session,
      jury = name,
      taste_original = value,
      taste_intermediate = taste,
      taste_harmonized = newValue
    ) |>
    tidytable::arrange(jury) |>
    tidytable::arrange(fraction) |>
    tidytable::arrange(session) |>
    tidytable::fwrite(file = output_descriptors, sep = "\t")

  return(list(coordinates = output_coordinates, descriptors = output_descriptors))
}

prepare_napping()

end <- Sys.time()

message("Script finished in ", format(end - start))
