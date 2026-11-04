#' Prepare napping coordinates and harmonized descriptors datasets
#'
#' Load and harmonize napping coordinates and panelist descriptor tables across
#' all sensory sessions, exporting both tables as TSV files.
#'
#' @include get_session_info.R
#' @include load_session.R
#' @include harmonize_terms_df.R
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:operation_2428 Validation
#' @concept EDAM:data_2048 Report
#' @param input_dir Path to the raw sensory sessions directory.
#' @param dictionary_generic_path Optional path or `data.frame` for generic
#'   dictionary (defaults to built-in [dictionary_generic]).
#' @param dictionary_napping_path Optional path or `data.frame` for napping
#'   dictionary (defaults to built-in [dictionary_napping]).
#' @param dictionary_specific_path Optional path or `data.frame` for specific
#'   dictionary (defaults to built-in [dictionary_specific]).
#' @param sessions Integer vector of session numbers (default `1:8`).
#' @param output_coordinates Output TSV path for napping coordinates.
#' @param output_descriptors Output TSV path for napping descriptors.
#'
#' @return Named list with elements `coordinates` and `descriptors`.
#' @export
#' @examples
#' \dontrun{
#' prepare_napping()
#' }
prepare_napping <- function(
  input_dir = "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory",
  dictionary_generic_path = NULL,
  dictionary_napping_path = NULL,
  dictionary_specific_path = NULL,
  sessions = seq(1, 8),
  output_coordinates = "inst/extdata/napping_coordinates.tsv",
  output_descriptors = "inst/extdata/napping_descriptors.tsv"
) {
  dict_gen <- load_dataset_or_file(
    dictionary_generic_path,
    "dictionary_generic"
  )
  dict_nap <- load_dataset_or_file(
    dictionary_napping_path,
    "dictionary_napping"
  )
  dict_spec <- load_dataset_or_file(
    dictionary_specific_path,
    "dictionary_specific"
  )

  session_infos <- lapply(sessions, get_session_info)

  coord_list <- lapply(
    session_infos,
    load_session,
    input_dir = input_dir,
    tab = "napping_coord"
  )
  coords_df <- as.data.frame(
    data.table::rbindlist(coord_list, use.names = TRUE, fill = TRUE),
    stringsAsFactors = FALSE
  )
  coords_df <- coords_df[
    order(coords_df$session, coords_df$fraction),
    ,
    drop = FALSE
  ]
  .cascade_check_export_dir(dirname(output_coordinates))
  data.table::fwrite(coords_df, file = output_coordinates, sep = "\t")

  words_raw_list <- lapply(
    session_infos,
    load_session,
    input_dir = input_dir,
    tab = "napping_words"
  )
  words_harm_list <- lapply(
    words_raw_list,
    harmonize_terms_df,
    dictionary_generic_path = dict_gen,
    dictionary_napping_path = dict_nap,
    dictionary_specific_path = dict_spec
  )
  words_df <- as.data.frame(
    data.table::rbindlist(words_harm_list, use.names = TRUE, fill = TRUE),
    stringsAsFactors = FALSE
  )

  desc_out <- data.frame(
    fraction = paste0("fraction_", words_df$Produit),
    session = words_df$session,
    jury = words_df$name,
    # taste_original = value,
    taste_original = words_df$value_2,
    taste_intermediate = words_df$taste,
    taste_harmonized = words_df$newValue,
    stringsAsFactors = FALSE
  )
  desc_out <- desc_out[
    order(desc_out$session, desc_out$fraction, desc_out$jury),
    ,
    drop = FALSE
  ]
  .cascade_check_export_dir(dirname(output_descriptors))
  data.table::fwrite(desc_out, file = output_descriptors, sep = "\t")

  return(list(
    coordinates = output_coordinates,
    descriptors = output_descriptors
  ))
}
