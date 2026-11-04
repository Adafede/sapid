#' Prepare chemical profiles from LC-MS/CAD data
#'
#' Run `cascade::process_compare_peaks()` across raw extract and fraction
#' `.mzML` files to generate CAD-informed and non-informed feature peak tables.
#'
#' @concept EDAM:topic_3172 Metabolomics
#' @concept EDAM:operation_3215 Peak detection
#' @concept EDAM:data_2536 Mass spectrometry data
#' @param mzmls_dir Directory containing `.mzML` files.
#' @param peaks_dir_extract Output directory for extract peak tables.
#' @param peaks_dir_fractions Output directory for fraction peak tables.
#' @param features_path_extract Path to extract mzmine feature CSV.
#' @param features_path_fractions Path to fractions mzmine feature CSV.
#' @param min_intensity Minimum MS intensity threshold (default `1e5`).
#' @param start_extract Integer start index for extract files (default `5L`).
#' @param end_extract Integer end index for extract files (default `7L`).
#' @param start_fractions Integer start index for fraction files
#'   (default `12L`).
#' @param end_fractions Integer end index for fraction files (default `65L`).
#'
#' @return Invisibly returns `NULL`.
#' @export
#' @examples
#' \dontrun{
#' prepare_chemical_profiles()
#' }
prepare_chemical_profiles <- function(
  mzmls_dir = "./data/20210619",
  peaks_dir_extract = "./data/interim/peaks/extract",
  peaks_dir_fractions = "./data/interim/peaks/fractions",
  features_path_extract = "./data/extract_mzmine/extract.csv",
  features_path_fractions = "./data/fractions_mzmine/fractions.csv",
  min_intensity = 1e5,
  start_extract = 5L,
  end_extract = 7L,
  start_fractions = 12L,
  end_fractions = 65L
) {
  mzmls <- list.files(mzmls_dir, pattern = "\\.mzML$", full.names = TRUE)

  if (start_extract <= end_extract) {
    for (idx in seq.int(start_extract, end_extract)) {
      message(idx)
      cascade::process_compare_peaks(
        export_dir = peaks_dir_extract,
        file = mzmls[idx],
        features = features_path_extract,
        min_intensity = min_intensity
      )
      gc()
    }
  }

  if (start_fractions <= end_fractions) {
    for (idx in seq.int(start_fractions, end_fractions)) {
      message(idx)
      cascade::process_compare_peaks(
        export_dir = peaks_dir_fractions,
        file = mzmls[idx],
        features = features_path_fractions,
        min_intensity = min_intensity
      )
      gc()
    }
  }

  invisible(NULL)
}
