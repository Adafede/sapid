start <- Sys.time()

pkgload::load_all()

message("This program prepares chemical profiles")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Prepare chemical profiles
#'
#' @param mzmls_dir Mzmls dir
#' @param peaks_dir_extract Peaks dir extract
#' @param peaks_dir_fractions Peaks dir fractions
#' @param features_path_extract Features path extract
#' @param features_path_fractions Features path fractions
#' @param min_intensity Minimal intensity
#' @param start_extract Start extract
#' @param end_extract End extract
#' @param start_fractions Start fractions
#' @param end_fractions End fractions
#'
#' @return NULL
#'
#' @examples NULL
prepare_chemical_profiles <- function(mzmls_dir = "./data/20210619",
                                      peaks_dir_extract = "./data/interim/peaks/extract",
                                      peaks_dir_fractions = "./data/interim/peaks/fractions",
                                      features_path_extract = "./data/extract_mzmine/extract.csv",
                                      features_path_fractions = "./data/fractions_mzmine/fractions.csv",
                                      min_intensity = 1E5,
                                      start_extract = 5L,
                                      end_extract = 7L,
                                      start_fractions = 12L,
                                      end_fractions = 65L) {
  mzmls <- mzmls_dir |>
    list.files(pattern = ".mzML", full.names = TRUE)

  while (start_extract <= end_extract) {
    message(start_extract)
    cascade::process_compare_peaks(
      export_dir = peaks_dir_extract,
      file = mzmls[start_extract],
      features = features_path_extract,
      min_intensity = min_intensity
    )
    start_extract <- start_extract + 1
    gc()
  }
  while (start_fractions <= end_fractions) {
    message(start_fractions)
    cascade::process_compare_peaks(
      export_dir = peaks_dir_fractions,
      file = mzmls[start_fractions],
      features = features_path_fractions,
      min_intensity = min_intensity
    )
    start_fractions <- start_fractions + 1
    gc()
  }
}

prepare_chemical_profiles()

end <- Sys.time()

message("Script finished in ", format(end - start))
