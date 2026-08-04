start <- Sys.time()

message("This program calculates ion/taste intensities correlations.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

if (!requireNamespace("sapid", quietly = TRUE)) {
  stop("Package 'sapid' must be installed to run this script.", call. = FALSE)
}
get("correlate_ion_taste_intensities", envir = asNamespace("sapid"))()

end <- Sys.time()

message("Script finished in ", format(end - start))
