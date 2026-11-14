start <- Sys.time()

message("This program prepares chemical profiles")
message("Authors: \n", "AR")

if (!requireNamespace("sapid", quietly = TRUE)) {
  stop("Package 'sapid' must be installed to run this script.", call. = FALSE)
}
sapid::prepare_chemical_profiles()

end <- Sys.time()

message("Script finished in ", format(end - start))
