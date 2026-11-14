start <- Sys.time()

message("This program prepares flash profiles results.")
message("Authors: \n", "AR")

if (!requireNamespace("sapid", quietly = TRUE)) {
  stop("Package 'sapid' must be installed to run this script.", call. = FALSE)
}
sapid::prepare_profiles()

end <- Sys.time()

message("Script finished in ", format(end - start))
