start <- Sys.time()

message("This program prepares Chasselas data.")
message("Authors: \n", "AR")

if (!requireNamespace("sapid", quietly = TRUE)) {
  stop("Package 'sapid' must be installed to run this script.", call. = FALSE)
}
sapid::prepare_chasselas()

end <- Sys.time()

message("Script finished in ", format(end - start))
