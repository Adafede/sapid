start <- Sys.time()

message("This program prepares Chasselas data.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

if (!requireNamespace("sapid", quietly = TRUE)) {
  stop("Package 'sapid' must be installed to run this script.", call. = FALSE)
}
get("prepare_chasselas", envir = asNamespace("sapid"))()

end <- Sys.time()

message("Script finished in ", format(end - start))
