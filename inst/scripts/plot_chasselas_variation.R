start <- Sys.time()

message("This program plots chasselas taste variation.")
message("Authors: \n", "AR")

if (!requireNamespace("sapid", quietly = TRUE)) {
  stop("Package 'sapid' must be installed to run this script.", call. = FALSE)
}
sapid::plot_chasselas_variation()

end <- Sys.time()

message("Script finished in ", format(end - start))
