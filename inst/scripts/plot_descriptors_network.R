start <- Sys.time()

message("This program plots descriptors network.")
message("Authors: \n", "AR")

if (!requireNamespace("sapid", quietly = TRUE)) {
  stop("Package 'sapid' must be installed to run this script.", call. = FALSE)
}
sapid::plot_descriptors_network()

end <- Sys.time()

message("Script finished in ", format(end - start))
