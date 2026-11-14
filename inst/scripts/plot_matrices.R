start <- Sys.time()

message("This program plots matrices before and after vocabulary curation.")
message("Authors: \n", "AR")

if (!requireNamespace("sapid", quietly = TRUE)) {
  stop("Package 'sapid' must be installed to run this script.", call. = FALSE)
}
sapid::plot_matrices()

end <- Sys.time()

message("Script finished in ", format(end - start))
