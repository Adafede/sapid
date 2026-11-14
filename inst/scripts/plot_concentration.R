start <- Sys.time()

message("This program plots concentration evaluation.")
message("Authors: \n", "AR")

if (!requireNamespace("sapid", quietly = TRUE)) {
  stop("Package 'sapid' must be installed to run this script.", call. = FALSE)
}
sapid::plot_concentration()

end <- Sys.time()

message("Script finished in ", format(end - start))
