start <- Sys.time()

message(
  "This program plots a comparison between classical and chemically informed analysis."
)
message("Authors: \n", "AR")

if (!requireNamespace("sapid", quietly = TRUE)) {
  stop("Package 'sapid' must be installed to run this script.", call. = FALSE)
}
sapid::plot_informed_tasting()

end <- Sys.time()

message("Script finished in ", format(end - start))
