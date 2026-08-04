start <- Sys.time()

message("This program plots matrices before and after vocabulary curation.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

if (!requireNamespace("sapid", quietly = TRUE)) {
  stop("Package 'sapid' must be installed to run this script.", call. = FALSE)
}
get("plot_matrices", envir = asNamespace("sapid"))()

end <- Sys.time()

message("Script finished in ", format(end - start))
