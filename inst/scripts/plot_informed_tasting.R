start <- Sys.time()

message(
  "This program plots a comparison between classical and chemically informed analysis."
)
message("Authors: \n", "AR")
message("Contributors: \n", "...")

plot_informed_tasting()

end <- Sys.time()

message("Script finished in ", format(end - start))
