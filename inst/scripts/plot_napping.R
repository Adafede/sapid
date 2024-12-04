start <- Sys.time()

pkgload::load_all()

message("This program plots napping data.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

plot_napping()

end <- Sys.time()

message("Script finished in ", format(end - start))
