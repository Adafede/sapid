start <- Sys.time()

pkgload::load_all()

message("This program plots descriptors network.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

plot_descriptors_network()

end <- Sys.time()

message("Script finished in ", format(end - start))
