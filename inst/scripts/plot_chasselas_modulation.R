start <- Sys.time()

pkgload::load_all()

message("This program plots chasselas taste modulation.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

plot_chasselas_modulation()

end <- Sys.time()

message("Script finished in ", format(end - start))
