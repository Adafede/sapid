start <- Sys.time()

pkgload::load_all()

message("This program plots chasselas taste variation.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

plot_chasselas_variation()

end <- Sys.time()

message("Script finished in ", format(end - start))
