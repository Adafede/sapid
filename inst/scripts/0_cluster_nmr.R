start <- Sys.time()

pkgload::load_all()

message("This program clusters proton NMR spectra.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

cluster_nmr()

end <- Sys.time()

message("Script finished in ", format(end - start))
