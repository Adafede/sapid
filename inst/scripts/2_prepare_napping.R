start <- Sys.time()

pkgload::load_all()

message("This program prepares napping data.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

prepare_napping()

end <- Sys.time()

message("Script finished in ", format(end - start))
