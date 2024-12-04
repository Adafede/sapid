start <- Sys.time()

pkgload::load_all()

message("This program prepares working concentration determination.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

prepare_concentration()

end <- Sys.time()

message("Script finished in ", format(end - start))
