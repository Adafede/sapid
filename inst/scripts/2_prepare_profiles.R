start <- Sys.time()

pkgload::load_all()

message("This program prepares flash profiles results.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

prepare_profiles()

end <- Sys.time()

message("Script finished in ", format(end - start))
