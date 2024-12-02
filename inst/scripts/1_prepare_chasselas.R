start <- Sys.time()

pkgload::load_all()

message("This program prepares Chasselas data.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

source(file = "paths.R")
source(file = "params.R")

prepare_chasselas()

end <- Sys.time()

message("Script finished in ", format(end - start))
