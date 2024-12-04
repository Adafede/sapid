start <- Sys.time()

pkgload::load_all()

message("This program plots working concentration determination.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

plot_concentration()

end <- Sys.time()

message("Script finished in ", format(end - start))
