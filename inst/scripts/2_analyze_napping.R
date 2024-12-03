start <- Sys.time()

pkgload::load_all()

message("This program analyzes napping data.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

analyze_napping()

end <- Sys.time()

message("Script finished in ", format(end - start))
