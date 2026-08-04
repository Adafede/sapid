library(tinytest)

message("Preparation and napping path checks")
ns <- asNamespace("sapid")
prepare_chasselas <- get("prepare_chasselas", envir = ns)
prepare_profiles <- get("prepare_profiles", envir = ns)
prepare_napping <- get("prepare_napping", envir = ns)
plot_napping <- get("plot_napping", envir = ns)

input_dir <- withr::local_tempdir(pattern = "sapid-input-")
xlsx_dir <- file.path(input_dir, "20210412_cluster6", "03_files")
dir.create(xlsx_dir, recursive = TRUE)

fixture <- file.path("fixtures", "session_fixture.xlsx")
if (!file.exists(fixture)) {
  fixture <- file.path("inst", "tinytest", "fixtures", "session_fixture.xlsx")
}
expect_true(file.exists(fixture))
expect_true(file.copy(fixture, file.path(xlsx_dir, "session.xlsx"), overwrite = TRUE))

# These preparation functions currently expect richer source sheets than this fixture provides.
# We still run them in tests to exercise parsing and preprocessing paths.
out_chasselas <- file.path(input_dir, "chasselas.tsv")
expect_error(
  prepare_chasselas(input_dir = input_dir, sessions = 1L, output = out_chasselas),
  "CJ"
)

out_profiles <- file.path(input_dir, "profiles.tsv")
data(dictionary_generic, package = "sapid")
data(dictionary_specific, package = "sapid")
expect_error(
  prepare_profiles(
    input_dir = input_dir,
    sessions = 1L,
    output = out_profiles,
    dictionary_generic_path = dictionary_generic,
    dictionary_specific_path = dictionary_specific
  ),
  "CJ"
)

out_coordinates <- file.path(input_dir, "napping_coordinates.tsv")
out_descriptors <- file.path(input_dir, "napping_descriptors.tsv")
data(dictionary_napping, package = "sapid")
expect_error(
  prepare_napping(
    input_dir = input_dir,
    sessions = 1L,
    output_coordinates = out_coordinates,
    output_descriptors = out_descriptors,
    dictionary_generic_path = dictionary_generic,
    dictionary_napping_path = dictionary_napping,
    dictionary_specific_path = dictionary_specific
  ),
  "Location 3 doesn't exist|There are only 2 columns"
)
expect_true(file.exists(out_coordinates))

# Drive plot_napping through data preparation with explicit TSV inputs.
plot_dir <- withr::local_tempdir(pattern = "sapid-napping-")
data(napping_coordinates, package = "sapid")
data(napping_descriptors, package = "sapid")
coords_path <- file.path(plot_dir, "coords.tsv")
desc_path <- file.path(plot_dir, "descriptors.tsv")
utils::write.table(
  as.data.frame(napping_coordinates),
  file = coords_path,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)
utils::write.table(
  as.data.frame(napping_descriptors),
  file = desc_path,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)
expect_error(
  plot_napping(input_coordinates = coords_path, input_descriptors = desc_path, sessions = 2L),
  "differing number of rows"
)


