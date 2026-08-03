# Load all sapid datasets from inst/extdata and generate .rda files.
# This script keeps dependencies minimal (base R only for data generation).

extdata_dir <- file.path("inst", "extdata")
data_dir <- "data"

load_tsv <- function(filename, strings_as_factors = FALSE) {
  path <- file.path(extdata_dir, filename)
  if (!file.exists(path)) {
    stop("Missing input file: ", path, call. = FALSE)
  }

  utils::read.delim(
    file = path,
    sep = "\t",
    header = TRUE,
    quote = "\"",
    stringsAsFactors = strings_as_factors,
    check.names = FALSE
  )
}

save_dataset <- function(name, object) {
  if (!is.data.frame(object)) {
    stop("Dataset is not a data.frame: ", name, call. = FALSE)
  }

  assign(name, object, envir = environment())
  save(
    list = name,
    file = file.path(data_dir, paste0(name, ".rda")),
  )
}

validate_saved_dataset <- function(name) {
  env <- new.env(parent = emptyenv())
  load(file.path(data_dir, paste0(name, ".rda")), envir = env)
  object <- get(name, envir = env, inherits = FALSE)

  if (!is.data.frame(object)) {
    stop("Saved .rda does not contain a data.frame: ", name, call. = FALSE)
  }

  # Optional check: downstream tidytable workflows can wrap the object when needed.
  if (requireNamespace("tidytable", quietly = TRUE)) {
    tt <- tidytable::as_tidytable(object)
    if (!inherits(tt, "tidytable")) {
      stop("Failed tidytable conversion for dataset: ", name, call. = FALSE)
    }
  }
}

# 1. Chasselas sensory panel data
chasselas <- load_tsv("chasselas.tsv")
chasselas$date <- as.POSIXct(
  chasselas$date,
  format = "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
)

# 2. Concentration AFC test data
concentration_afc <- load_tsv("concentration_afc.tsv")
concentration_afc$afc_correct <- as.integer(concentration_afc$afc_correct)
concentration_afc$afc_total <- as.integer(concentration_afc$afc_total)

# 3. Main sensory profiles
profiles <- load_tsv("profiles.tsv")

# 4. Napping coordinates (sensory map positions)
napping_coordinates <- load_tsv("napping_coordinates.tsv")

# 5. Napping descriptors (sensory map terms)
napping_descriptors <- load_tsv("napping_descriptors.tsv")

# 6. Sample fractionation hierarchy - standardize mass column name and type
fractions <- load_tsv("fractions.tsv")
names(fractions)[names(fractions) == "mass [mg]"] <- "mass"
mass_first_value <- sub(";.*$", "", as.character(fractions$mass))
fractions$mass <- as.numeric(mass_first_value)

# 7. Sample grouping/clustering
groups <- load_tsv("groups.tsv")

# 8. Generic taste dictionary
dictionary_generic <- load_tsv("dictionary_generic.tsv")

# 9. Napping-specific dictionary
dictionary_napping <- load_tsv("dictionary_napping.tsv")

# 10. Specific taste dictionary
dictionary_specific <- load_tsv("dictionary_specific.tsv")

dataset_names <- c(
  "chasselas",
  "concentration_afc",
  "profiles",
  "napping_coordinates",
  "napping_descriptors",
  "fractions",
  "groups",
  "dictionary_generic",
  "dictionary_napping",
  "dictionary_specific"
)

for (dataset_name in dataset_names) {
  save_dataset(dataset_name, get(dataset_name, inherits = FALSE))
}

for (dataset_name in dataset_names) {
  validate_saved_dataset(dataset_name)
}
