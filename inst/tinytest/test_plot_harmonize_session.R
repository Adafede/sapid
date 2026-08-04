library(tinytest)

message("Plot, harmonization, and session loader checks")
ns <- asNamespace("sapid")
geom_sigmoid <- get("geom_sigmoid", envir = ns)
stat_sigmoid <- get("StatSigmoid", envir = ns)
harmonize_terms_df <- get("harmonize_terms_df", envir = ns)
load_session <- get("load_session", envir = ns)


# geom_sigmoid builds a layer and StatSigmoid assigns group IDs per panel
layer <- geom_sigmoid(smooth = 5, direction = "y", linewidth = 0.8)
expect_true(inherits(layer, "LayerInstance"))
expect_identical(layer$stat_params$smooth, 5)
expect_identical(layer$stat_params$direction, "y")

setup_out <- stat_sigmoid$setup_data(
  data.frame(
    PANEL = c(1, 1, 2),
    x = c(1, 2, 3),
    y = c(1, 2, 3),
    xend = c(2, 3, 4),
    yend = c(2, 3, 4)
  ),
  list()
)
expect_true("group" %in% names(setup_out))
expect_identical(setup_out$group, c(1L, 2L, 1L))

sigmoid_x <- stat_sigmoid$compute_group(
  data.frame(x = 1, y = 1, xend = 3, yend = 4),
  list(),
  smooth = 6,
  direction = "x"
)
expect_true(all(c("x", "y") %in% names(sigmoid_x)))
expect_true(nrow(sigmoid_x) > 1)

sigmoid_y <- stat_sigmoid$compute_group(
  data.frame(x = 1, y = 1, xend = 3, yend = 4),
  list(),
  smooth = 6,
  direction = "y"
)
expect_true(all(c("x", "y") %in% names(sigmoid_y)))
expect_true(nrow(sigmoid_y) > 1)


# harmonize_terms_df applies specific -> napping -> generic harmonization chain
input_df <- data.frame(
  fraction = "fraction_1",
  session = "session_1",
  jury_1 = "amer agrume",
  jury_2 = "acide",
  stringsAsFactors = FALSE
)

dictionary_specific <- data.frame(
  original = c("AMER", "AGRUME", "ACIDE"),
  translated = c("BITTER", "CITRUS", "ACID"),
  translated_simple = c("BITTER", "CITRUS", "ACID"),
  stringsAsFactors = FALSE
)

dictionary_napping <- data.frame(
  original = c("BITTER", "CITRUS", "ACID"),
  translated = c("BITTER_1", "CITRUS_2", "ACID_3"),
  stringsAsFactors = FALSE
)

dictionary_generic <- data.frame(
  original = c("BITTER", "CITRUS", "ACID"),
  translated = c("BITTER", "CITRUS", "ACID"),
  translated_simple = c("BITTER", "CITRUS", "ACID"),
  stringsAsFactors = FALSE
)

harmonized <- harmonize_terms_df(
  df = input_df,
  dictionary_generic_path = dictionary_generic,
  dictionary_napping_path = dictionary_napping,
  dictionary_specific_path = dictionary_specific
)
expect_true("taste" %in% names(harmonized))
expect_identical(
  which(names(harmonized) == "taste"),
  which(names(harmonized) == "name") + 1L
)
expect_identical(
  sort(harmonized$taste),
  sort(c("ACID_3", "BITTER_1", "CITRUS_2"))
)


# load_session branch behavior is checked with a real multi-sheet xlsx fixture
session_info <- list(date = 20210412, cluster = 6, product_name = "CHASAVANT")
input_dir <- tempfile("sapid-load-session-")
xlsx_dir <- file.path(input_dir, "20210412_cluster6", "03_files")
dir.create(xlsx_dir, recursive = TRUE)
fixture <- file.path("fixtures", "session_fixture.xlsx")
if (!file.exists(fixture)) {
  fixture <- file.path("inst", "tinytest", "fixtures", "session_fixture.xlsx")
}
expect_true(file.exists(fixture))
expect_true(file.copy(
  fixture,
  file.path(xlsx_dir, "session.xlsx"),
  overwrite = TRUE
))

loaded_chasselas <- load_session(input_dir, session_info, "chasselas")
expect_true("session" %in% names(loaded_chasselas))
expect_true("J01A" %in% names(loaded_chasselas))
expect_identical(
  loaded_chasselas$ProductName,
  c("product_1before", "product_2after")
)
expect_true(all(loaded_chasselas$session == "session_06"))

loaded_napping <- load_session(input_dir, session_info, "napping_coord")
expect_true("fraction" %in% names(loaded_napping))
expect_false("Produit" %in% names(loaded_napping))
expect_true(all(loaded_napping$session == "session_06"))

loaded_profiles <- load_session(input_dir, session_info, "profiles")
expect_true(all(
  c("session", "ProductName", "name", "value") %in% names(loaded_profiles)
))
expect_false("drop_me" %in% names(loaded_profiles))
expect_false(anyNA(loaded_profiles$value))
expect_true(all(loaded_profiles$name %in% c("J01A", "J02B")))
