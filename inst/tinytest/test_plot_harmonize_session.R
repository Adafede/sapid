library(tinytest)

message("Plot, harmonization, and session loader checks")
ns <- asNamespace("sapid")
geom_sigmoid <- get("geom_sigmoid", envir = ns)
stat_sigmoid <- get("StatSigmoid", envir = ns)
harmonize_terms_df <- get("harmonize_terms_df", envir = ns)
load_session <- get("load_session", envir = ns)


# 1. geom_sigmoid builds a layer and StatSigmoid assigns group IDs per panel
layer <- geom_sigmoid(smooth = 5, direction = "y", linewidth = 0.8)
expect_true(inherits(layer, "LayerInstance"))
expect_identical(layer$stat_params$smooth, 5)
expect_identical(layer$stat_params$direction, "y")
expect_identical(layer$stat_params$na.rm, FALSE)

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
  data.frame(x = 1, y = 1, xend = 3, yend = 5),
  list(),
  smooth = 6,
  direction = "x"
)
expect_identical(names(sigmoid_x), c("x", "y"))
expect_identical(nrow(sigmoid_x), 100L)
expect_equal(sigmoid_x$x[1L], 1, tolerance = 1e-12)
expect_equal(sigmoid_x$x[100L], 3, tolerance = 1e-12)
expect_equal(
  sigmoid_x$y[1L],
  stats::plogis(-6) * 4 + 1,
  tolerance = 1e-12
)
expect_equal(
  sigmoid_x$y[100L],
  stats::plogis(6) * 4 + 1,
  tolerance = 1e-12
)

sigmoid_y <- stat_sigmoid$compute_group(
  data.frame(x = 1, y = 1, xend = 3, yend = 5),
  list(),
  smooth = 6,
  direction = "y"
)
expect_identical(names(sigmoid_y), c("y", "x"))
expect_identical(nrow(sigmoid_y), 100L)
expect_equal(sigmoid_y$y[1L], 1, tolerance = 1e-12)
expect_equal(sigmoid_y$y[100L], 5, tolerance = 1e-12)


# 2. harmonize_terms_df: error on <3 cols, empty/NA handling, intensity splitting, column ordering
dictionary_specific <- data.frame(
  original = c("AMER", "AGRUME", "ACIDE", "DOUX", "VIDE"),
  translated = c("BITTER", "CITRUS", "ACID", "SWEET", ""),
  translated_simple = c("BITTER", "CITRUS", "ACID", "SWEET", ""),
  stringsAsFactors = FALSE
)

dictionary_napping <- data.frame(
  original = c("BITTER", "CITRUS", "ACID", "SWEET"),
  translated = c("BITTER_1", "CITRUS_2", "ACID__", "SWEET"),
  stringsAsFactors = FALSE
)

dictionary_generic <- data.frame(
  original = c("BITTER", "CITRUS", "ACID", "SWEET"),
  translated = c("BITTER", "CITRUS", "ACID", "SWEET"),
  translated_simple = c("BITTER", "CITRUS", "ACID", "SWEET"),
  stringsAsFactors = FALSE
)

# Boundary: < 3 columns must error with exact column count message
expect_error(
  harmonize_terms_df(
    df = data.frame(a = 1, b = 2),
    dictionary_generic_path = dictionary_generic,
    dictionary_napping_path = dictionary_napping,
    dictionary_specific_path = dictionary_specific
  ),
  pattern = "There are only 2 columns in df; at least 3 are required\\."
)

# All-NA measure columns return empty data.frame()
expect_identical(
  harmonize_terms_df(
    df = data.frame(
      fraction = "f1",
      session = "s1",
      jury_1 = NA_character_,
      stringsAsFactors = FALSE
    ),
    dictionary_generic_path = dictionary_generic,
    dictionary_napping_path = dictionary_napping,
    dictionary_specific_path = dictionary_specific
  ),
  data.frame()
)

# Tokens that reduce to empty string after specific harmonization return data.frame()
expect_identical(
  harmonize_terms_df(
    df = data.frame(
      fraction = "f1",
      session = "s1",
      jury_1 = "vide",
      stringsAsFactors = FALSE
    ),
    dictionary_generic_path = dictionary_generic,
    dictionary_napping_path = dictionary_napping,
    dictionary_specific_path = dictionary_specific
  ),
  data.frame()
)

# Full pipeline:
# - "amer agrume" -> "BITTER_1", "CITRUS_2" (with intensities "1", "2")
# - "acide" -> napping gives "ACID__" -> parts[[2]] is "" -> intensity becomes NA -> taste is "ACID"
# - "doux" -> napping gives "SWEET" (no underscore) -> intensity is NA -> taste is "SWEET"
input_df <- data.frame(
  fraction = "fraction_1",
  session = "session_1",
  jury_1 = "amer agrume",
  jury_2 = "acide doux",
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
  harmonized$taste,
  c("BITTER_1", "CITRUS_2", "ACID", "SWEET")
)
expect_identical(
  harmonized$intensity,
  c("1", "2", NA_character_, NA_character_)
)


# 3. load_session: branch behavior, J-column regex, character cluster, missing file error
session_info <- list(date = 20210412, cluster = 6, product_name = "CHASAVANT")
input_dir <- tempfile("sapid-load-session-")

# Missing directory / .xlsx file error
expect_error(
  load_session(input_dir, session_info, "chasselas"),
  pattern = "No \\.xlsx file found in session directory\\."
)

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
expect_identical(names(loaded_chasselas)[2L], "session")
expect_true("J01A" %in% names(loaded_chasselas))
expect_identical(
  loaded_chasselas$ProductName,
  c("product_1before", "product_2after")
)
expect_true(all(loaded_chasselas$session == "session_06"))

loaded_napping <- load_session(input_dir, session_info, "napping_coord")
expect_identical(names(loaded_napping)[1:2], c("fraction", "session"))
expect_false("Produit" %in% names(loaded_napping))
expect_true(all(loaded_napping$session == "session_06"))

loaded_words <- load_session(input_dir, session_info, "napping_words")
expect_identical(names(loaded_words)[1:2], c("J01A", "session"))
expect_true(all(loaded_words$session == "session_06"))

loaded_profiles <- load_session(input_dir, session_info, "profiles")
expect_identical(
  names(loaded_profiles),
  c("ProductName", "session", "name", "value")
)
expect_false("drop_me" %in% names(loaded_profiles))
expect_false(anyNA(loaded_profiles$value))
expect_true(all(loaded_profiles$name %in% c("J01A", "J02B")))

# Character cluster (e.g., session 8 with cluster = "All")
session_info_all <- list(date = 20210614, cluster = "All", product_name = NULL)
xlsx_dir_all <- file.path(input_dir, "20210614_clusterAll", "03_files")
dir.create(xlsx_dir_all, recursive = TRUE)
file.copy(fixture, file.path(xlsx_dir_all, "session.xlsx"), overwrite = TRUE)
loaded_all <- load_session(input_dir, session_info_all, "napping_coord")
expect_true(all(loaded_all$session == "session_All"))

# Single-column worksheet named 'session' (other_cols has length 0)
td_s <- tempfile("xlsx_single_")
dir.create(file.path(td_s, "xl", "worksheets"), recursive = TRUE)
writeLines(
  paste0(
    "<worksheet><sheetData>",
    "<row r=\"1\"><c r=\"A1\" t=\"inlineStr\"><is><t>session</t></is></c></row>",
    "<row r=\"2\"><c r=\"A2\" t=\"inlineStr\"><is><t>old</t></is></c></row>",
    "</sheetData></worksheet>"
  ),
  file.path(td_s, "xl", "worksheets", "sheet2.xml")
)
xlsx_single_dir <- file.path(input_dir, "20210419_cluster2", "03_files")
dir.create(xlsx_single_dir, recursive = TRUE)
old_wd <- setwd(td_s)
utils::zip(
  file.path(xlsx_single_dir, "single.xlsx"),
  files = "xl",
  flags = "-rq"
)
setwd(old_wd)
unlink(td_s, recursive = TRUE)
loaded_single <- load_session(
  input_dir,
  list(date = 20210419, cluster = 2, product_name = "Chas avant"),
  "napping_coord"
)
expect_identical(names(loaded_single), "session")
expect_identical(loaded_single$session, "session_02")

unlink(input_dir, recursive = TRUE)
