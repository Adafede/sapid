library(tinytest)

message("Fast function checks")

# get_session_info: deterministic mapping
session_1 <- get_session_info(1)
expect_identical(session_1$cluster, 6)
expect_identical(session_1$date, 20210412)
expect_identical(session_1$product_name, "CHASAVANT")

session_8 <- get_session_info(8)
expect_identical(session_8$cluster, "All")
expect_identical(session_8$date, 20210614)
expect_identical(session_8$product_name, NULL)

# harmonize_terms with in-memory dictionary
mini_dict <- data.frame(
  original = c("MINERAL AMER", "AMER", "ACIDE"),
  translated = c("MINERAL BITTER", "BITTER", "ACID"),
  stringsAsFactors = FALSE
)

word_out <- harmonize_terms(
  dictionary = mini_dict,
  x = "mineral amer et amer",
  mode = "word"
)
expect_identical(word_out, "MINERAL BITTER ET BITTER")

fixed_out <- harmonize_terms(
  dictionary = mini_dict,
  x = "acide-acide",
  mode = "fixed"
)
expect_identical(fixed_out, "ACID-ACID")

# harmonize_terms with dictionary file path
path_dict <- tempfile(fileext = ".tsv")
utils::write.table(
  data.frame(original = "UMAMI", translated = "SAVORY", stringsAsFactors = FALSE),
  file = path_dict,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)
path_out <- harmonize_terms(
  dictionary = path_dict,
  x = "umami",
  mode = "word"
)
expect_identical(path_out, "SAVORY")

# load_consistent_profiles keeps descriptors used by at least min_jury panelists
small_profiles <- data.frame(
  fraction = c("fraction_1", "fraction_1", "fraction_1", "fraction_2", "fraction_2", "fraction_2"),
  session = c("session_1", "session_1", "session_1", "session_1", "session_1", "session_1"),
  jury = c("jury_1", "jury_2", "jury_3", "jury_1", "jury_2", "jury_3"),
  taste_original = c("AMER", "AMER", "ACIDE", "AMER", "RIEN", "RIEN"),
  taste_harmonized = c("BITTER", "BITTER", "ACID", "BITTER", "", NA_character_),
  value = c(1, 2, 3, 4, 0, 0),
  stringsAsFactors = FALSE
)

consistent <- load_consistent_profiles(small_profiles, min_jury = 2L)
expect_true(is.data.frame(consistent))
expect_true(all(consistent$taste == "BITTER"))
expect_identical(nrow(consistent), 3L)
expect_true(all(c("sum", "sum_taste", "group") %in% names(consistent)))
expect_true(all(consistent$sum_taste == 7))

