library(tinytest)

message("Core function checks")
ns <- asNamespace("sapid")
get_session_info <- get("get_session_info", envir = ns)
harmonize_terms <- get("harmonize_terms", envir = ns)
load_consistent_profiles <- get("load_consistent_profiles", envir = ns)


# 1. get_session_info: exhaustive deterministic mapping for sessions 1..8 and out-of-bounds
expected_sessions <- list(
  "1" = list(cluster = 6, date = 20210412, product_name = "CHASAVANT"),
  "2" = list(cluster = 2, date = 20210419, product_name = "Chas avant"),
  "3" = list(cluster = 5, date = 20210426, product_name = "CHASAVANT"),
  "4" = list(cluster = 7, date = 20210510, product_name = "CHASAVANT"),
  "5" = list(cluster = 3, date = 20210517, product_name = "CHASAVANT"),
  "6" = list(cluster = 4, date = 20210531, product_name = "CHASAVANT"),
  "7" = list(cluster = 1, date = 20210607, product_name = "CHASAVANT"),
  "8" = list(cluster = "All", date = 20210614, product_name = NULL)
)

for (s_chr in names(expected_sessions)) {
  s_int <- as.integer(s_chr)
  expect_identical(get_session_info(s_int), expected_sessions[[s_chr]])
  expect_identical(get_session_info(s_chr), expected_sessions[[s_chr]])
}

expect_error(get_session_info(0), pattern = "EXPR must be a length 1 vector")
expect_error(get_session_info(9), pattern = "EXPR must be a length 1 vector")
expect_error(
  get_session_info("invalid"),
  pattern = "EXPR must be a length 1 vector"
)


# 2. harmonize_terms: mode distinguishing, pattern length ordering, translated_simple, fallback
# Note: "AMER" is intentionally placed BEFORE "MINERAL AMER" to verify decreasing nchar ordering
mini_dict <- data.frame(
  original = c("AMER", "MINERAL AMER", "ACIDE", "A+B", "RIEN"),
  translated = c("BITTER", "MIN_BITTER", "ACID", "PLUS", NA_character_),
  stringsAsFactors = FALSE
)

# Pattern length ordering: "mineral amer" must become "MIN_BITTER", not "MINERAL BITTER"
word_out <- harmonize_terms(
  dictionary = mini_dict,
  x = "mineral amer et amer",
  mode = "word"
)
expect_identical(word_out, "MIN_BITTER ET BITTER")

# Distinguish mode = "word" vs mode = "substring" vs mode = "fixed"
expect_identical(
  harmonize_terms(mini_dict, x = "amertume et amer", mode = "word"),
  "AMERTUME ET BITTER"
)
expect_identical(
  harmonize_terms(mini_dict, x = "amertume et amer", mode = "substring"),
  "BITTERTUME ET BITTER"
)

# Distinguish mode = "fixed" vs mode = "substring" with regex metacharacter '+'
expect_identical(
  harmonize_terms(mini_dict, x = "a+b aab", mode = "fixed"),
  "PLUS AAB"
)
expect_identical(
  harmonize_terms(mini_dict, x = "a+b aab", mode = "substring"),
  "A+B PLUS"
)

fixed_out <- harmonize_terms(
  dictionary = mini_dict,
  x = "acide-acide",
  mode = "fixed"
)
expect_identical(fixed_out, "ACID-ACID")

# Distinguish fallback = FALSE vs fallback = TRUE when replacement produces NA
expect_identical(
  harmonize_terms(
    mini_dict,
    x = c("rien", "amer"),
    mode = "word",
    fallback = FALSE
  ),
  c(NA_character_, "BITTER")
)
expect_identical(
  harmonize_terms(
    mini_dict,
    x = c("rien", "amer"),
    mode = "word",
    fallback = TRUE
  ),
  c("RIEN", "BITTER")
)

# Distinguish translated_simple vs translated preference
dict_simple <- data.frame(
  original = "AMER",
  translated = "BITTER_FULL",
  translated_simple = "BITTER_SIMPLE",
  stringsAsFactors = FALSE
)
expect_identical(
  harmonize_terms(dict_simple, x = "amer", mode = "word"),
  "BITTER_SIMPLE"
)

# harmonize_terms with dictionary file path
path_dict <- tempfile(fileext = ".tsv")
utils::write.table(
  data.frame(
    original = "UMAMI",
    translated = "SAVORY",
    stringsAsFactors = FALSE
  ),
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
unlink(path_dict)

# Empty and all-NA inputs in harmonize_terms preserve length and NA semantics
expect_identical(
  harmonize_terms(dict_simple, character(0), mode = "word"),
  character(0)
)
expect_identical(
  harmonize_terms(
    dict_simple,
    c(NA_character_, NA_character_),
    fallback = TRUE
  ),
  c(NA_character_, NA_character_)
)

# Two-pass sentinel placeholder protection: shorter pattern must not overwrite
# substrings inside an already-translated target string
dict_cascade <- data.frame(
  original = c("LONG TERM", "TAR"),
  translated = c("TARGET_A", "BAD_REPLACE"),
  stringsAsFactors = FALSE
)
expect_identical(
  harmonize_terms(dict_cascade, x = "long term and tar", mode = "substring"),
  "TARGET_A AND BAD_REPLACE"
)

# Unicode accented words, leading hyphens, regex metacharacters, and NA/empty dictionary rows
data(dictionary_generic, package = "sapid")
data(dictionary_specific, package = "sapid")
expect_identical(
  harmonize_terms(
    dictionary_generic,
    x = c("amer", "boisé", "salé", "fruité", "acidité"),
    mode = "word"
  ),
  c("BITTER", "WOODY", "SALTY", "FRUITY", "ACID")
)
expect_identical(
  harmonize_terms(
    dictionary_generic,
    x = c("emboisé", "boisé_fort"),
    mode = "word"
  ),
  c("EMBOISÉ", "BOISÉ_FORT")
)
expect_identical(
  harmonize_terms(
    dictionary_specific,
    x = "-un-peu-plus-soutenue",
    mode = "word"
  ),
  ""
)
expect_identical(
  harmonize_terms(
    mini_dict,
    x = c("test a+b end", "test aab end"),
    mode = "word"
  ),
  c("TEST PLUS END", "TEST AAB END")
)
dirty_dict <- data.frame(
  original = c(NA_character_, "", "AMER"),
  translated = c("BAD", "BAD2", "BITTER"),
  stringsAsFactors = FALSE
)
expect_identical(
  harmonize_terms(dirty_dict, x = c("amer", NA_character_), mode = "word"),
  c("BITTER", NA_character_)
)
expect_identical(
  harmonize_terms(
    dirty_dict[1:2, , drop = FALSE],
    x = c("amer", NA_character_),
    mode = "word"
  ),
  c("AMER", NA_character_)
)


# 3. load_consistent_profiles: min_jury boundaries, distinct juror counting, deduplication, ranking
small_profiles <- data.frame(
  fraction = c(
    "fraction_1",
    "fraction_1",
    "fraction_1",
    "fraction_2",
    "fraction_2",
    "fraction_2",
    "fraction_1", # duplicate of row 1 to test deduplication
    "fraction_2" # same juror jury_3 using ACID a second time (still only 1 distinct juror!)
  ),
  session = rep("session_1", 8),
  jury = c(
    "jury_1",
    "jury_2",
    "jury_3",
    "jury_1",
    "jury_2",
    "jury_3",
    "jury_1",
    "jury_3"
  ),
  taste_original = c(
    "AMER",
    "AMER",
    "ACIDE",
    "AMER",
    "SUCRE",
    "RIEN",
    "AMER",
    "ACIDE"
  ),
  taste_harmonized = c(
    "BITTER",
    "BITTER",
    "ACID",
    "BITTER",
    "SWEET",
    "",
    "BITTER",
    "ACID"
  ),
  value = c(1, 2, 3, 4, 10, 0, 1, 5),
  extra_col = rep("ignored", 8),
  stringsAsFactors = FALSE
)

# With min_jury = 2L:
# - BITTER has 2 distinct jurors (jury_1, jury_2) -> kept!
#   Duplicate row 7 is deduplicated, so values are 1 + 2 = 3 (fraction_1) and 4 (fraction_2), sum_taste = 7.
# - ACID has 2 rows in fraction_1 & fraction_2, but BOTH are from jury_3 (1 distinct juror < 2) -> excluded!
# - SWEET has 1 juror -> excluded!
consistent <- load_consistent_profiles(small_profiles, min_jury = 2L)
expect_true(is.data.frame(consistent))
expect_identical(
  names(consistent),
  c(
    "fraction",
    "session",
    "jury",
    "taste_original",
    "taste",
    "value",
    "sum",
    "sum_taste",
    "group"
  )
)
expect_identical(nrow(consistent), 3L)
expect_true(all(consistent$taste == "BITTER"))
expect_identical(consistent$sum, c(3, 3, 4))
expect_identical(consistent$sum_taste, c(7, 7, 7))
expect_identical(consistent$group, c(1L, 1L, 1L))

# Boundary: min_jury = 1L keeps SWEET (sum_taste = 10, group = 1),
# ACID (sum_taste = 8, group = 2), BITTER (sum_taste = 7, group = 3)
consistent_1 <- load_consistent_profiles(small_profiles, min_jury = 1L)
expect_identical(nrow(consistent_1), 6L)
u_1 <- unique(consistent_1[, c("taste", "sum_taste", "group")])
expect_identical(u_1$taste, c("SWEET", "ACID", "BITTER"))
expect_identical(u_1$sum_taste, c(10, 8, 7))
expect_identical(u_1$group, c(1L, 2L, 3L))

# Boundary: min_jury = 3L excludes everything -> returns 0-row data.frame with exact schema
consistent_3 <- load_consistent_profiles(small_profiles, min_jury = 3L)
expect_identical(nrow(consistent_3), 0L)
expect_identical(
  names(consistent_3),
  c(
    "fraction",
    "session",
    "jury",
    "taste_original",
    "taste",
    "value",
    "sum",
    "sum_taste",
    "group"
  )
)
expect_identical(consistent_3$sum, numeric(0))
expect_identical(consistent_3$sum_taste, numeric(0))
expect_identical(consistent_3$group, integer(0))

profiles_path <- tempfile(fileext = ".tsv")
utils::write.table(
  x = small_profiles[1:6, ],
  file = profiles_path,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)
consistent_from_path <- load_consistent_profiles(profiles_path, min_jury = 2L)
expect_equal(consistent_from_path, consistent)
unlink(profiles_path)

# Golden regression check on built-in profiles ranking
data(profiles, package = "sapid")
full_consistent <- load_consistent_profiles(profiles, min_jury = 2L)
u_ranks <- unique(full_consistent[, c("taste", "sum_taste", "group")])
expect_identical(u_ranks$taste[1:3], c("BITTER", "FATTY", "ACID"))
expect_identical(u_ranks$group, seq_len(nrow(u_ranks)))
expect_equal(u_ranks$sum_taste[1], 1626.6)

# Verify multiplicative RMS juror scaling on consistent BITTER entries:
# jury_1 values (1, 4) yield RMS sqrt(8.5); jury_2 value (2) yields RMS 2;
# pooled RMS across all 3 consistent entries equals sqrt(7).
consistent_norm <- load_consistent_profiles(
  small_profiles,
  min_jury = 2L,
  normalize_panelists = TRUE
)
rms_global <- sqrt(mean(c(1, 2, 4)^2))
rms_j1 <- sqrt(mean(c(1, 4)^2))
rms_j2 <- sqrt(mean(2^2))
expected_vals <- c(
  1 * rms_global / rms_j1,
  2 * rms_global / rms_j2,
  4 * rms_global / rms_j1
)
expect_equal(consistent_norm$value, expected_vals, tolerance = 1e-12)
expect_equal(sum(consistent_norm$value^2), sum(c(1, 2, 4)^2), tolerance = 1e-12)

# .onAttach: safe citation lookup with lib.loc and missing-package guard
on_attach <- get(".onAttach", envir = ns)
expect_message(
  on_attach(tempdir(), "nonexistent_sapid_pkg"),
  pattern = "Welcome to nonexistent_sapid_pkg"
)
expect_message(
  on_attach(dirname(find.package("sapid")), "sapid"),
  pattern = "Welcome to sapid"
)
