library(tinytest)

message("Globals and colors helper checks")
ns <- asNamespace("sapid")
fct_rev_base <- get("fct_rev_base", envir = ns)
fct_reorder_base <- get("fct_reorder_base", envir = ns)
sign_test_pvalue <- get("sign_test_pvalue", envir = ns)
pratt_wilcox_pvalue <- get("pratt_wilcox_pvalue", envir = ns)
fit_afc_psychometric <- get("fit_afc_psychometric", envir = ns)
unescape_xml <- get("unescape_xml", envir = ns)
read_xlsx_base <- get("read_xlsx_base", envir = ns)
load_dataset_or_file <- get("load_dataset_or_file", envir = ns)
prepare_ion_intensities_df <- get("prepare_ion_intensities_df", envir = ns)
prepare_taste_intensities_df <- get("prepare_taste_intensities_df", envir = ns)
cascade_keep_best <- get(".cascade_keep_best_candidates", envir = ns)
label_si_number <- get("label_si_number", envir = ns)
label_short_scale <- get("label_short_scale", envir = ns)
scale_fill_batlow <- get("scale_fill_batlow", envir = ns)
scale_color_batlow <- get("scale_color_batlow", envir = ns)
sigmoid <- get("sigmoid", envir = ns)

# 1. fct_rev_base: reverses factor levels for character and factor inputs
rev_chr <- fct_rev_base(c("b", "a", "c", "a"))
expect_true(is.factor(rev_chr))
expect_identical(levels(rev_chr), c("c", "b", "a"))
expect_identical(as.character(rev_chr), c("b", "a", "c", "a"))

f_custom <- factor(c("x", "y"), levels = c("first", "x", "y", "last"))
rev_fac <- fct_rev_base(f_custom)
expect_identical(levels(rev_fac), c("last", "y", "x", "first"))
expect_identical(as.character(rev_fac), c("x", "y"))

# 2. fct_reorder_base: orders levels by fun(x), handles desc and NA scores
f_in <- c("low", "low", "high", "high", "mid", "mid", "na_grp")
x_in <- c(1, 3, 100, 200, 10, 20, NA_real_)
ord_asc <- fct_reorder_base(f_in, x_in, fun = stats::median, desc = FALSE)
expect_identical(levels(ord_asc), c("low", "mid", "high", "na_grp"))

ord_desc <- fct_reorder_base(f_in, x_in, fun = stats::median, desc = TRUE)
expect_identical(levels(ord_desc), c("high", "mid", "low", "na_grp"))

ord_max <- fct_reorder_base(
  c("a", "a", "b", "b"),
  c(1, 50, 10, 12),
  fun = max,
  desc = FALSE
)
expect_identical(levels(ord_max), c("b", "a"))

# 3. sign_test_pvalue: exact two-sided binomial sign test and edge cases
expect_identical(sign_test_pvalue(c(NA_real_, NA_real_)), 1)
expect_identical(sign_test_pvalue(numeric(0)), 1)
expect_identical(sign_test_pvalue(c(5, 5, 5), md = 5), 1)
expect_identical(sign_test_pvalue(c(0, 0, NA_real_), md = 0), 1)

# 6 positive diffs, 1 zero diff (ignored), 0 negative diffs -> binom.test(6, 6)
x_pos <- c(6, 7, 8, 9, 10, 11, 5)
expect_equal(
  sign_test_pvalue(x_pos, md = 5),
  stats::binom.test(6L, 6L, p = 0.5)$p.value,
  tolerance = 1e-12
)
expect_equal(sign_test_pvalue(x_pos, md = 5), 0.03125, tolerance = 1e-12)

# Asymmetric positive and negative diffs around non-zero md
x_mix <- c(1, 2, 3, 10, 20)
expect_equal(
  sign_test_pvalue(x_mix, md = 5),
  stats::binom.test(2L, 5L, p = 0.5)$p.value,
  tolerance = 1e-12
)

# 4. unescape_xml: unescapes XML entities in correct order (&amp; last)
raw_xml_str <- "&lt;node a=&quot;1&quot; b=&apos;2&apos;&gt;x &amp; &amp;lt;y&amp;gt;"
expect_identical(
  unescape_xml(raw_xml_str),
  "<node a=\"1\" b='2'>x & &lt;y&gt;"
)

# 5. label_si_number & label_short_scale: exact threshold boundaries and signs
nums <- c(
  NA_real_,
  0,
  -0.456,
  999,
  999.99,
  1000,
  -1000,
  1540,
  999999,
  1e6,
  -2.5e6,
  999999999,
  1e9,
  -1.234e9
)
expect_identical(
  label_si_number(nums),
  c(
    NA_character_,
    "0",
    "-0.46",
    "999",
    "999.99",
    "1k",
    "-1k",
    "1.54k",
    "1000k",
    "1M",
    "-2.5M",
    "1000M",
    "1G",
    "-1.23G"
  )
)
expect_identical(
  label_short_scale(nums),
  c(
    NA_character_,
    "0",
    "-0.46",
    "999",
    "999.99",
    "1K",
    "-1K",
    "1.54K",
    "1000K",
    "1M",
    "-2.5M",
    "1000M",
    "1B",
    "-1.23B"
  )
)

# 6. scale_fill_batlow & scale_color_batlow
sf <- scale_fill_batlow()
sc <- scale_color_batlow()
expect_true(inherits(sf, "ScaleContinuous"))
expect_true(inherits(sc, "ScaleContinuous"))
expect_identical(sf$aesthetics, "fill")
expect_identical(sc$aesthetics, "colour")
expect_identical(sf$palette(0), "#011959")
expect_identical(sf$palette(1), "#FACCFA")
expect_identical(sc$palette(0), "#011959")
expect_identical(sc$palette(1), "#FACCFA")

# 7. sigmoid: exact endpoints, midpoint symmetry, direction x vs y, and error
sig_x <- sigmoid(
  x_from = 2,
  x_to = 12,
  y_from = -4,
  y_to = 6,
  smooth = 5,
  n = 101,
  direction = "x"
)
expect_identical(nrow(sig_x), 101L)
expect_identical(names(sig_x), c("x", "y"))
expect_equal(sig_x$x[1L], 2, tolerance = 1e-12)
expect_equal(sig_x$x[51L], 7, tolerance = 1e-12)
expect_equal(sig_x$x[101L], 12, tolerance = 1e-12)
expect_equal(sig_x$y[51L], 1, tolerance = 1e-12)
expect_equal(sig_x$y[1L], stats::plogis(-5) * 10 - 4, tolerance = 1e-12)
expect_equal(sig_x$y[101L], stats::plogis(5) * 10 - 4, tolerance = 1e-12)
expect_true(all(diff(sig_x$x) > 0))
expect_true(all(diff(sig_x$y) > 0))

sig_y <- sigmoid(
  x_from = 2,
  x_to = 12,
  y_from = -4,
  y_to = 6,
  smooth = 5,
  n = 101,
  direction = "y"
)
expect_identical(nrow(sig_y), 101L)
expect_identical(names(sig_y), c("y", "x"))
expect_equal(sig_y$y[1L], -4, tolerance = 1e-12)
expect_equal(sig_y$y[51L], 1, tolerance = 1e-12)
expect_equal(sig_y$y[101L], 6, tolerance = 1e-12)
expect_equal(sig_y$x[51L], 7, tolerance = 1e-12)
expect_equal(sig_y$x[1L], stats::plogis(-5) * 10 + 2, tolerance = 1e-12)
expect_equal(sig_y$x[101L], stats::plogis(5) * 10 + 2, tolerance = 1e-12)

expect_error(
  sigmoid(0, 1, 0, 1, direction = "z"),
  pattern = "Only the directions x or y is allowed\\."
)

# 8. read_xlsx_base: XML/ZIP parsing and edge cases
expect_error(
  read_xlsx_base(tempfile(fileext = ".xlsx")),
  pattern = "File does not exist:"
)

make_custom_xlsx <- function(sheets_xml, shared_strings_xml = NULL) {
  td <- tempfile("xlsx_build_")
  dir.create(file.path(td, "xl", "worksheets"), recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  if (!is.null(shared_strings_xml)) {
    writeLines(shared_strings_xml, file.path(td, "xl", "sharedStrings.xml"))
  }
  for (i in seq_along(sheets_xml)) {
    writeLines(
      sheets_xml[[i]],
      file.path(td, "xl", "worksheets", paste0("sheet", i, ".xml"))
    )
  }
  out_zip <- tempfile(fileext = ".xlsx")
  old_wd <- setwd(td)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(out_zip, files = "xl", flags = "-rq")
  out_zip
}

ss_xml <- paste0(
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
  "<sst><si><t>col_a</t></si>",
  "<si><r><t xml:space=\"preserve\">multi </t></r><r><t>&amp; part</t></r></si>",
  "<si><t>col_aa</t></si></sst>"
)

# Sheet 1: normal with sharedStrings, inlineStr, booleans (1 and 0), numbers,
# empty cells, empty row, and multi-letter column D (col 4)
sheet1_xml <- paste0(
  "<worksheet><sheetData>",
  "<row r=\"1\">",
  "<c r=\"A1\" t=\"s\"><v>0</v></c>",
  "<c r=\"B1\" t=\"inlineStr\"><is><t>col_b</t></is></c>",
  "<c r=\"C1\" t=\"inlineStr\"><is><t>col_bool</t></is></c>",
  "<c r=\"D1\" t=\"s\"><v>2</v></c>",
  "</row>",
  "<row r=\"2\"></row>",
  "<row r=\"3\">",
  "<c r=\"A3\" t=\"s\"><v>1</v></c>",
  "<c r=\"B3\"><v>42</v></c>",
  "<c r=\"C3\" t=\"b\"><v>1</v></c>",
  "<c r=\"D3\"><v>3.5</v></c>",
  "</row>",
  "<row r=\"4\">",
  "<c r=\"A4\" t=\"inlineStr\"><is><t>plain &lt;text&gt;</t></is></c>",
  "<c r=\"B4\"/>",
  "<c r=\"C4\" t=\"b\"><v>0</v></c>",
  "</row>",
  "</sheetData></worksheet>"
)

# Sheet 2: empty sheet (no rows)
sheet2_xml <- "<worksheet><sheetData/></worksheet>"

# Sheet 3: header-only sheet (1 row)
sheet3_xml <- paste0(
  "<worksheet><sheetData>",
  "<row r=\"1\">",
  "<c r=\"A1\" t=\"inlineStr\"><is><t>h1</t></is></c>",
  "<c r=\"B1\" t=\"inlineStr\"><is><t>h2</t></is></c>",
  "</row>",
  "</sheetData></worksheet>"
)

xlsx_test_file <- make_custom_xlsx(
  list(sheet1_xml, sheet2_xml, sheet3_xml),
  shared_strings_xml = ss_xml
)

parsed_s1 <- read_xlsx_base(xlsx_test_file, sheet = 1L)
expect_identical(names(parsed_s1), c("col_a", "col_b", "col_bool", "col_aa"))
expect_identical(nrow(parsed_s1), 3L)
expect_identical(
  parsed_s1$col_a,
  c(NA_character_, "multi & part", "plain <text>")
)
expect_identical(parsed_s1$col_b, c(NA_real_, 42, NA_real_))
expect_identical(typeof(parsed_s1$col_b), "double")
expect_identical(parsed_s1$col_bool, c(NA, TRUE, FALSE))
expect_identical(parsed_s1$col_aa, c(NA_real_, 3.5, NA_real_))

parsed_s2 <- read_xlsx_base(xlsx_test_file, sheet = 2L)
expect_identical(parsed_s2, data.frame())

parsed_s3 <- read_xlsx_base(xlsx_test_file, sheet = 3L)
expect_identical(names(parsed_s3), c("h1", "h2"))
expect_identical(nrow(parsed_s3), 0L)

expect_error(
  read_xlsx_base(xlsx_test_file, sheet = 99L),
  pattern = "Sheet 99 not found in"
)
unlink(xlsx_test_file)

# 9. load_dataset_or_file: data.frame, NULL, empty string, existing file, missing file
df_direct <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
expect_identical(load_dataset_or_file(df_direct, "groups"), df_direct)

loaded_null <- load_dataset_or_file(NULL, "groups")
expect_true(is.data.frame(loaded_null))
expect_true(nrow(loaded_null) > 0L)

loaded_empty <- load_dataset_or_file("", "groups")
expect_identical(loaded_empty, loaded_null)

tmp_tsv <- tempfile(fileext = ".tsv")
utils::write.table(df_direct, tmp_tsv, sep = "\t", row.names = FALSE)
expect_identical(load_dataset_or_file(tmp_tsv, "groups"), df_direct)
unlink(tmp_tsv)

expect_error(load_dataset_or_file(tempfile(fileext = ".tsv"), "groups"))

# 10. prepare_ion_intensities_df: boundaries for min_width, min_area_ion, duplicates
no_area_df <- data.frame(id = 1:2, rt = c(1.1, 2.2), mz = c(100, 200))
res_no_area <- prepare_ion_intensities_df(
  no_area_df,
  min_width = 2L,
  min_area_ion = 100,
  imputation_factor = 0.5
)
expect_identical(
  res_no_area,
  data.frame(
    fraction = integer(0),
    id_ion = integer(0),
    intensity_ion = numeric(0),
    stringsAsFactors = FALSE
  )
)

ions_df <- data.frame(
  id = c(10L, 20L, 30L, 10L), # row 4 is duplicate of row 1
  rt = c(1.0, 2.0, 3.0, 1.0),
  mz = c(100.0, 200.0, 300.0, 100.0),
  `datafile:210619_AR_01_M_03_01.mzML:area` = c(1000, 5000, 2000, 1000),
  `datafile:210619_AR_01_M_01_01.mzML:area` = c(999, 6000, NA_real_, 999),
  `datafile:210619_AR_01_M_02_01.mzML:area` = c(1500, NA_real_, 2500, 1500),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
# Ion 10: non-NA run of length 3 across all 3 cols; values are 1000, 999, 1500.
#   With min_width = 3, min_area_ion = 1000:
#   col 1 (frac 3, 1000) and col 3 (frac 2, 1500) pass; col 2 (999 < 1000) fails!
# Ion 20: non-NA run of length 2 in cols 1-2, then NA in col 3.
#   With min_width = 3: run_counts = c(2, 2, 0) < 3 -> all fail!
#   With min_width = 2: cols 1-2 pass!
res_w3 <- prepare_ion_intensities_df(
  ions_df,
  min_width = 3L,
  min_area_ion = 1000,
  imputation_factor = 0.1
)
expect_identical(res_w3$fraction, c(2L, 3L))
expect_identical(res_w3$id_ion, c(10L, 10L))
expect_identical(res_w3$intensity_ion, c(1500, 1000))

res_w2 <- prepare_ion_intensities_df(
  ions_df,
  min_width = 2L,
  min_area_ion = 1000,
  imputation_factor = 0.1
)
expect_identical(res_w2$fraction, c(1L, 2L, 3L, 3L))
expect_identical(res_w2$id_ion, c(20L, 10L, 10L, 20L))
expect_identical(res_w2$intensity_ion, c(6000, 1500, 1000, 5000))

# Empty result when min_area_ion filters out everything
res_empty_ions <- prepare_ion_intensities_df(
  ions_df,
  min_width = 2L,
  min_area_ion = 99999,
  imputation_factor = 0.1
)
expect_identical(nrow(res_empty_ions), 0L)
expect_identical(
  names(res_empty_ions),
  c("fraction", "id_ion", "intensity_ion")
)

# 11. prepare_taste_intensities_df: 0-value imputation, aggregation, and empty case
tastes_in <- data.frame(
  fraction = c("fraction_02", "fraction_02", "fraction_01", "fraction_01"),
  session = rep("session_01", 4),
  jury = c("jury_01", "jury_02", "jury_01", "jury_02"),
  taste_original = rep("AMER", 4),
  taste_harmonized = rep("BITTER", 4),
  value = c(0, 4, 2, 6),
  stringsAsFactors = FALSE
)
# Non-zero values are 4, 2, 6 -> min_v = 2.
# value == 0 in fraction_02 is imputed to imputation_factor * 2 = 0.25 * 2 = 0.5.
# Sum in fraction_01 = 2 + 6 = 8; sum in fraction_02 = 0.5 + 4 = 4.5.
res_tastes <- prepare_taste_intensities_df(
  tastes_in,
  min_jury = 2L,
  imputation_factor = 0.25
)
expect_identical(res_tastes$fraction, c(1L, 2L))
expect_identical(res_tastes$id_taste, c("BITTER", "BITTER"))
expect_equal(res_tastes$intensity_taste, c(8, 4.5), tolerance = 1e-12)

res_tastes_empty <- prepare_taste_intensities_df(
  tastes_in,
  min_jury = 5L,
  imputation_factor = 0.25
)
expect_identical(
  res_tastes_empty,
  data.frame(
    fraction = integer(0),
    id_taste = character(0),
    intensity_taste = numeric(0),
    stringsAsFactors = FALSE
  )
)

# 12. .cascade_keep_best_candidates: maps candidate_structure_inchikey_no_stereo
data("cascade_annotations", package = "cascade")
ann_sub <- utils::head(as.data.frame(cascade_annotations), 5L)
if ("candidate_structure_inchikey_connectivity_layer" %in% names(ann_sub)) {
  ann_sub$candidate_structure_inchikey_no_stereo <-
    ann_sub$candidate_structure_inchikey_connectivity_layer
  ann_sub$candidate_structure_inchikey_connectivity_layer <- NULL
}
kept <- cascade_keep_best(ann_sub)
expect_true(is.data.frame(kept))
expect_true(nrow(kept) >= 1L)

# 13. pratt_wilcox_pvalue: Pratt (1959) zero-preserving signed-rank test
expect_identical(pratt_wilcox_pvalue(c(NA_real_, NA_real_)), 1)
expect_identical(pratt_wilcox_pvalue(numeric(0)), 1)
expect_identical(pratt_wilcox_pvalue(c(5, 5, 5), md = 5), 1)
expect_identical(pratt_wilcox_pvalue(c(0, 0, NA_real_), md = 0), 1)

# Hand-calculated Pratt test with n0 = 2 zeros and 4 non-zero diffs:
# Expected Pratt (1959) test for c(0, 0, 1, 2, 2, -4):
# |d| ranks (1.5, 1.5, 3, 4.5, 4.5, 6), positive rank sum 12, mean 9,
# zero/tie-corrected variance 513/24, continuity-corrected z of 2.5 / sqrt(513/24).
d_pratt <- c(0, 0, 1, 2, 2, -4)
expected_p_pratt <- 2 * stats::pnorm(2.5 / sqrt(513 / 24), lower.tail = FALSE)
expect_equal(
  pratt_wilcox_pvalue(d_pratt, md = 0),
  expected_p_pratt,
  tolerance = 1e-12
)

# 14. fit_afc_psychometric: 3-AFC binomial psychometric MLE with guessing asymptote 1/3
afc_fit <- fit_afc_psychometric(
  conc = c(0.16, 0.40, 1.00, 2.50, 6.25),
  k = c(4, 5, 9, 10, 10),
  n = c(11, 11, 11, 10, 10),
  m = 3L,
  lambda = 0.05,
  n_grid = 50L
)
expect_identical(afc_fit$gamma, 1 / 3)
expect_true(
  is.finite(afc_fit$ec50) && afc_fit$ec50 > 0.16 && afc_fit$ec50 < 2.50
)
expect_true(is.finite(afc_fit$slope) && afc_fit$slope > 0)
expect_identical(nrow(afc_fit$curve), 50L)
expect_true(all(afc_fit$curve$y >= 1 / 3 & afc_fit$curve$y <= 1))
expect_true(all(afc_fit$curve$ymin >= 1 / 3 & afc_fit$curve$ymax <= 1))
expect_true(all(diff(afc_fit$curve$y) > 0))
# At x = ec50, psychometric curve prob = (1 + gamma)/2 = 2/3
p_at_ec50 <- stats::approx(
  afc_fit$curve$x,
  afc_fit$curve$y,
  xout = afc_fit$ec50
)$y
expect_equal(p_at_ec50, 2 / 3, tolerance = 1e-3)
expect_true(all(afc_fit$curve$ymin <= afc_fit$curve$y + 1e-12))
expect_true(all(afc_fit$curve$y <= afc_fit$curve$ymax + 1e-12))

# 15. prepare_taste_intensities_df: complete_grid = TRUE & normalize_panelists = TRUE
tastes_grid_in <- data.frame(
  fraction = c("fraction_01", "fraction_01", "fraction_02", "fraction_02"),
  session = rep("session_01", 4),
  jury = c("jury_01", "jury_02", "jury_01", "jury_02"),
  taste_original = c("AMER", "AMER", "ACIDE", "ACIDE"),
  taste_harmonized = c("BITTER", "BITTER", "ACID", "ACID"),
  value = c(2, 6, 4, 8),
  stringsAsFactors = FALSE
)
# min_v = 2, baseline = 0.25 * 2 = 0.5
# Without complete_grid: 2 rows (BITTER in frac 1, ACID in frac 2)
# With complete_grid: 4 rows (both BITTER and ACID in frac 1 and frac 2, missing imputed to 0.5)
res_grid <- prepare_taste_intensities_df(
  tastes_grid_in,
  min_jury = 2L,
  imputation_factor = 0.25,
  complete_grid = TRUE
)
expect_identical(nrow(res_grid), 4L)
expect_equal(
  res_grid$intensity_taste[
    res_grid$fraction == 2L & res_grid$id_taste == "BITTER"
  ],
  0.5,
  tolerance = 1e-12
)
expect_equal(
  res_grid$intensity_taste[
    res_grid$fraction == 1L & res_grid$id_taste == "ACID"
  ],
  0.5,
  tolerance = 1e-12
)

# 16. read_xlsx_base hardening: workbook sheet resolution, styles dates, <rPh>, missing r=, zero-cell rows
td_adv <- tempfile("xlsx_adv_")
dir.create(file.path(td_adv, "xl", "worksheets"), recursive = TRUE)
dir.create(file.path(td_adv, "xl", "_rels"), recursive = TRUE)
writeLines(
  paste0(
    "<workbook><sheets>",
    "<sheet name=\"Summary\" sheetId=\"1\" r:id=\"rId1\"/>",
    "<sheet name=\"Sensory\" sheetId=\"2\" r:id=\"rId2\"/>",
    "</sheets></workbook>"
  ),
  file.path(td_adv, "xl", "workbook.xml")
)
writeLines(
  paste0(
    "<Relationships>",
    "<Relationship Id=\"rId1\" Target=\"worksheets/sheet2.xml\"/>",
    "<Relationship Id=\"rId2\" Target=\"/xl/worksheets/sheet1.xml\"/>",
    "</Relationships>"
  ),
  file.path(td_adv, "xl", "_rels", "workbook.xml.rels")
)
writeLines(
  paste0(
    "<sst>",
    "<si xml:space=\"preserve\"><t>label</t><rPh sb=\"0\" eb=\"2\"><t>PHON</t></rPh></si>",
    "</sst>"
  ),
  file.path(td_adv, "xl", "sharedStrings.xml")
)
writeLines(
  paste0(
    "<styleSheet>",
    "<numFmts count=\"1\"><numFmt numFmtId=\"164\" formatCode=\"yyyy-mm-dd\"/></numFmts>",
    "<cellXfs count=\"3\">",
    "<xf numFmtId=\"0\"/>",
    "<xf numFmtId=\"14\"/>",
    "<xf numFmtId=\"164\"/>",
    "</cellXfs>",
    "</styleSheet>"
  ),
  file.path(td_adv, "xl", "styles.xml")
)
writeLines(
  paste0(
    "<worksheet><sheetData>",
    paste0(
      "<row r=\"1\"><c t=\"s\"><v>0</v></c>",
      "<c t=\"inlineStr\"><is><t>dt_builtin</t></is></c>",
      "<c t=\"inlineStr\"><is><t>dt_custom</t></is></c>",
      "<c t=\"inlineStr\"><is><t>val</t></is></c></row>"
    ),
    paste0(
      "<row r=\"2\"><c t=\"inlineStr\"><is><t>row1</t></is></c>",
      "<c s=\"1\"><v>45292</v></c><c s=\"2\"><v>45293.5</v></c>",
      "<c s=\"0\"><v>12.5</v></c></row>"
    ),
    "</sheetData></worksheet>"
  ),
  file.path(td_adv, "xl", "worksheets", "sheet1.xml")
)
writeLines(
  "<worksheet><sheetData><row r=\"1\"/><row r=\"2\"></row></sheetData></worksheet>",
  file.path(td_adv, "xl", "worksheets", "sheet2.xml")
)
xlsx_adv_file <- tempfile(fileext = ".xlsx")
old_wd_adv <- setwd(td_adv)
utils::zip(xlsx_adv_file, files = "xl", flags = "-rq")
setwd(old_wd_adv)
unlink(td_adv, recursive = TRUE)

by_name <- read_xlsx_base(xlsx_adv_file, sheet = "Sensory")
expect_identical(names(by_name), c("label", "dt_builtin", "dt_custom", "val"))
expect_identical(by_name$label, "row1")
expect_identical(by_name$dt_builtin, "2024-01-01")
expect_identical(by_name$dt_custom, "2024-01-02 12:00:00")
expect_identical(by_name$val, 12.5)
expect_identical(read_xlsx_base(xlsx_adv_file, sheet = 1L), data.frame())
expect_error(read_xlsx_base(xlsx_adv_file, sheet = "Missing"), "not found")
unlink(xlsx_adv_file)

# 17. fit_afc_psychometric and sigmoid validation + generic _M_ ion column parsing
expect_error(
  fit_afc_psychometric(c(1, 2), c(1), c(2, 2)),
  "same length"
)
expect_error(
  fit_afc_psychometric(c(1, 2), c(1, 2), c(5, 5), m = 1),
  ">= 2"
)
expect_error(
  fit_afc_psychometric(c(1, 2), c(6, 2), c(5, 5), m = 3L),
  "0 <= k <= n"
)
expect_error(
  fit_afc_psychometric(c(0, -1), c(0, 0), c(5, 5), m = 3L),
  "At least one valid positive concentration"
)

expect_error(sigmoid(0, 10, 0, 1, smooth = 0), "positive number")
expect_error(sigmoid(0, 10, 0, 1, n = 1), ">= 2")

generic_ions <- data.frame(
  id = c(1L, 2L),
  rt = c(1.0, 2.0),
  mz = c(100.0, 200.0),
  `datafile:sample_M_17_run2.mzML:area` = c(500, 10),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
res_gen_ions <- prepare_ion_intensities_df(
  generic_ions,
  min_width = 1L,
  min_area_ion = 100
)
expect_identical(res_gen_ions$fraction, 17L)
expect_identical(res_gen_ions$id_ion, 1L)
expect_identical(res_gen_ions$intensity_ion, 500)
