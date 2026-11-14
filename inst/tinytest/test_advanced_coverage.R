library(tinytest)

message(
  "Advanced coverage: prepare_concentration, correlate edge cases, plot_correlations, plot_profiles, prepare_chemical_profiles"
)
ns <- asNamespace("sapid")
correlate_ion_taste_intensities <- get(
  "correlate_ion_taste_intensities",
  envir = ns
)
prepare_concentration <- get("prepare_concentration", envir = ns)
plot_correlations <- get("plot_correlations", envir = ns)
plot_profiles <- get("plot_profiles", envir = ns)
prepare_chemical_profiles <- get("prepare_chemical_profiles", envir = ns)

orig_dir <- getwd()
conc_fixture <- file.path(orig_dir, "fixtures", "concentration_fixture.xlsx")
if (!file.exists(conc_fixture)) {
  conc_fixture <- file.path(
    orig_dir,
    "inst",
    "tinytest",
    "fixtures",
    "concentration_fixture.xlsx"
  )
}

# 1. prepare_concentration: full happy path with exact value assertions
withr::with_tempdir({
  expect_true(file.exists(conc_fixture))
  out_tsv <- file.path(getwd(), "conc.tsv")
  ret <- prepare_concentration(input_xlsx = conc_fixture, output = out_tsv)
  expect_identical(ret, out_tsv)
  expect_true(file.exists(out_tsv))
  result <- utils::read.delim(
    out_tsv,
    check.names = FALSE,
    colClasses = c(concentration = "character"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    names(result),
    c("concentration", "jury", "taste", "value", "afc_correct", "afc_total")
  )
  expect_identical(nrow(result), 4L)
  expect_identical(result$concentration, rep("3.10", 4L))
  expect_identical(result$jury, c("jury_01", "jury_01", "jury_02", "jury_02"))
  expect_identical(result$taste, c("amer", "amer", "acide", "amer"))
  expect_equal(result$value, c(7.5, 7.5, 8.2, 8.2), tolerance = 1e-12)
  expect_identical(result$afc_correct, c(8L, 8L, 9L, 9L))
  expect_identical(result$afc_total, c(12L, 12L, 12L, 12L))
})

# 2. correlate_ion_taste_intensities: exact mathematical verification across all branches
withr::with_tempdir({
  # 5 fractions (1..5):
  # - Ion 101: normal vector c(10, 20, 30, 40, 50) -> Pearson with normal taste y_norm
  # - Ion 102: skewed untied vector c(100, 101, 102, 103, 10000) -> Kendall exact LUT (positive)
  # - Ion 103: skewed untied descending c(10000, 103, 102, 101, 100) -> Kendall exact LUT (negative)
  # - Ion 104: skewed tied vector c(100, 100, 200, 200, 10000) -> Kendall tied normal approximation
  # - Ion 105: constant vector c(500, 500, 500, 500, 500) -> constant warning, NA filtered out
  x_norm <- c(10, 20, 30, 40, 50)
  x_skew_pos <- c(100, 101, 102, 103, 10000)
  x_skew_neg <- c(10000, 103, 102, 101, 100)
  x_tied <- c(100, 100, 200, 200, 10000)
  x_const <- c(500, 500, 500, 500, 500)

  ions_df <- data.frame(
    id = c(101L, 102L, 103L, 104L, 105L),
    rt = 1:5,
    mz = c(100, 200, 300, 400, 500)
  )
  for (fr in 1:5) {
    ions_df[[sprintf("datafile:210619_AR_%02d_M_%d_01.mzML:area", fr, fr)]] <-
      c(
        x_norm[fr],
        x_skew_pos[fr],
        x_skew_neg[fr],
        x_tied[fr],
        x_const[fr]
      )
  }

  # Taste "BITTER" is normal & untied: sum of 2 jurors = c(12, 18, 32, 39, 51)
  # Taste "SWEET" has ties: sum of 2 jurors = c(10, 10, 20, 30, 40)
  y_bitter <- c(12, 18, 32, 39, 51)
  y_sweet <- c(10, 10, 20, 30, 40)

  tastes_df <- rbind(
    data.frame(
      fraction = sprintf("fraction_%d", rep(1:5, each = 2L)),
      session = "session_01",
      jury = rep(c("jury_01", "jury_02"), 5L),
      taste_original = "AMER",
      taste_harmonized = "BITTER",
      value = as.vector(rbind(y_bitter / 2, y_bitter / 2)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      fraction = sprintf("fraction_%d", rep(1:5, each = 2L)),
      session = "session_01",
      jury = rep(c("jury_01", "jury_02"), 5L),
      taste_original = "SUCRE",
      taste_harmonized = "SWEET",
      value = as.vector(rbind(y_sweet / 2, y_sweet / 2)),
      stringsAsFactors = FALSE
    )
  )

  corr_out <- file.path(getwd(), "corr_exact.tsv")
  warn_env <- new.env(parent = emptyenv())
  warn_env$msgs <- character(0)
  ret_c <- withCallingHandlers(
    correlate_ion_taste_intensities(
      input_ions = ions_df,
      input_tastes = tastes_df,
      output = corr_out,
      min_jury = 2L,
      min_area_ion = 1L,
      widths = 5L
    ),
    warning = function(w) {
      warn_env$msgs <- c(warn_env$msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(ret_c, corr_out)
  expect_true(any(grepl("All values in x or y are identical", warn_env$msgs)))
  expect_true(any(grepl(
    "cannot compute exact p-value with ties",
    warn_env$msgs
  )))

  res_c <- utils::read.delim(corr_out, stringsAsFactors = FALSE)
  # Constant ion 105 has NA p_adjusted and must be filtered out
  expect_false(105L %in% res_c$id_ion)

  # Check Ion 101 vs BITTER (Pearson)
  row_101_b <- res_c[res_c$id_ion == 101L & res_c$id_taste == "BITTER", ]
  ref_101_b <- stats::cor.test(x_norm, y_bitter, method = "pearson")
  expect_identical(row_101_b$method, "pearson")
  expect_equal(
    row_101_b$correlation,
    unname(ref_101_b$estimate),
    tolerance = 1e-12
  )
  expect_equal(row_101_b$p_value, ref_101_b$p.value, tolerance = 1e-12)

  # Check Ion 102 vs BITTER (Kendall exact LUT, positive)
  row_102_b <- res_c[res_c$id_ion == 102L & res_c$id_taste == "BITTER", ]
  ref_102_b <- stats::cor.test(
    x_skew_pos,
    y_bitter,
    method = "kendall",
    exact = TRUE
  )
  expect_identical(row_102_b$method, "kendall")
  expect_equal(
    row_102_b$correlation,
    unname(ref_102_b$estimate),
    tolerance = 1e-12
  )
  expect_equal(row_102_b$p_value, ref_102_b$p.value, tolerance = 1e-12)

  # Check Ion 103 vs BITTER (Kendall exact LUT, negative)
  row_103_b <- res_c[res_c$id_ion == 103L & res_c$id_taste == "BITTER", ]
  ref_103_b <- stats::cor.test(
    x_skew_neg,
    y_bitter,
    method = "kendall",
    exact = TRUE
  )
  expect_identical(row_103_b$method, "kendall")
  expect_equal(
    row_103_b$correlation,
    unname(ref_103_b$estimate),
    tolerance = 1e-12
  )
  expect_equal(row_103_b$p_value, ref_103_b$p.value, tolerance = 1e-12)

  # Check Ion 104 vs SWEET (Kendall tied normal approximation with ties in BOTH x and y)
  row_104_s <- res_c[res_c$id_ion == 104L & res_c$id_taste == "SWEET", ]
  ref_104_s <- suppressWarnings(stats::cor.test(
    x_tied,
    y_sweet,
    method = "kendall",
    exact = FALSE
  ))
  expect_identical(row_104_s$method, "kendall")
  expect_equal(
    row_104_s$correlation,
    unname(ref_104_s$estimate),
    tolerance = 1e-12
  )
  expect_equal(row_104_s$p_value, ref_104_s$p.value, tolerance = 1e-12)

  # Verify BH FDR adjustment across the 4 non-constant ions for BITTER
  bitter_rows <- res_c[res_c$id_taste == "BITTER", ]
  expect_equal(
    bitter_rows$p_adjusted,
    stats::p.adjust(bitter_rows$p_value, method = "BH"),
    tolerance = 1e-12
  )

  # Verify greiner_kendall = TRUE and fdr_scope = "global"
  corr_greiner <- file.path(getwd(), "corr_greiner.tsv")
  suppressWarnings(correlate_ion_taste_intensities(
    input_ions = ions_df,
    input_tastes = tastes_df,
    output = corr_greiner,
    min_jury = 2L,
    min_area_ion = 1L,
    widths = 5L,
    greiner_kendall = TRUE,
    fdr_scope = "global"
  ))
  res_g <- utils::read.delim(corr_greiner, stringsAsFactors = FALSE)
  row_102_g <- res_g[res_g$id_ion == 102L & res_g$id_taste == "BITTER", ]
  expect_equal(
    row_102_g$correlation,
    sin(0.5 * pi * unname(ref_102_b$estimate)),
    tolerance = 1e-12
  )
  expect_equal(
    res_g$p_adjusted,
    stats::p.adjust(res_g$p_value, method = "BH"),
    tolerance = 1e-12
  )

  # Edge cases: n == 2L, n == 1L, non-overlapping window, and width > length(fractions)
  tastes_sub <- tastes_df[
    tastes_df$taste_harmonized == "BITTER" &
      tastes_df$fraction %in% c("fraction_1", "fraction_2"),
  ]
  warn_env2 <- new.env(parent = emptyenv())
  warn_env2$msgs <- character(0)
  out_w2 <- file.path(getwd(), "corr_w2.tsv")
  withCallingHandlers(
    correlate_ion_taste_intensities(
      input_ions = ions_df[1:2, ],
      input_tastes = tastes_sub,
      output = out_w2,
      min_jury = 2L,
      min_area_ion = 1L,
      widths = 2L
    ),
    warning = function(w) {
      warn_env2$msgs <- c(warn_env2$msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # Window 1:2 has n == 2; Window 2:3 has n == 1; Windows 3:4 & 4:5 have 0 valid fractions
  expect_true(any(grepl(
    "Sample size too small for Shapiro-Wilk test",
    warn_env2$msgs
  )))
  expect_true(any(grepl(
    "Not enough finite observations for correlation",
    warn_env2$msgs
  )))
  expect_true(any(grepl("No data found for fractions_list:", warn_env2$msgs)))

  # Window width > number of fractions -> empty 0-row TSV
  out_empty <- file.path(getwd(), "corr_empty.tsv")
  correlate_ion_taste_intensities(
    input_ions = ions_df[1:2, ],
    input_tastes = tastes_sub,
    output = out_empty,
    min_jury = 2L,
    min_area_ion = 1L,
    widths = 10L
  )
  df_empty <- utils::read.delim(out_empty, stringsAsFactors = FALSE)
  expect_identical(nrow(df_empty), 0L)
  expect_identical(
    names(df_empty),
    c(
      "id_ion",
      "id_taste",
      "fractions",
      "correlation",
      "p_value",
      "p_adjusted",
      "method"
    )
  )

  # Complementary missing ions: each ion has run length 2 (passes min_width = 2),
  # but in window of width 4 no single ion is complete across all 4 fractions
  ions_comp <- data.frame(
    id = c(201L, 202L),
    rt = c(1.0, 2.0),
    mz = c(100.0, 200.0),
    `datafile:210619_AR_01_M_1_01.mzML:area` = c(100, NA),
    `datafile:210619_AR_02_M_2_01.mzML:area` = c(200, NA),
    `datafile:210619_AR_03_M_3_01.mzML:area` = c(NA, 300),
    `datafile:210619_AR_04_M_4_01.mzML:area` = c(NA, 400),
    check.names = FALSE
  )
  out_comp <- file.path(getwd(), "corr_comp.tsv")
  suppressWarnings(correlate_ion_taste_intensities(
    input_ions = ions_comp,
    input_tastes = tastes_df[
      tastes_df$taste_harmonized == "BITTER" &
        tastes_df$fraction %in% paste0("fraction_", 1:4),
    ],
    output = out_comp,
    min_jury = 2L,
    min_area_ion = 1L,
    widths = c(2L, 4L)
  ))
  df_comp <- utils::read.delim(out_comp, stringsAsFactors = FALSE)
  expect_false("1 2 3 4" %in% df_comp$fractions)

  # Sample size n > 5000L defaults to Kendall correlation with warning
  n_large <- 5001L
  large_cols <- as.list(as.numeric(seq_len(n_large) + 10L))
  names(large_cols) <- sprintf(
    "datafile:210619_AR_01_M_%d_01.mzML:area",
    seq_len(n_large)
  )
  ions_large <- as.data.frame(
    c(list(id = 301L, rt = 1.0, mz = 150.0), large_cols),
    check.names = FALSE
  )
  tastes_large <- data.frame(
    fraction = rep(sprintf("fraction_%d", seq_len(n_large)), each = 2L),
    session = "session_01",
    jury = rep(c("jury_01", "jury_02"), n_large),
    taste_original = "AMER",
    taste_harmonized = "BITTER",
    value = rep(as.numeric(seq_len(n_large)), each = 2L),
    stringsAsFactors = FALSE
  )
  warn_large <- new.env(parent = emptyenv())
  warn_large$msgs <- character(0)
  out_large <- file.path(getwd(), "corr_large.tsv")
  withCallingHandlers(
    correlate_ion_taste_intensities(
      input_ions = ions_large,
      input_tastes = tastes_large,
      output = out_large,
      min_jury = 2L,
      min_area_ion = 1L,
      widths = n_large
    ),
    warning = function(w) {
      warn_large$msgs <- c(warn_large$msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl(
    "Sample size too large for Shapiro-Wilk test",
    warn_large$msgs
  )))
  df_large <- utils::read.delim(out_large, stringsAsFactors = FALSE)
  expect_identical(df_large$method, "kendall")
  expect_equal(df_large$correlation, 1.0, tolerance = 1e-12)
})

# 3. plot_correlations: full 3-PDF run with iridoid annotation and missing-bitter-iridoid error
withr::with_tempdir({
  corr <- data.frame(
    id_ion = c(101L, 102L),
    id_taste = c("BITTER", "SWEET"),
    fractions = c("32 33 34", "32 33 34"),
    correlation = c(0.99, 0.98),
    p_value = c(0.001, 0.002),
    p_adjusted = c(0.01, 0.02),
    method = c("kendall", "kendall"),
    stringsAsFactors = FALSE
  )
  ions <- data.frame(
    id = c(101L, 102L),
    rt = c(1.0, 2.0),
    mz = c(100.0, 200.0),
    stringsAsFactors = FALSE
  )
  ions[["datafile:210619_AR_12_M_32_01.mzML:area"]] <- c(5000, 3000)
  ions[["datafile:210619_AR_13_M_33_01.mzML:area"]] <- c(6000, 4000)
  ions[["datafile:210619_AR_14_M_34_01.mzML:area"]] <- c(4000, 2000)

  num_cols <- c(
    "feature_mz",
    "feature_rt",
    "score_input",
    "candidate_count_similarity_peaks_matched",
    "candidate_score_similarity",
    "score_initial",
    "score_biological",
    "score_chemical",
    "score_final",
    "feature_pred_tax_npc_01pat_score",
    "feature_pred_tax_npc_02sup_score",
    "feature_pred_tax_npc_03cla_score"
  )
  all_cols <- c(
    "feature_id",
    "feature_mz",
    "feature_rt",
    "score_input",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_molecular_formula",
    "candidate_structure_tax_npc_01pat",
    "candidate_structure_tax_npc_02sup",
    "candidate_structure_tax_npc_03cla",
    "candidate_structure_organism_occurrence_closest",
    "candidate_count_similarity_peaks_matched",
    "candidate_score_similarity",
    "score_initial",
    "score_biological",
    "score_chemical",
    "score_final",
    "feature_pred_tax_npc_01pat_val",
    "feature_pred_tax_npc_02sup_val",
    "feature_pred_tax_npc_03cla_val",
    "feature_pred_tax_npc_01pat_score",
    "feature_pred_tax_npc_02sup_score",
    "feature_pred_tax_npc_03cla_score",
    "mode"
  )
  ann <- as.data.frame(setNames(
    lapply(all_cols, function(x) {
      if (x == "feature_id") {
        101L
      } else if (x %in% num_cols) {
        0.95
      } else {
        NA_character_
      }
    }),
    all_cols
  ))
  ann$mode <- "pos"
  ann$candidate_structure_tax_npc_03cla <- "Iridoids"
  ann$feature_pred_tax_npc_03cla_val <- "Iridoids"
  ann$candidate_structure_inchikey_connectivity_layer <- "AAABBBCCCDDDEEE"
  ann$candidate_structure_smiles_no_stereo <- "C1CCCC1"
  ann$feature_pred_tax_npc_03cla_score <- 0.95
  ann$score_final <- 0.95

  corr_path <- file.path(getwd(), "corr.tsv")
  ions_path <- file.path(getwd(), "ions.csv")
  ann_path <- file.path(getwd(), "ann.tsv")
  utils::write.table(
    corr,
    corr_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  utils::write.table(
    ions,
    ions_path,
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )
  utils::write.table(
    ann,
    ann_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  c1 <- file.path(getwd(), "c1.pdf")
  c2 <- file.path(getwd(), "c2.pdf")
  c3 <- file.path(getwd(), "c3.pdf")

  ret <- plot_correlations(
    input_correlations = corr_path,
    input_ions = ions_path,
    annotation_path_fractions = ann_path,
    output_1 = c1,
    output_2 = c2,
    output_3 = c3,
    widths = 2:3,
    min_corr = 0.0,
    max_pval = 1.0,
    min_intense_ions_ratio = 0.01,
    min_jury = 1L,
    min_area_ion = 1L,
    min_consistency = 0.9
  )
  expect_identical(
    ret,
    list(
      plot_correlations_1 = c1,
      plot_correlations_2 = c2,
      plot_correlations_3 = c3
    )
  )
  expect_true(file.exists(c1) && file.info(c1)$size > 0)
  expect_true(file.exists(c2) && file.info(c2)$size > 0)
  expect_true(file.exists(c3) && file.info(c3)$size > 0)

  # Error path: confident iridoids exist (for ACID), but no BITTER correlation matches
  corr_acid <- corr
  corr_acid$id_taste <- "ACID"
  corr_acid_path <- file.path(getwd(), "corr_acid.tsv")
  utils::write.table(
    corr_acid,
    corr_acid_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  expect_error(
    plot_correlations(
      input_correlations = corr_acid_path,
      input_ions = ions_path,
      annotation_path_fractions = ann_path,
      output_1 = c1,
      output_2 = c2,
      output_3 = c3,
      widths = 2:3,
      min_corr = 0.0,
      max_pval = 1.0,
      min_intense_ions_ratio = 0.01,
      min_jury = 1L,
      min_area_ion = 1L,
      min_consistency = 0.9
    ),
    pattern = "No bitter iridoid correlations found for fractions\\.x\\."
  )
})

# 4. plot_profiles: full happy path with cascade dataset subsets
withr::with_tempdir({
  data("cascade_annotations", package = "cascade")
  data("cascade_features", package = "cascade")
  data("cascade_features_informed", package = "cascade")
  data("cascade_features_not_informed", package = "cascade")

  fids <- unique(cascade_features_informed$feature_id)[1:15]
  fi_sub <- subset(cascade_features_informed, feature_id %in% fids)
  fni_sub <- subset(cascade_features_not_informed, feature_id %in% fids)
  feat_sub <- subset(cascade_features, id %in% fids)
  ann_sub <- subset(cascade_annotations, feature_id %in% fids)

  peaks_dir <- file.path(getwd(), "peaks_frac")
  dir.create(peaks_dir)
  data.table::fwrite(
    fi_sub,
    file.path(peaks_dir, "210619_AR_06_M_17_2_01_featuresInformed_cad.tsv"),
    sep = "\t"
  )
  data.table::fwrite(
    fni_sub,
    file.path(peaks_dir, "210619_AR_06_M_17_2_01_featuresNotInformed_cad.tsv"),
    sep = "\t"
  )

  feat_path <- file.path(getwd(), "features.csv")
  data.table::fwrite(feat_sub, feat_path)
  ann_path <- file.path(getwd(), "annotations.tsv")
  data.table::fwrite(ann_sub, ann_path, sep = "\t")

  out_pdf <- file.path(getwd(), "figure_profiles.pdf")
  ret_prof <- plot_profiles(
    output = out_pdf,
    annotation_path_fractions = ann_path,
    features_path_fractions = feat_path,
    peaks_dir_fractions = peaks_dir
  )
  expect_identical(ret_prof, out_pdf)
  expect_true(file.exists(out_pdf) && file.info(out_pdf)$size > 0)
})

# 5. prepare_chemical_profiles: full happy path with synthetic .mzML and features
withr::with_tempdir({
  data("cascade_chromatograms_positive", package = "cascade")
  cad <- subset(cascade_chromatograms_positive, chromatogram == "cad")[
    seq(1, 49468, by = 20),
  ]
  enc_f64 <- function(x) {
    raw_bytes <- writeBin(as.numeric(x), raw(), size = 8L, endian = "little")
    gsub("[\r\n]", "", jsonlite::base64_enc(raw_bytes))
  }
  rt_b64 <- enc_f64(cad$rtime)
  int_b64 <- enc_f64(cad$intensity)
  n_pts <- nrow(cad)
  mz_b64 <- enc_f64(c(150.0, 250.0))
  sp_int_b64 <- enc_f64(c(200000.0, 300000.0))

  cids <- c("BasePeak_0", "PDA#1_TotalAbsorbance_0", "UV#1_CAD_1_0")
  chrom_xml <- paste(
    vapply(
      seq_along(cids),
      function(i) {
        paste0(
          "<chromatogram index=\"",
          i - 1L,
          "\" id=\"",
          cids[i],
          "\" defaultArrayLength=\"",
          n_pts,
          "\">",
          "<binaryDataArrayList count=\"2\">",
          "<binaryDataArray encodedLength=\"",
          nchar(rt_b64),
          "\"><cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\"/>",
          "<cvParam cvRef=\"MS\" accession=\"MS:1000576\" name=\"no compression\"/>",
          "<cvParam cvRef=\"MS\" accession=\"MS:1000595\" name=\"time array\" unitCvRef=\"UO\" unitAccession=\"UO:0000031\" unitName=\"minute\"/>",
          "<binary>",
          rt_b64,
          "</binary></binaryDataArray>",
          "<binaryDataArray encodedLength=\"",
          nchar(int_b64),
          "\"><cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\"/>",
          "<cvParam cvRef=\"MS\" accession=\"MS:1000576\" name=\"no compression\"/>",
          "<cvParam cvRef=\"MS\" accession=\"MS:1000515\" name=\"intensity array\"/>",
          "<binary>",
          int_b64,
          "</binary></binaryDataArray>",
          "</binaryDataArrayList></chromatogram>"
        )
      },
      character(1L)
    ),
    collapse = "\n"
  )

  mzml_txt <- paste0(
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
    "<mzML xmlns=\"http://psi.hupo.org/ms/mzml\" version=\"1.1.0\">\n",
    "<cvList count=\"1\"><cv id=\"MS\" fullName=\"MS\" version=\"1\" URI=\"\"/></cvList>\n",
    "<run id=\"run1\">\n",
    "<spectrumList count=\"2\" defaultDataProcessingRef=\"dp\">\n",
    "<spectrum index=\"0\" id=\"scan=1\" defaultArrayLength=\"2\">",
    "<cvParam cvRef=\"MS\" accession=\"MS:1000511\" name=\"ms level\" value=\"1\"/>",
    "<scanList count=\"1\"><scan><cvParam cvRef=\"MS\" accession=\"MS:1000016\" name=\"scan start time\" value=\"5.0\" unitCvRef=\"UO\" unitAccession=\"UO:0000031\" unitName=\"minute\"/></scan></scanList>",
    "<binaryDataArrayList count=\"2\">",
    "<binaryDataArray encodedLength=\"",
    nchar(mz_b64),
    "\"><cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\"/><cvParam cvRef=\"MS\" accession=\"MS:1000576\" name=\"no compression\"/><cvParam cvRef=\"MS\" accession=\"MS:1000514\" name=\"m/z array\"/><binary>",
    mz_b64,
    "</binary></binaryDataArray>",
    "<binaryDataArray encodedLength=\"",
    nchar(sp_int_b64),
    "\"><cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\"/><cvParam cvRef=\"MS\" accession=\"MS:1000576\" name=\"no compression\"/><cvParam cvRef=\"MS\" accession=\"MS:1000515\" name=\"intensity array\"/><binary>",
    sp_int_b64,
    "</binary></binaryDataArray>",
    "</binaryDataArrayList></spectrum>\n",
    "<spectrum index=\"1\" id=\"scan=2\" defaultArrayLength=\"2\">",
    "<cvParam cvRef=\"MS\" accession=\"MS:1000511\" name=\"ms level\" value=\"1\"/>",
    "<scanList count=\"1\"><scan><cvParam cvRef=\"MS\" accession=\"MS:1000016\" name=\"scan start time\" value=\"5.5\" unitCvRef=\"UO\" unitAccession=\"UO:0000031\" unitName=\"minute\"/></scan></scanList>",
    "<binaryDataArrayList count=\"2\">",
    "<binaryDataArray encodedLength=\"",
    nchar(mz_b64),
    "\"><cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\"/><cvParam cvRef=\"MS\" accession=\"MS:1000576\" name=\"no compression\"/><cvParam cvRef=\"MS\" accession=\"MS:1000514\" name=\"m/z array\"/><binary>",
    mz_b64,
    "</binary></binaryDataArray>",
    "<binaryDataArray encodedLength=\"",
    nchar(sp_int_b64),
    "\"><cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\"/><cvParam cvRef=\"MS\" accession=\"MS:1000576\" name=\"no compression\"/><cvParam cvRef=\"MS\" accession=\"MS:1000515\" name=\"intensity array\"/><binary>",
    sp_int_b64,
    "</binary></binaryDataArray>",
    "</binaryDataArrayList></spectrum>\n",
    "</spectrumList>\n",
    "<chromatogramList count=\"3\" defaultDataProcessingRef=\"dp\">\n",
    chrom_xml,
    "\n</chromatogramList>\n</run>\n</mzML>\n"
  )

  mzml_dir <- file.path(getwd(), "mzmls")
  dir.create(mzml_dir)
  writeLines(mzml_txt, file.path(mzml_dir, "sample_M_01_01.mzML"))
  writeLines(mzml_txt, file.path(mzml_dir, "sample_M_17_01.mzML"))

  feat_df <- data.frame(
    id = 1L,
    rt = 5.2,
    mz = 150.0,
    area = 500000,
    `datafile:sample_M_01_01.mzML:feature_state` = "DETECTED",
    `datafile:sample_M_01_01.mzML:rt_range:min` = 4.8,
    `datafile:sample_M_01_01.mzML:rt_range:max` = 5.8,
    `datafile:sample_M_01_01.mzML:mz_range:min` = 149.9,
    `datafile:sample_M_01_01.mzML:mz_range:max` = 150.1,
    `datafile:sample_M_01_01.mzML:intensity_range:min` = 1000,
    `datafile:sample_M_01_01.mzML:intensity_range:max` = 300000,
    `datafile:sample_M_17_01.mzML:feature_state` = "DETECTED",
    `datafile:sample_M_17_01.mzML:rt_range:min` = 4.8,
    `datafile:sample_M_17_01.mzML:rt_range:max` = 5.8,
    `datafile:sample_M_17_01.mzML:mz_range:min` = 149.9,
    `datafile:sample_M_17_01.mzML:mz_range:max` = 150.1,
    `datafile:sample_M_17_01.mzML:intensity_range:min` = 1000,
    `datafile:sample_M_17_01.mzML:intensity_range:max` = 300000,
    check.names = FALSE
  )
  feat_csv <- file.path(getwd(), "features.csv")
  utils::write.csv(feat_df, feat_csv, row.names = FALSE)

  ext_dir <- file.path(getwd(), "ext_peaks")
  frac_dir <- file.path(getwd(), "frac_peaks")
  ret_chem <- prepare_chemical_profiles(
    mzmls_dir = mzml_dir,
    peaks_dir_extract = ext_dir,
    peaks_dir_fractions = frac_dir,
    features_path_extract = feat_csv,
    features_path_fractions = feat_csv,
    min_intensity = 1e5,
    start_extract = 1L,
    end_extract = 1L,
    start_fractions = 2L,
    end_fractions = 2L
  )
  expect_true(is.null(ret_chem))
  expected_cols <- c(
    "sample",
    "peak_id",
    "peak_rt_min",
    "peak_rt_apex",
    "peak_rt_max",
    "peak_area",
    "feature_id",
    "feature_rt",
    "feature_mz",
    "feature_area",
    "comparison_score"
  )
  informed_tsvs <- c(
    file.path(ext_dir, "sample_M_01_01_featuresInformed_cad.tsv"),
    file.path(frac_dir, "sample_M_17_01_featuresInformed_cad.tsv")
  )
  not_informed_tsvs <- c(
    file.path(ext_dir, "sample_M_01_01_featuresNotInformed_cad.tsv"),
    file.path(frac_dir, "sample_M_17_01_featuresNotInformed_cad.tsv")
  )
  for (tsv_path in informed_tsvs) {
    expect_true(file.exists(tsv_path))
    tsv_df <- data.table::fread(tsv_path, data.table = FALSE)
    expect_identical(nrow(tsv_df), 1L)
    expect_identical(names(tsv_df), expected_cols)
    expect_identical(as.integer(tsv_df$feature_id), 1L)
  }
  for (tsv_path in not_informed_tsvs) {
    expect_true(file.exists(tsv_path))
    tsv_df <- data.table::fread(tsv_path, data.table = FALSE)
    expect_identical(nrow(tsv_df), 0L)
    expect_identical(names(tsv_df), expected_cols)
  }
})
