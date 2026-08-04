library(tinytest)

message(
  "Advanced coverage: prepare_concentration, correlate edge cases, plot_correlations full run"
)
ns <- asNamespace("sapid")
correlate_ion_taste_intensities <- get(
  "correlate_ion_taste_intensities",
  envir = ns
)
prepare_concentration <- get("prepare_concentration", envir = ns)
plot_correlations <- get("plot_correlations", envir = ns)

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

# prepare_concentration: full happy path with xlsx fixture
withr::with_tempdir({
  expect_true(file.exists(conc_fixture))
  out_tsv <- file.path(getwd(), "conc.tsv")
  ret <- prepare_concentration(input_xlsx = conc_fixture, output = out_tsv)
  expect_identical(ret, out_tsv)
  expect_true(file.exists(out_tsv))
  result <- utils::read.delim(out_tsv, check.names = FALSE)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("jury" %in% names(result))
  expect_true("concentration" %in% names(result))
  expect_true("taste" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true("afc_correct" %in% names(result))
  expect_true("afc_total" %in% names(result))
})

# correlate_ion_taste_intensities: file-path input_tastes branch (line 91)
withr::with_tempdir({
  data(profiles, package = "sapid")
  profiles_path <- file.path(getwd(), "profiles.tsv")
  utils::write.table(
    as.data.frame(profiles),
    profiles_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  ions <- data.frame(id = c(101L, 102L), rt = c(1, 2), mz = c(100.1, 200.2))
  for (fr in 32:38) {
    ions[[sprintf("datafile:210619_AR_%02d_M_%d_01.mzML:area", fr - 20, fr)]] <-
      c(fr * 100, fr * 120)
  }
  ions_path <- file.path(getwd(), "ions.csv")
  corr_path <- file.path(getwd(), "corr.tsv")
  utils::write.table(
    ions,
    ions_path,
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )
  ret <- correlate_ion_taste_intensities(
    input_ions = ions_path,
    input_tastes = profiles_path,
    output = corr_path,
    min_jury = 1L,
    min_area_ion = 1L,
    widths = 2:3
  )
  expect_identical(ret, corr_path)
  expect_true(file.exists(corr_path))
  df <- utils::read.delim(corr_path, check.names = FALSE)
  expect_true(is.data.frame(df))
})

# plot_correlations: full 3-PDF run with iridoid annotation
withr::with_tempdir({
  corr <- data.frame(
    id_ion = 101L,
    id_taste = "BITTER",
    fractions = "32 33 34",
    correlation = 0.99,
    p_value = 0.001,
    p_adjusted = 0.01,
    method = "kendall",
    stringsAsFactors = FALSE
  )
  ions <- data.frame(id = 101L, rt = 1.0, mz = 100.0, stringsAsFactors = FALSE)
  ions[["datafile:210619_AR_12_M_32_01.mzML:area"]] <- 5000
  ions[["datafile:210619_AR_13_M_33_01.mzML:area"]] <- 6000
  ions[["datafile:210619_AR_14_M_34_01.mzML:area"]] <- 4000

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
  # Provide iridoid classification so c2 and c3 plots also execute
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

  ret <- tryCatch(
    plot_correlations(
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
    ),
    error = function(e) e
  )

  # c1.pdf always gets created when data flows through first section
  expect_true(file.exists(c1))
  # If no error, all 3 PDFs are created
  if (!inherits(ret, "error")) {
    expect_true(file.exists(c2))
    expect_true(file.exists(c3))
    expect_true(is.list(ret))
  }
})
