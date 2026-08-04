library(tinytest)

message("Low-coverage function path checks")
ns <- asNamespace("sapid")
cluster_nmr <- get("cluster_nmr", envir = ns)
plot_chromatograms <- get("plot_chromatograms", envir = ns)
plot_profiles <- get("plot_profiles", envir = ns)
plot_correlations <- get("plot_correlations", envir = ns)
prepare_chemical_profiles <- get("prepare_chemical_profiles", envir = ns)
prepare_concentration <- get("prepare_concentration", envir = ns)

withr::with_tempdir({
  empty_dir <- file.path(getwd(), "empty_mzml")
  dir.create(empty_dir)
  ret <- prepare_chemical_profiles(
    mzmls_dir = empty_dir,
    peaks_dir_extract = file.path(getwd(), "peaks_extract"),
    peaks_dir_fractions = file.path(getwd(), "peaks_fractions"),
    features_path_extract = file.path(getwd(), "features_extract.csv"),
    features_path_fractions = file.path(getwd(), "features_fractions.csv"),
    start_extract = 1L,
    end_extract = 0L,
    start_fractions = 1L,
    end_fractions = 0L
  )
  expect_true(is.null(ret))
})

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
        0.5
      } else {
        NA_character_
      }
    }),
    all_cols
  ))
  ann$mode <- "pos"

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

  expect_error(
    plot_correlations(
      input_correlations = corr_path,
      input_ions = ions_path,
      annotation_path_fractions = ann_path,
      output_1 = file.path(getwd(), "c1.pdf"),
      output_2 = file.path(getwd(), "c2.pdf"),
      output_3 = file.path(getwd(), "c3.pdf"),
      widths = 2:3,
      min_corr = 0.0,
      max_pval = 1.0,
      min_intense_ions_ratio = 0.01,
      min_jury = 1L,
      min_area_ion = 1L
    ),
    "fractions.x"
  )
  expect_true(file.exists(file.path(getwd(), "c1.pdf")))
})

fixture <- file.path("fixtures", "session_fixture.xlsx")
if (!file.exists(fixture)) {
  fixture <- file.path("inst", "tinytest", "fixtures", "session_fixture.xlsx")
}
expect_true(file.exists(fixture))
expect_error(
  prepare_concentration(
    input_xlsx = fixture,
    output = tempfile(fileext = ".tsv")
  ),
  "concentration"
)

withr::with_tempdir({
  expect_error(
    cluster_nmr(
      nmr_dir = getwd(),
      output_figure = file.path(getwd(), "nmr.pdf"),
      output_groups = file.path(getwd(), "groups.tsv"),
      experiments_fractions = "proton_17",
      experiments_to_fix = "proton_17"
    ),
    "No samples to load"
  )
})

withr::with_tempdir({
  groups_path <- file.path(getwd(), "groups.tsv")
  utils::write.table(
    data.frame(group = "x", rowname = "1", stringsAsFactors = FALSE),
    file = groups_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  expect_error(
    plot_chromatograms(
      input_dir = getwd(),
      input_groups = groups_path,
      raw_index = 1L,
      fractions_indices = 1L,
      output = file.path(getwd(), "chrom.pdf")
    ),
    "File NA not found"
  )
})

withr::with_tempdir({
  ann_path <- file.path(getwd(), "ann.tsv")
  utils::write.table(
    data.frame(
      feature_id = integer(0),
      score_input = numeric(0),
      candidate_structure_inchikey_connectivity_layer = character(0),
      candidate_structure_smiles_no_stereo = character(0),
      candidate_structure_molecular_formula = character(0),
      candidate_structure_name = character(0),
      stringsAsFactors = FALSE
    ),
    file = ann_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  feat_path <- file.path(getwd(), "features.csv")
  utils::write.table(
    data.frame(id = 1L, check.names = FALSE),
    feat_path,
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )
  dir.create("peaks")
  expect_error(
    plot_profiles(
      output = file.path(getwd(), "profiles.pdf"),
      annotation_path_fractions = ann_path,
      features_path_fractions = feat_path,
      peaks_dir_fractions = file.path(getwd(), "peaks")
    ),
    "At least one column must be supplied to cols"
  )
})
