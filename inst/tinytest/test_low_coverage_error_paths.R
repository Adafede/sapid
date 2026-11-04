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

# Full happy path for plot_chromatograms with .mzML files
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
    "<chromatogramList count=\"3\" defaultDataProcessingRef=\"dp\">\n",
    chrom_xml,
    "\n</chromatogramList>\n</run>\n</mzML>\n"
  )

  mzml_dir <- file.path(getwd(), "chrom_mzmls")
  dir.create(mzml_dir)
  writeLines(mzml_txt, file.path(mzml_dir, "210619_AR_01_M_1_01.mzML"))
  writeLines(mzml_txt, file.path(mzml_dir, "210619_AR_02_M_17_01.mzML"))

  out_chrom <- file.path(getwd(), "figure_chromatograms.pdf")
  ret_chrom <- plot_chromatograms(
    input_dir = mzml_dir,
    input_groups = NULL,
    raw_index = 1L,
    fractions_indices = 2L,
    xlim = c(2, 25),
    output = out_chrom
  )
  expect_identical(ret_chrom, out_chrom)
  expect_true(file.exists(out_chrom) && file.info(out_chrom)$size > 0)
})

# Full happy path for cluster_nmr with Bruker spectra and narrow ppm window
withr::with_tempdir({
  z10 <- system.file("dataset-demo", "10.zip", package = "AlpsNMR")
  base_dir <- file.path(getwd(), "bruker_base")
  dir.create(base_dir)
  utils::unzip(z10, exdir = base_dir)
  src <- file.path(base_dir, "10")

  r_full <- readBin(
    file.path(src, "pdata", "1", "1r"),
    what = "int",
    n = 65536L,
    size = 4L,
    endian = "little"
  )
  procs_lines <- readLines(file.path(src, "pdata", "1", "procs"))
  off <- as.numeric(sub(
    ".*= ",
    "",
    grepv("^##\\$OFFSET=", procs_lines)
  ))
  sw_p <- as.numeric(sub(
    ".*= ",
    "",
    grepv("^##\\$SW_p=", procs_lines)
  ))
  sf <- as.numeric(sub(
    ".*= ",
    "",
    grepv("^##\\$SF=", procs_lines)
  ))
  sw <- sw_p / sf
  ppm_full <- seq(from = off, to = off - sw, length.out = 65536L)

  keep_idx <- which(ppm_full <= 3.0 & ppm_full >= 1.0)
  keep_idx <- keep_idx[seq(1L, length(keep_idx), by = 8L)]
  n_pts <- length(keep_idx)
  new_off <- ppm_full[keep_idx[1L]]
  new_sw_p <- (ppm_full[keep_idx[1L]] - ppm_full[keep_idx[n_pts]]) * sf

  procs_mod <- procs_lines
  procs_mod[grep("^##\\$SI=", procs_mod)] <- paste0("##$SI= ", n_pts)
  procs_mod[grep("^##\\$OFFSET=", procs_mod)] <- paste0("##$OFFSET= ", new_off)
  procs_mod[grep("^##\\$SW_p=", procs_mod)] <- paste0("##$SW_p= ", new_sw_p)
  writeLines(procs_mod, file.path(src, "pdata", "1", "procs"))

  nmr_dir <- file.path(getwd(), "nmr_samples")
  dir.create(nmr_dir)
  scales <- c(1.0, 1.0, 1.3, 0.7, 2.0, 2.5)
  exps <- c(
    "proton_00",
    "proton_98",
    "proton_17",
    "proton_18",
    "proton_19",
    "proton_20"
  )
  for (i in seq_along(exps)) {
    dest <- file.path(nmr_dir, exps[i])
    dir.create(dest)
    file.copy(
      list.files(src, full.names = TRUE),
      dest,
      recursive = TRUE
    )
    r_slice <- as.integer(round(
      r_full[keep_idx] * scales[i] + (i * 500L) * ((-1L)^i)
    ))
    writeBin(
      r_slice,
      file.path(dest, "pdata", "1", "1r"),
      size = 4L,
      endian = "little"
    )
  }

  out_fig <- file.path(getwd(), "nmr_fig.pdf")
  out_grp <- file.path(getwd(), "nmr_groups.tsv")
  ret_nmr <- cluster_nmr(
    nmr_dir = nmr_dir,
    output_figure = out_fig,
    output_groups = out_grp,
    experiment_raw = "proton_00",
    experiment_ref = "proton_98",
    experiments_fractions = c(
      "proton_17",
      "proton_18",
      "proton_19",
      "proton_20"
    ),
    experiments_to_fix = "proton_17",
    k = 2,
    ppm_min = 1.0,
    ppm_max = 2.0,
    area_min = 0,
    range_exclude_area = c(1.5, 1.55),
    range_without_peaks = c(2.7, 2.9)
  )
  expect_identical(ret_nmr, list(figure = out_fig, groups = out_grp))
  expect_true(file.exists(out_fig) && file.info(out_fig)$size > 0)
  expect_true(file.exists(out_grp))
  grp_df <- utils::read.delim(out_grp, stringsAsFactors = FALSE)
  expect_identical(names(grp_df), c("group", "rowname"))
  expect_identical(nrow(grp_df), 4L)
  expect_identical(sort(grp_df$rowname), 17:20)
})
