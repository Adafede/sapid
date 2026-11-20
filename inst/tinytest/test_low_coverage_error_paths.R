library(tinytest)
source(
  if (file.exists("helpers.R")) {
    "helpers.R"
  } else {
    file.path("inst", "tinytest", "helpers.R")
  },
  local = TRUE
)

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

  ann <- make_minimal_annotation_df(num_val = 0.5)

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

fixture <- fixture_path("session_fixture.xlsx")
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
  mzml_txt <- make_synthetic_mzml(include_spectra = FALSE)

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

  ds_orig <- AlpsNMR::nmr_read_samples(
    src,
    format = "bruker",
    all_components = TRUE
  )
  ax <- ds_orig$axis[[1L]][[1L]]
  idx <- which(ax <= 3.0 & ax >= 1.0)[seq(1L, 8724L, by = 8L)]

  make_slice_bruker <- function(src_dir, dst_dir, idx_vec, shift_idx = 0L) {
    dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(
      list.files(src_dir, full.names = TRUE),
      dst_dir,
      recursive = TRUE
    )
    p_dir <- file.path(dst_dir, "pdata", "1")
    idx_s <- idx_vec + shift_idx
    for (fn in c("1r", "1i")) {
      fp <- file.path(p_dir, fn)
      if (file.exists(fp)) {
        vals <- readBin(
          fp,
          what = "integer",
          n = 131072L,
          size = 4L,
          endian = "little"
        )
        writeBin(as.integer(vals[idx_s]), fp, size = 4L, endian = "little")
      }
    }
    procs_fp <- file.path(p_dir, "procs")
    lines <- readLines(procs_fp)
    lines <- sub("^##\\$SI=.*", paste0("##$SI= ", length(idx_vec)), lines)
    lines <- sub("^##\\$OFFSET=.*", "##$OFFSET= 3.0", lines)
    lines <- sub("^##\\$SW_p=.*", "##$SW_p= 1200.06", lines)
    writeLines(lines, procs_fp)
  }

  nmr_dir <- file.path(getwd(), "nmr_samples")
  dir.create(nmr_dir)
  make_slice_bruker(src, file.path(nmr_dir, "proton_00"), idx, 0L)
  make_slice_bruker(src, file.path(nmr_dir, "proton_17"), idx, 0L)
  make_slice_bruker(src, file.path(nmr_dir, "proton_18"), idx, 100L)
  make_slice_bruker(src, file.path(nmr_dir, "proton_19"), idx, 200L)
  make_slice_bruker(src, file.path(nmr_dir, "proton_98"), idx, 0L)

  out_fig <- file.path(getwd(), "nmr_fig.pdf")
  out_grp <- file.path(getwd(), "nmr_groups.tsv")
  ret_nmr <- cluster_nmr(
    nmr_dir = nmr_dir,
    output_figure = out_fig,
    output_groups = out_grp,
    experiment_raw = "proton_00",
    experiment_ref = "proton_98",
    experiments_fractions = c("proton_17", "proton_18", "proton_19"),
    experiments_to_fix = "proton_19",
    k = 2,
    ppm_min = 1.0,
    ppm_max = 2.4,
    area_min = 0,
    range_exclude_area = c(1.5, 1.6),
    range_without_peaks = c(2.2, 2.4)
  )
  expect_identical(ret_nmr, list(figure = out_fig, groups = out_grp))
  expect_true(file.exists(out_fig) && file.info(out_fig)$size > 0)
  expect_true(file.exists(out_grp))
  grp_df <- utils::read.delim(out_grp, stringsAsFactors = FALSE)
  expect_identical(names(grp_df), c("group", "rowname"))
  expect_identical(nrow(grp_df), 3L)
  expect_identical(sort(grp_df$rowname), 17:19)
})
