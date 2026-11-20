library(tinytest)

fixture_dir <- normalizePath(
  if (dir.exists("fixtures")) {
    "fixtures"
  } else {
    file.path("inst", "tinytest", "fixtures")
  },
  mustWork = FALSE
)

fixture_path <- function(name) {
  file.path(fixture_dir, name)
}

build_test_xlsx <- function(
  sheets_xml = list(),
  extra_xml = list(),
  out_zip = tempfile(fileext = ".xlsx")
) {
  td <- tempfile("xlsx_build_")
  dir.create(file.path(td, "xl", "worksheets"), recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  for (i in seq_along(sheets_xml)) {
    writeLines(
      enc2utf8(sheets_xml[[i]]),
      file.path(td, "xl", "worksheets", paste0("sheet", i, ".xml")),
      useBytes = TRUE
    )
  }
  for (rel_path in names(extra_xml)) {
    dest <- file.path(td, rel_path)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    writeLines(enc2utf8(extra_xml[[rel_path]]), dest, useBytes = TRUE)
  }
  out_abs <- file.path(
    normalizePath(dirname(out_zip), mustWork = FALSE),
    basename(out_zip)
  )
  withr::with_dir(td, utils::zip(out_abs, files = "xl", flags = "-rq"))
  out_zip
}

make_minimal_annotation_df <- function(num_val = 0.5) {
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
  ann <- as.data.frame(
    stats::setNames(
      lapply(all_cols, function(x) {
        if (x == "feature_id") {
          101L
        } else if (x %in% num_cols) {
          num_val
        } else {
          NA_character_
        }
      }),
      all_cols
    ),
    stringsAsFactors = FALSE
  )
  ann$mode <- "pos"
  ann
}

make_synthetic_mzml <- function(include_spectra = FALSE) {
  e <- new.env(parent = emptyenv())
  utils::data("cascade_chromatograms_positive", package = "cascade", envir = e)
  df_cad <- e$cascade_chromatograms_positive
  cad <- df_cad[
    !is.na(df_cad$chromatogram) & df_cad$chromatogram == "cad",
  ][seq(1, 49468, by = 20), ]
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
          paste0(
            "<cvParam cvRef=\"MS\" accession=\"MS:1000595\"",
            " name=\"time array\" unitCvRef=\"UO\"",
            " unitAccession=\"UO:0000031\" unitName=\"minute\"/>"
          ),
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
  spec_xml <- ""
  if (include_spectra) {
    mz_b64 <- enc_f64(c(150.0, 250.0))
    sp_int_b64 <- enc_f64(c(200000.0, 300000.0))
    rts <- c("5.0", "5.5")
    specs <- vapply(
      seq_along(rts),
      function(idx) {
        paste0(
          "<spectrum index=\"",
          idx - 1L,
          "\" id=\"scan=",
          idx,
          "\" defaultArrayLength=\"2\">",
          "<cvParam cvRef=\"MS\" accession=\"MS:1000511\" name=\"ms level\" value=\"1\"/>",
          paste0(
            "<scanList count=\"1\"><scan><cvParam cvRef=\"MS\"",
            " accession=\"MS:1000016\" name=\"scan start time\" value=\"",
            rts[[idx]],
            "\" unitCvRef=\"UO\" unitAccession=\"UO:0000031\"",
            " unitName=\"minute\"/></scan></scanList>"
          ),
          "<binaryDataArrayList count=\"2\">",
          "<binaryDataArray encodedLength=\"",
          nchar(mz_b64),
          paste0(
            "\"><cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\"/>",
            "<cvParam cvRef=\"MS\" accession=\"MS:1000576\" name=\"no compression\"/>",
            "<cvParam cvRef=\"MS\" accession=\"MS:1000514\" name=\"m/z array\"/><binary>"
          ),
          mz_b64,
          "</binary></binaryDataArray>",
          "<binaryDataArray encodedLength=\"",
          nchar(sp_int_b64),
          paste0(
            "\"><cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\"/>",
            "<cvParam cvRef=\"MS\" accession=\"MS:1000576\" name=\"no compression\"/>",
            "<cvParam cvRef=\"MS\" accession=\"MS:1000515\" name=\"intensity array\"/><binary>"
          ),
          sp_int_b64,
          "</binary></binaryDataArray>",
          "</binaryDataArrayList></spectrum>\n"
        )
      },
      character(1L)
    )
    spec_xml <- paste0(
      "<spectrumList count=\"2\" defaultDataProcessingRef=\"dp\">\n",
      paste(specs, collapse = ""),
      "</spectrumList>\n"
    )
  }
  paste0(
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
    "<mzML xmlns=\"http://psi.hupo.org/ms/mzml\" version=\"1.1.0\">\n",
    "<cvList count=\"1\"><cv id=\"MS\" fullName=\"MS\" version=\"1\" URI=\"\"/></cvList>\n",
    "<run id=\"run1\">\n",
    spec_xml,
    "<chromatogramList count=\"3\" defaultDataProcessingRef=\"dp\">\n",
    chrom_xml,
    "\n</chromatogramList>\n</run>\n</mzML>\n"
  )
}

expect_valid_pdf <- function(path) {
  expect_true(file.exists(path))
  expect_true(file.info(path)$size > 1000)
  expect_identical(rawToChar(readBin(path, "raw", 4L)), "%PDF")
}

capture_warnings <- function(expr) {
  warn_env <- new.env(parent = emptyenv())
  warn_env$msgs <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warn_env$msgs <- c(warn_env$msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warn_env$msgs)
}
