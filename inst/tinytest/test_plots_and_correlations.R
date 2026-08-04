library(tinytest)

message("Plot and correlation coverage checks")
ns <- asNamespace("sapid")
plot_chasselas_modulation <- get("plot_chasselas_modulation", envir = ns)
plot_chasselas_variation <- get("plot_chasselas_variation", envir = ns)
plot_concentration <- get("plot_concentration", envir = ns)
plot_descriptors_network <- get("plot_descriptors_network", envir = ns)
plot_matrices <- get("plot_matrices", envir = ns)
plot_informed_tasting <- get("plot_informed_tasting", envir = ns)
correlate_ion_taste_intensities <- get(
  "correlate_ion_taste_intensities",
  envir = ns
)

withr::with_tempdir({
  tmp_dir <- getwd()

  out_mod <- file.path(tmp_dir, "modulation.pdf")
  ret_mod <- plot_chasselas_modulation(output = out_mod)
  expect_identical(ret_mod, out_mod)
  expect_true(file.exists(out_mod))

  out_var_jury <- file.path(tmp_dir, "variation_jury.pdf")
  out_var_session <- file.path(tmp_dir, "variation_session.pdf")
  ret_var <- plot_chasselas_variation(
    output_jury = out_var_jury,
    output_session = out_var_session
  )
  expect_identical(ret_var$jury, out_var_jury)
  expect_identical(ret_var$session, out_var_session)
  expect_true(file.exists(out_var_jury))
  expect_true(file.exists(out_var_session))

  out_conc <- file.path(tmp_dir, "concentration.pdf")
  ret_conc <- plot_concentration(output = out_conc)
  expect_identical(ret_conc, out_conc)
  expect_true(file.exists(out_conc))

  out_network <- file.path(tmp_dir, "network.pdf")
  ret_network <- plot_descriptors_network(output = out_network)
  expect_true(!is.null(ret_network))
  expect_true(file.exists(out_network))

  out_matrices <- file.path(tmp_dir, "matrices.pdf")
  ret_matrices <- plot_matrices(output = out_matrices)
  expect_true(!is.null(ret_matrices))
  expect_true(file.exists(out_matrices))

  data(fractions, package = "sapid")
  data(concentration_afc, package = "sapid")
  data(profiles, package = "sapid")

  fractions_path <- file.path(tmp_dir, "fractions.tsv")
  concentration_path <- file.path(tmp_dir, "concentration_afc.tsv")
  profiles_path <- file.path(tmp_dir, "profiles.tsv")
  utils::write.table(
    x = as.data.frame(fractions),
    file = fractions_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  utils::write.table(
    x = as.data.frame(concentration_afc),
    file = concentration_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  utils::write.table(
    x = as.data.frame(profiles),
    file = profiles_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  out_informed <- file.path(tmp_dir, "informed.pdf")
  ret_informed <- plot_informed_tasting(
    file_fractions_mass = fractions_path,
    file_taste_raw = concentration_path,
    file_taste_informed = profiles_path,
    output = out_informed,
    min_panelists = 2L
  )
  expect_identical(ret_informed, out_informed)
  expect_true(file.exists(out_informed))

  ions <- data.frame(id = c(101L, 102L), rt = c(1, 2), mz = c(100.1, 200.2))
  for (fraction in 32:38) {
    ions[[sprintf(
      "datafile:210619_AR_%02d_M_%d_01.mzML:area",
      fraction - 20,
      fraction
    )]] <-
      c(fraction * 100, fraction * 120)
  }
  ions_path <- file.path(tmp_dir, "ions.csv")
  data.table::fwrite(ions, ions_path)

  corr_path <- file.path(tmp_dir, "correlations.tsv")
  ret_corr <- correlate_ion_taste_intensities(
    input_ions = ions_path,
    output = corr_path,
    min_jury = 1L,
    min_area_ion = 1L,
    widths = 2:3
  )
  expect_identical(ret_corr, corr_path)
  expect_true(file.exists(corr_path))

  corr <- data.table::fread(corr_path)
  expect_true(nrow(corr) > 0)
  expect_identical(
    names(corr),
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
  expect_true(all(corr$method %in% c("kendall", "pearson")))
})
