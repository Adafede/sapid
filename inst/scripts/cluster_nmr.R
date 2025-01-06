start <- Sys.time()

pkgload::load_all()

message("This program clusters proton NMR spectra.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Cluster NMR
#'
#' @include colors.R
#'
#' @param nmr_dir NMR directory
#' @param output_figure Output figure
#' @param output_groups Output groups
#' @param experiment_raw Experiment raw
#' @param experiment_ref Experiment ref
#' @param experiments_fractions Experiments fractions
#' @param experiments_to_fix Experiments to fix
#' @param k Number of clusters
#' @param peak_width_ppm Peak width ppm
#' @param ppm_min Ppm min
#' @param ppm_max Ppm max
#' @param area_min Area min
#' @param range_exclude_area Range exclude area
#' @param range_without_peaks Range without peaks
#' @param ylim Y lim
#'
#' @return NULL
#'
#' @examples NULL
cluster_nmr <- function(nmr_dir = "./data/10043",
                        output_figure = "./data/figures/figure_nmr.pdf",
                        output_groups = "./inst/extdata/groups.tsv",
                        experiment_raw = "proton_00",
                        experiment_ref = "proton_98",
                        experiments_fractions = c(
                          "proton_17",
                          "proton_18",
                          "proton_19",
                          "proton_20",
                          "proton_21",
                          "proton_22",
                          "proton_23",
                          "proton_24",
                          "proton_25",
                          "proton_26",
                          "proton_27",
                          "proton_28",
                          "proton_29",
                          "proton_30",
                          "proton_31",
                          "proton_32",
                          "proton_33",
                          "proton_34",
                          "proton_35",
                          "proton_36",
                          "proton_37",
                          "proton_38",
                          "proton_39",
                          "proton_40",
                          "proton_41",
                          "proton_42",
                          "proton_43",
                          "proton_44",
                          "proton_45",
                          "proton_46",
                          "proton_47",
                          "proton_48",
                          "proton_50",
                          "proton_51",
                          "proton_52",
                          "proton_53",
                          "proton_54",
                          "proton_55",
                          "proton_56",
                          "proton_57",
                          "proton_58",
                          "proton_59",
                          "proton_60",
                          "proton_61",
                          "proton_62",
                          "proton_63",
                          "proton_64",
                          "proton_65",
                          "proton_66",
                          "proton_67",
                          "proton_68",
                          "proton_69",
                          "proton_70",
                          "proton_71"
                        ),
                        experiments_to_fix = c("proton_69", "proton_70", "proton_71"),
                        k = 7,
                        peak_width_ppm = 0.01,
                        ppm_min = 0,
                        ppm_max = 14.85,
                        area_min = 0,
                        range_exclude_area = c(2.5, 2.6),
                        range_without_peaks = c(0, 0.4),
                        ylim = c(0, 5E10)) {
  dirnames <- nmr_dir |>
    dir(
      pattern = "proton_\\d",
      full.names = TRUE,
      recursive = TRUE,
      include.dirs = TRUE
    )
  experiments <- c(experiment_raw, experiments_fractions, experiment_ref)
  # experiments_subset <- c(
  #   experiment_raw,
  #   sample(x = experiments_fractions, size = 8),
  #   experiment_ref
  # )
  regions_to_exclude <- list(
    lower = c(-Inf, ppm_min),
    # dmso = c(2.3, 2.6),
    # water = c(3.23, 3.53),
    upper = c(ppm_max, Inf)
  )

  message("Loading NMR experiments")
  dataset <- AlpsNMR::nmr_read_samples(
    sample_names = dirnames,
    format = "bruker",
    all_components = TRUE
  )

  message("Filtering desired experiments")
  dataset_filtered <- dataset |>
    AlpsNMR::filter(NMRExperiment %in% experiments)

  dataset_corrected <- dataset_filtered

  if (!is.null(experiments_to_fix)) {
    message("Correcting experiments to fix")
    for (experiment_to_fix in experiments_to_fix) {
      dataset_corrected$axis[[experiment_to_fix]][[1]] <- dataset_corrected$axis[[experiment_ref]][[1]]
    }
  }

  message("Phasing automatically experiments")
  dataset_autophased <- dataset_corrected |>
    AlpsNMR::nmr_autophase(withBC = TRUE)

  ppm_res <- AlpsNMR::nmr_ppm_resolution(dataset_autophased)[[1]]
  message(
    "The ppm resolution of the experiments is: ",
    format(ppm_res, digits = 2),
    " ppm. \n",
    "Using it for interplation ..."
  )

  message("Interpolating experiments")
  dataset_interpolated <- dataset_autophased |>
    AlpsNMR::nmr_interpolate_1D(axis = c(min = ppm_min, max = ppm_max, by = ppm_res))

  message("Excluding ppm regions")
  dataset_excluded <- dataset_interpolated |>
    AlpsNMR::nmr_exclude_region(exclude = regions_to_exclude)

  # message("Baselining experiments")
  # dataset_baselined <- dataset_excluded |>
  #   AlpsNMR::nmr_baseline_estimation()
  #
  # message("Estimating threshold")
  # baselineThresh <- dataset_baselined |>
  #   AlpsNMR::nmr_baseline_threshold(range_without_peaks = range_without_peaks, method = "median3mad")

  message("Detecting peaks")
  peak_list_initial <- dataset_excluded |>
    AlpsNMR::nmr_detect_peaks(
      baselineThresh = NULL,
      range_without_peaks = range_without_peaks,
      SNR.Th = -1,
      fit_lorentzians = TRUE
    )

  # message("Plotting detected peaks overview")
  # peak_list_initial |>
  #   AlpsNMR::nmr_detect_peaks_plot_overview()

  message("Normalizing fractions")
  dataset_fractions <- dataset_excluded |>
    AlpsNMR::filter(NMRExperiment %in% experiments_fractions) |>
    AlpsNMR::nmr_normalize()

  message("Integrating peaks positions")
  peak_table_integration <- dataset_fractions |>
    AlpsNMR::nmr_integrate_peak_positions(
      peak_pos_ppm = peak_list_initial$ppm,
      peak_width_ppm = peak_width_ppm,
      fix_baseline = TRUE,
      set_negative_areas_to_zero = TRUE
    )

  message("Formatting ...")
  peak_table_integration_ready <- peak_table_integration |>
    AlpsNMR::get_integration_with_metadata() |>
    tibble::column_to_rownames("NMRExperiment")
  table_integration <- peak_table_integration_ready |>
    tidytable::tidytable()
  rownames(table_integration) <- experiments_fractions |>
    gsub(pattern = "proton_", replacement = "")

  message("Filtering ...")
  table_integration_filtered <- table_integration |>
    tibble::rownames_to_column() |>
    tidytable::pivot_longer(cols = tidytable::where(is.numeric)) |>
    tidytable::mutate(
      name = name |> gsub(
        pattern = "ppm_",
        replacement = "",
        fixed = TRUE
      ) |>
        gsub(pattern = "\\.\\.\\..*", replacement = "") |>
        as.numeric()
    ) |>
    tidytable::filter(!is.na(name)) |>
    tidytable::filter(value > area_min) |>
    tidytable::filter(name < range_exclude_area[1] |
      name > range_exclude_area[2]) |>
    tidytable::group_by(rowname, name) |>
    tidytable::summarize(value = value |>
      sqrt()) |>
    tidytable::ungroup() |>
    tidytable::pivot_wider(names_from = name, values_from = value) |>
    tibble::column_to_rownames()

  hclust <-
    table_integration_filtered |>
    stats::dist(method = "canberra") |>
    stats::hclust(method = "ward.D2")

  dend <- hclust |>
    stats::as.dendrogram() |>
    dendextend::set("branches_k_color", k = k, value = paired) |>
    dendextend::set("labels_colors", k = k, value = paired) |>
    dendextend::set("labels_cex", 1) |>
    sort()

  dend_attr <- dend |>
    dendextend::get_leaves_attr("nodePar")

  groups <- dend_attr[names(dend_attr) == "lab.col"] |>
    data.frame() |>
    tidytable::rename(group = 1) |>
    tidytable::mutate(group = group |>
      as.character())
  groups$rowname <- experiments_fractions |>
    gsub(
      pattern = "proton_",
      replacement = "",
      fixed = TRUE
    )

  table_xy <- dataset_fractions$data_1r |>
    data.frame()
  colnames(table_xy) <- dataset_fractions$axis
  rownames(table_xy) <- experiments_fractions

  table_xy_ready <- table_xy |>
    tibble::rownames_to_column() |>
    tidytable::pivot_longer(cols = tidytable::where(is.numeric))

  table_xy_ready <- table_xy_ready |>
    # tidytable::slice_sample(n = 100000) |>
    tidytable::group_by(rowname) |>
    # tidytable::mutate(max = value |>
    #                     max()) |>
    # tidytable::mutate(value = value / max) |>
    tidytable::ungroup() |>
    tidytable::mutate(name = name |>
      as.numeric() |>
      round(digits = 3)) |>
    tidytable::group_by(rowname, name) |>
    tidytable::summarize(value = sum(value)) |>
    tidytable::ungroup() |>
    tidytable::mutate(rowname = rowname |>
      gsub(
        pattern = "proton_",
        replacement = "",
        fixed = TRUE
      )) |>
    tidytable::inner_join(groups) |>
    data.frame()

  table_xy_ready$rowname <- forcats::fct_rev(table_xy_ready$rowname)

  plot_2 <- table_xy_ready |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = name,
        y = value,
        group = group,
        color = group,
        fill = group,
        label = rowname
      )
    ) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::facet_grid(rowname ~ .) +
    ggplot2::ylim(ylim) +
    ggplot2::scale_x_reverse(
      limits = c(round(ppm_max), round(ppm_min)),
      n.breaks = round(ppm_max - ppm_min)
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::theme_minimal() +
    ggplot2::xlab("ppm") +
    # ggplot2::ylab("Intensity") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "grey30"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(color = "grey30"),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      legend.text = ggplot2::element_text(color = "grey30"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(x = -100, units = "pt"),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      text = ggplot2::element_text(
        face = "bold",
        color = "grey30",
        size = 20
      )
    )

  plot_2

  plot_1 <- dend |>
    dendextend::as.ggdend() |>
    ggplot2::ggplot(horiz = TRUE, mapping = ggplot2::aes(size = 0.5)) +
    ggplot2::theme(plot.margin = ggplot2::margin(
      t = 75,
      r = 0,
      b = 2,
      l = 0,
      "pt"
    ))
  plot_1

  cascade:::check_export_dir(output_figure)
  cascade:::check_export_dir(output_groups)
  ggpubr::ggarrange(plot_1, plot_2, widths = c(0.2, 1)) |>
    ggplot2::ggsave(
      filename = output_figure,
      width = 16,
      height = 12
    )
  groups |>
    tidytable::fwrite(file = output_groups, sep = "\t")

  return(output)
}

cluster_nmr()

end <- Sys.time()

message("Script finished in ", format(end - start))
