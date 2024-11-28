#' Cluster NMR
#'
#' @param todo todo
#'
#' @return NULL
#'
#' @examples NULL
cluster_nmr <- function(todo = NULL) {
  BiocParallel::register(
    BPPARAM = BiocParallel::MulticoreParam(exportglobals = FALSE),
    default = TRUE
  )

  nmr_dir <- "~/../../Volumes/LaCie/Adriano/06_data/NMR/10043"

  dirnames <- nmr_dir |>
    dir(
      pattern = "proton_\\d",
      full.names = TRUE,
      recursive = TRUE,
      include.dirs = TRUE
    )
  # dataset <- AlpsNMR::nmr_read_samples(sample_names = dirnames,
  #                                      format = "bruker",
  #                                      all_components = FALSE)
  dataset <- AlpsNMR::nmr_read_samples(
    sample_names = dirnames,
    format = "bruker",
    all_components = TRUE
  )
  experiment_raw <- "proton_00"
  experiment_ref <- "proton_98"
  experiments_fractions <- c(
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
  )
  experiments <- c(experiment_raw, experiments_fractions, experiment_ref)
  experiments_close_water <- c(
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
    "proton_35"
  )
  experiments_close_2 <- c("proton_43", "proton_44", "proton_45", "proton_46")

  experiments_subset <- c(
    experiment_raw,
    "proton_17",
    "proton_27",
    "proton_37",
    "proton_47",
    "proton_57",
    "proton_67",
    experiment_ref
  )

  dataset_filtered <- dataset |>
    AlpsNMR::filter(NMRExperiment %in% experiments)

  dataset_corrected <- dataset_filtered

  dataset_corrected$axis[["proton_69"]][[1]] <- dataset_corrected$axis[["proton_23"]][[1]]
  dataset_corrected$axis[["proton_70"]][[1]] <- dataset_corrected$axis[["proton_23"]][[1]]
  dataset_corrected$axis[["proton_71"]][[1]] <- dataset_corrected$axis[["proton_23"]][[1]]

  # dataset_autophased <- dataset_corrected

  dataset_autophased <- dataset_corrected |>
    AlpsNMR::nmr_autophase(withBC = TRUE)

  ppm_res <- AlpsNMR::nmr_ppm_resolution(dataset_autophased)[[1]]

  message(
    "The ppm resolution is: ",
    format(ppm_res, digits = 2),
    " ppm. \n",
    "Using it for interplation"
  )
  ppm_min <- 0
  ppm_max <- 14.85
  dataset_interpolated <- dataset_autophased |>
    AlpsNMR::nmr_interpolate_1D(axis = c(min = ppm_min, max = ppm_max, by = ppm_res))

  regions_to_exclude <- list(
    lower = c(-Inf, ppm_min),
    # dmso = c(2.3, 2.6),
    # water = c(3.23, 3.53),
    upper = c(ppm_max, Inf)
  )
  dataset_excluded <- dataset_interpolated |>
    AlpsNMR::nmr_exclude_region(exclude = regions_to_exclude)

  dataset_baselined <- dataset_excluded |>
    AlpsNMR::nmr_baseline_estimation()

  range_without_peaks <- c(0, 0.4)
  baselineThresh <- dataset_baselined |>
    AlpsNMR::nmr_baseline_threshold(range_without_peaks = range_without_peaks, method = "median3mad")

  dataset_baselined |>
    AlpsNMR::nmr_baseline_threshold_plot(thresholds = baselineThresh, chemshift_range = range_without_peaks)

  peak_list_initial <- dataset_excluded |>
    AlpsNMR::nmr_detect_peaks(
      baselineThresh = NULL,
      range_without_peaks = range_without_peaks,
      SNR.Th = -1,
      fit_lorentzians = TRUE
    )

  peak_list_initial |>
    AlpsNMR::nmr_detect_peaks_plot_overview()

  dataset_fractions <- dataset_excluded |>
    AlpsNMR::filter(NMRExperiment %in% experiments_fractions) |>
    AlpsNMR::nmr_normalize()

  peak_table_integration <- dataset_fractions |>
    AlpsNMR::nmr_integrate_peak_positions(
      peak_pos_ppm = peak_list_initial$ppm,
      peak_width_ppm = 0.01,
      fix_baseline = TRUE,
      set_negative_areas_to_zero = TRUE
    )

  peak_table_integration_ready <- peak_table_integration |>
    AlpsNMR::get_integration_with_metadata() |>
    tibble::column_to_rownames("NMRExperiment")

  table_integration <- peak_table_integration_ready |>
    tidytable::tidytable()
  rownames(table_integration) <- experiments_fractions |>
    gsub(pattern = "proton_", replacement = "")

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
    tidytable::filter(value > 0) |>
    tidytable::filter(name < 2.5 | name > 2.6) |>
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
    dendextend::set("branches_k_color", k = 7, value = paired) |>
    dendextend::set("labels_colors", k = 7, value = paired) |>
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

  groups <- cutree(hclust, k = 7) |>
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
    # directlabels::geom_dl(method = list(
    #   directlabels::dl.trans(x = round(x - 0.2)),
    #   "first.points",
    #   cex = 0.5,
    #   fontface = 'bold'
    # )) +
    ggplot2::facet_grid(rowname ~ .) +
    ggplot2::ylim(c(0, 5E10)) +
    ggplot2::scale_x_reverse(limits = c(14, 0), n.breaks = 14) +
    ggplot2::scale_color_identity() +
    ggplot2::theme_minimal() +
    ggplot2::xlab("ppm") +
    # ggplot2::ylab("Intensity") +
    ggplot2::theme(
      axis.text = ggtext::element_markdown(color = "grey30"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggtext::element_markdown(color = "grey30"),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      legend.text = ggtext::element_markdown(color = "grey30"),
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

  plot <- ggpubr::ggarrange(plot_1, plot_2, widths = c(0.2, 1))
  plot |> ggplot2::ggsave(
    filename = "protons.svg",
    width = 16,
    height = 12
  )
}
