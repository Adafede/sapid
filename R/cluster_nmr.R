#' Cluster proton NMR spectra of fractions
#'
#' Load Bruker 1D proton NMR experiments, autophase, interpolate, exclude
#' solvent/artifact regions before total-area normalization, merge overlapping
#' detected peak positions within `peak_width_ppm / 2`, integrate peaks,
#' cluster fractions using Hellinger (Euclidean on square-root areas) or
#' Canberra distance with Ward's minimum-variance hierarchical linkage
#' (`ward.D2`), and export a dendrogram-aligned spectral figure and cluster
#' group table.
#'
#' @details
#' Four chemometric refinements are applied:
#' \enumerate{
#'   \item **Pre-normalization solvent exclusion**: `range_exclude_area` (e.g.,
#'     residual DMSO-d6 at 2.5--2.6 ppm) is excluded prior to
#'     `AlpsNMR::nmr_normalize()` so that solvent peak fluctuations do not
#'     distort total-area normalization across fractions.
#'   \item **Non-overlapping peak integration windows**: Detected peak positions
#'     closer than `peak_width_ppm / 2` are merged via single-linkage grouping
#'     to their mean chemical shift before integration, preventing double
#'     counting of multiplet splittings.
#'   \item **Zero-preserving Hellinger/Ward.D2 clustering**: Sub-threshold peak
#'     areas (`<= area_min`) are set to `0` (not `NA`) prior to the Hellinger
#'     square-root transformation so that absent peaks contribute well-defined
#'     zero coordinates compatible with Euclidean/Hellinger and Ward.D2
#'     minimum-variance geometry.
#'   \item **Dendrogram leaf-to-fraction alignment**: After sorting the
#'     dendrogram (`sort(dend)`), leaf colors (`lab.col`) are matched back to
#'     `frac_short` via `labels(dend)` so each fraction receives its true
#'     cluster color.
#' }
#'
#' @include colors.R
#' @concept EDAM:topic_0593 NMR
#' @concept EDAM:topic_3172 Metabolomics
#' @concept EDAM:operation_3214 Spectral analysis
#' @concept EDAM:operation_3432 Clustering
#' @concept EDAM:data_3488 NMR spectrum
#' @param nmr_dir Directory containing Bruker NMR experiment folders.
#' @param output_figure Output path for the PDF figure.
#' @param output_groups Output path for the TSV cluster groups table.
#' @param experiment_raw Identifier of the raw extract experiment.
#' @param experiment_ref Identifier of the reference experiment.
#' @param experiments_fractions Character vector of fraction experiment IDs.
#' @param experiments_to_fix Optional character vector of experiment IDs whose
#'   ppm axis should be aligned to `experiment_ref`.
#' @param k Integer number of hierarchical clusters (default `7`).
#' @param peak_width_ppm Numeric integration peak width in ppm (default `0.01`).
#' @param ppm_min Minimum chemical shift in ppm (default `0`).
#' @param ppm_max Maximum chemical shift in ppm (default `14.85`).
#' @param area_min Minimum integrated peak area threshold (default `0`).
#' @param range_exclude_area Numeric length-2 ppm range excluded from
#'   normalization and clustering.
#' @param range_without_peaks Numeric length-2 baseline noise ppm range.
#' @param ylim Numeric length-2 y-axis limits for spectral traces.
#' @param dist_method Distance metric on square-root-transformed peak areas:
#'   `"euclidean"` (default, Hellinger distance) or `"canberra"`.
#'
#' @return Named list with elements `figure` and `groups`.
#' @export
#' @examples
#' \dontrun{
#' cluster_nmr()
#' }
cluster_nmr <- function(
  nmr_dir = "./data/10043",
  output_figure = "./data/figures/figure_nmr.pdf",
  output_groups = "./data/groups.tsv",
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
  ylim = c(0, 5E10),
  dist_method = c("euclidean", "canberra")
) {
  dist_method <- match.arg(dist_method)
  dirnames <- nmr_dir |>
    dir(
      pattern = "proton_\\d",
      full.names = TRUE,
      recursive = TRUE,
      include.dirs = TRUE
    )
  experiments <- c(experiment_raw, experiments_fractions, experiment_ref)
  regions_to_exclude <- list(
    lower = c(-Inf, ppm_min),
    exclude_area = range_exclude_area,
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
      dataset_corrected$axis[[experiment_to_fix]][[
        1
      ]] <- dataset_corrected$axis[[experiment_ref]][[1]]
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
    AlpsNMR::nmr_interpolate_1D(
      axis = c(min = ppm_min, max = ppm_max, by = ppm_res)
    )

  message("Excluding ppm regions")
  dataset_excluded <- dataset_interpolated |>
    AlpsNMR::nmr_exclude_region(exclude = regions_to_exclude)

  message("Detecting peaks")
  peak_list_initial <- dataset_excluded |>
    AlpsNMR::nmr_detect_peaks(
      baselineThresh = NULL,
      range_without_peaks = range_without_peaks,
      SNR.Th = -1,
      fit_lorentzians = TRUE
    )

  message("Normalizing fractions")
  dataset_fractions <- dataset_excluded |>
    AlpsNMR::filter(NMRExperiment %in% experiments_fractions) |>
    AlpsNMR::nmr_normalize()

  raw_ppm <- sort(unique(as.numeric(peak_list_initial$ppm)))
  raw_ppm <- raw_ppm[!is.na(raw_ppm)]
  if (length(raw_ppm) > 1L && peak_width_ppm > 0) {
    ppm_clusters <- cumsum(c(1L, diff(raw_ppm) > (peak_width_ppm / 2)))
    merged_ppm <- as.numeric(tapply(raw_ppm, ppm_clusters, mean))
  } else {
    merged_ppm <- raw_ppm
  }

  message("Integrating peaks positions")
  peak_table_integration <- dataset_fractions |>
    AlpsNMR::nmr_integrate_peak_positions(
      peak_pos_ppm = merged_ppm,
      peak_width_ppm = peak_width_ppm,
      fix_baseline = TRUE,
      set_negative_areas_to_zero = TRUE
    )

  message("Formatting ...")
  meta_df <- as.data.frame(
    AlpsNMR::get_integration_with_metadata(peak_table_integration),
    stringsAsFactors = FALSE
  )
  frac_short <- gsub("proton_", "", experiments_fractions, fixed = TRUE)
  rownames(meta_df) <- experiments_fractions

  message("Filtering ...")
  is_num <- vapply(meta_df, is.numeric, logical(1L))
  num_mat <- as.matrix(meta_df[, is_num, drop = FALSE])
  ppm_cols <- suppressWarnings(as.numeric(
    gsub(
      "\\.\\.\\..*",
      "",
      gsub("ppm_", "", colnames(num_mat), fixed = TRUE)
    )
  ))

  valid_cols <- !is.na(ppm_cols) &
    (ppm_cols < range_exclude_area[1L] | ppm_cols > range_exclude_area[2L])
  num_mat <- num_mat[, valid_cols, drop = FALSE]
  num_mat[is.na(num_mat) | num_mat <= area_min] <- 0
  table_integration_filtered <- sqrt(num_mat)
  rownames(table_integration_filtered) <- frac_short

  hclust <- table_integration_filtered |>
    stats::dist(method = dist_method) |>
    stats::hclust(method = "ward.D2")

  paired_k <- paired[seq_len(min(k, length(paired)))]
  dend <- hclust |>
    stats::as.dendrogram() |>
    dendextend::set("branches_k_color", k = k, value = paired_k) |>
    dendextend::set("labels_colors", k = k, value = paired_k) |>
    dendextend::set("labels_cex", 1) |>
    sort()

  dend_attr <- dend |>
    dendextend::get_leaves_attr("nodePar")
  leaf_cols <- as.character(unlist(
    dend_attr[names(dend_attr) == "lab.col"],
    use.names = FALSE
  ))
  leaf_labels <- as.character(labels(dend))
  idx_match <- match(frac_short, leaf_labels)

  groups <- data.frame(
    group = leaf_cols[idx_match],
    rowname = frac_short,
    stringsAsFactors = FALSE
  )

  axis_ppm <- round(as.numeric(dataset_fractions$axis), digits = 3L)
  data_mat <- as.matrix(dataset_fractions$data_1r)
  summed_mat <- rowsum(t(data_mat), axis_ppm, reorder = TRUE, na.rm = TRUE)
  ppm_vals <- as.numeric(rownames(summed_mat))
  n_ppm <- length(ppm_vals)
  n_fr <- length(frac_short)

  table_xy_ready <- data.frame(
    rowname = rep(frac_short, each = n_ppm),
    name = rep(ppm_vals, times = n_fr),
    value = as.numeric(summed_mat),
    group = rep(groups$group, each = n_ppm),
    stringsAsFactors = FALSE
  )
  table_xy_ready$rowname <- fct_rev_base(table_xy_ready$rowname)

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
      n.breaks = max(2L, round(ppm_max - ppm_min))
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::theme_minimal() +
    ggplot2::xlab("ppm") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "#767676"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(color = "#767676"),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      legend.text = ggplot2::element_text(color = "#767676"),
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
        color = "#767676",
        size = 20
      )
    )

  plot_1 <- dend |>
    dendextend::as.ggdend() |>
    ggplot2::ggplot(horiz = TRUE, mapping = ggplot2::aes(size = 0.5)) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = 75,
        r = 0,
        b = 2,
        l = 0,
        "pt"
      )
    )

  .cascade_check_export_dir(dirname(output_figure))
  .cascade_check_export_dir(dirname(output_groups))
  ggpubr::ggarrange(plot_1, plot_2, widths = c(0.2, 1)) |>
    ggplot2::ggsave(
      filename = output_figure,
      width = 16,
      height = 12
    )
  data.table::fwrite(groups, file = output_groups, sep = "\t")

  list(figure = output_figure, groups = output_groups)
}
