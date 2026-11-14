#' Plot sensory and chemical class profiles across fractions
#'
#' Generate a 2-panel stacked bar chart comparing panel-consistent sensory
#' taste intensity profiles (top) with CAD-informed chemical class profiles
#' annotated by TIMA (bottom) across chromatographic fractions.
#'
#' @concept EDAM:topic_3172 Metabolomics
#' @concept EDAM:topic_0092 Data visualisation
#' @concept EDAM:operation_3803 Natural product identification
#' @concept EDAM:operation_0337 Visualisation
#' @param input Optional path or `data.frame` of sensory profiles (defaults to
#'   built-in [profiles]).
#' @param output Output PDF file path.
#' @param annotation_path_extract Path to TIMA annotations for raw extract.
#' @param annotation_path_fractions Path to TIMA annotations for fractions.
#' @param features_path_extract Path to mzmine feature table for raw extract.
#' @param features_path_fractions Path to mzmine feature table for fractions.
#' @param peaks_dir_extract Directory of CAD peak comparison tables for extract.
#' @param peaks_dir_fractions Directory of CAD peak comparison tables for
#'   fractions.
#' @param min_confidence Minimum TIMA final confidence score (default `0.45`).
#' @param min_similarity_prefilter Minimum peak shape correlation prefilter
#'   (default `0.6`).
#' @param min_similarity_filter Minimum peak shape correlation filter
#'   (default `0.8`).
#' @param mode Ionization mode (`"pos"` or `"neg"`, default `"pos"`).
#' @param detector Detector identifier (default `"ms"`).
#' @param type Hierarchy analysis type (default `"analysis"`).
#'
#' @return Output file path (`output`).
#' @export
#' @examples
#' \dontrun{
#' plot_profiles()
#' }
plot_profiles <- function(
  input = NULL,
  output = "./data/figures/figure_profiles.pdf",
  annotation_path_extract = "./data/processed/241026_103144_extract/extract_results.tsv",
  annotation_path_fractions = "./data/processed/241217_130815_fractions/fractions_results.tsv",
  features_path_extract = "./data/extract_mzmine/extract.csv",
  features_path_fractions = "./data/fractions_mzmine/fractions.csv",
  peaks_dir_extract = "./data/interim/peaks/extract",
  peaks_dir_fractions = "./data/interim/peaks/fractions",
  min_confidence = 0.45,
  min_similarity_prefilter = 0.6,
  min_similarity_filter = 0.8,
  mode = "pos",
  detector = "ms",
  type = "analysis"
) {
  raw_profiles <- load_dataset_or_file(input, "profiles")
  profiles_consistent <- load_consistent_profiles(raw_profiles)

  profiles_consistent$fraction <- order_numeric_labels(
    gsub("fraction_", "", profiles_consistent$fraction, fixed = TRUE)
  )
  profiles_consistent$sum_name <- stats::ave(
    profiles_consistent$value,
    profiles_consistent$fraction,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  profiles_consistent$color <- discrete_rainbow_14[profiles_consistent$group]
  profiles_consistent$relative <- profiles_consistent$value /
    profiles_consistent$sum_name

  profiles_consistent$taste <- fct_reorder_base(
    profiles_consistent$taste,
    profiles_consistent$sum_taste,
    desc = FALSE
  )
  profiles_consistent$color <- fct_reorder_base(
    profiles_consistent$color,
    profiles_consistent$sum_taste,
    desc = FALSE
  )

  sens_distinct <- unique(
    profiles_consistent[, c("fraction", "sum", "taste", "color"), drop = FALSE]
  )
  sens_colors <- as.character(levels(profiles_consistent$color))

  profile_sensorial <- sens_distinct |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = fraction,
        y = sum,
        fill = taste,
        color = taste
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      values = sens_colors,
      guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
    ) +
    ggplot2::scale_color_manual(
      values = sens_colors,
      guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
    ) +
    ggplot2::labs(fill = "Taste", color = "Taste") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    ) +
    ggplot2::xlab("Fraction") +
    # ggbreak::scale_y_cut(
    #   breaks = c(10, 30),
    #   which = c(1, 3),
    #   scales = c(0, 1)
    # ) +
    ggplot2::ylab("Taste intensity") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "#767676"),
      axis.title = ggplot2::element_text(color = "#767676"),
      legend.text = ggplot2::element_text(color = "#767676"),
      text = ggplot2::element_text(
        face = "bold",
        color = "#767676",
        size = 20
      )
    )

  # profile_sensorial_facets <- profile_sensorial +
  #   ggplot2::facet_wrap(~taste)
  #
  # profile_sensorial_facets

  prepare_comparison_list <- function(
    x,
    features_informed,
    features_not_informed,
    candidates_confident,
    min_similarity_prefilter,
    min_similarity_filter,
    mode
  ) {
    compared_peaks_list_fractions <- .cascade_prepare_comparison(
      features_informed = features_informed[[x]],
      features_not_informed = features_not_informed[[x]],
      candidates_confident = candidates_confident,
      min_similarity_prefilter = min_similarity_prefilter,
      min_similarity_filter = min_similarity_filter,
      mode = mode
    )

    compared_peaks_list_fractions$peaks_maj_precor
  }

  # features_informed_extract <- peaks_dir_extract |>
  #   list.files(pattern = "featuresInformed_cad.tsv", full.names = TRUE)
  # features_not_informed_extract <- peaks_dir_extract |>
  #   list.files(pattern = "featuresNotInformed_cad.tsv", full.names = TRUE)
  features_informed_fractions <- list.files(
    peaks_dir_fractions,
    pattern = "featuresInformed_cad\\.tsv$",
    full.names = TRUE
  )
  features_not_informed_fractions <- list.files(
    peaks_dir_fractions,
    pattern = "featuresNotInformed_cad\\.tsv$",
    full.names = TRUE
  )

  # features_extract <- features_path_extract |>
  #   tidytable::fread() |>
  #   tidytable::select(id, tidytable::contains(":area")) |>
  #   tidytable::pivot_longer(tidytable::contains(":area"), names_prefix = "datafile:") |>
  #   tidytable::mutate(name = name |> gsub(pattern = ":area", replacement = "")) |>
  #   tidytable::select(
  #     feature_id = id,
  #     sample = name,
  #     intensity_new = value
  #   )
  raw_feat_frac <- data.table::fread(
    features_path_fractions,
    data.table = FALSE
  )
  area_cols <- grepv(":area", names(raw_feat_frac), fixed = TRUE)
  if (length(area_cols) == 0L) {
    stop("At least one column must be supplied to cols (:area).", call. = FALSE)
  }

  sample_names <- gsub(
    ":area",
    "",
    sub("^datafile:", "", area_cols),
    fixed = TRUE
  )
  feat_list <- lapply(seq_along(area_cols), function(i) {
    data.frame(
      feature_id = raw_feat_frac$id,
      sample = sample_names[[i]],
      intensity_new = raw_feat_frac[[area_cols[[i]]]],
      stringsAsFactors = FALSE
    )
  })
  features_fractions <- data.table::as.data.table(do.call(rbind, feat_list))

  # annotation_table_extract <- annotation_path_extract |>
  #   tidytable::fread()
  ann_dt <- data.table::fread(annotation_path_fractions)

  # candidates_confident_extract <- annotation_table_extract |>
  #   tidytable::mutate(mode = mode) |>
  #   .cascade_keep_best_candidates() |>
  #   tidytable::mutate(species = "Swertia chirayita") |>
  #   tidytable::mutate(feature_id = feature_id |>
  #     as.numeric()) |>
  #   .cascade_make_confident(score = min_confidence)
  ann_dt[, mode := mode]
  best_cand <- .cascade_keep_best_candidates(ann_dt)
  best_cand[, `:=`(
    species = "Swertia chirayita",
    feature_id = as.numeric(feature_id)
  )]
  candidates_confident_fractions <- .cascade_make_confident(
    best_cand,
    score = min_confidence
  )

  # chemicals_informed_extract <- seq_along(features_informed_extract) |>
  #   purrr::map(
  #     .f = prepare_comparison_list,
  #     features_informed = features_informed_extract,
  #     features_not_informed = features_not_informed_extract,
  #     candidates_confident = candidates_confident_extract,
  #     min_similarity_prefilter = min_similarity_prefilter,
  #     min_similarity_filter = min_similarity_filter,
  #     mode = mode
  #   ) |>
  #   tidytable::bind_rows() |>
  #   tidytable::inner_join(features_extract) |>
  #   tidytable::mutate(intensity = intensity_new) |>
  #   .cascade_make_other() |>
  #   .cascade_no_other() |>
  #   .cascade_prepare_hierarchy(type = type, detector = detector) |>
  #   tidytable::filter(!ids |>
  #     grepl(pattern = "$", fixed = TRUE)) |>
  #   tidytable::mutate(ids = ids |>
  #     gsub(pattern = "-", replacement = "- \n ")) |>
  #   .cascade_prepare_plot() |>
  #   tidytable::distinct(sample, values, ids, color)
  chemicals_informed_fractions_list <- lapply(
    seq_along(features_informed_fractions),
    prepare_comparison_list,
    features_informed = features_informed_fractions,
    features_not_informed = features_not_informed_fractions,
    candidates_confident = candidates_confident_fractions,
    min_similarity_prefilter = min_similarity_prefilter,
    min_similarity_filter = min_similarity_filter,
    mode = mode
  )

  bound_peaks <- data.table::rbindlist(
    chemicals_informed_fractions_list,
    use.names = TRUE,
    fill = TRUE
  )
  joined_peaks <- merge(
    bound_peaks,
    features_fractions,
    by = intersect(names(bound_peaks), names(features_fractions))
  )
  joined_peaks[, intensity := intensity_new]

  hier <- joined_peaks |>
    .cascade_make_other() |>
    .cascade_no_other() |>
    .cascade_prepare_hierarchy(type = type, detector = detector)
  hier <- hier[!grepl("$", ids, fixed = TRUE)]
  hier[,
    ids := gsub(
      "-",
      "\n",
      gsub(
        " notClassified",
        "\nnotClassified",
        gsub(" and ", " &\n", ids, fixed = TRUE),
        fixed = TRUE
      ),
      fixed = TRUE
    )
  ]
  chemicals_informed_fractions <- .cascade_prepare_plot(hier)
  chem_df <- as.data.frame(chemicals_informed_fractions)
  chem_df$fraction <- order_numeric_labels(
    gsub("_.*", "", gsub(".*M_", "", as.character(chem_df$sample)))
  )
  chem_colors <- levels(chemicals_informed_fractions$color)

  profile_chemical <- chem_df |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = fraction,
        y = values,
        fill = ids,
        color = ids
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      values = chem_colors,
      guide = ggplot2::guide_legend(reverse = FALSE, ncol = 1)
    ) +
    ggplot2::scale_color_manual(
      values = chem_colors,
      guide = ggplot2::guide_legend(reverse = FALSE, ncol = 1)
    ) +
    ggplot2::labs(color = "Chemical class", fill = "Chemical class") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    ) +
    ggplot2::xlab("Fraction") +
    ggplot2::ylab("Feature intensity") +
    ggplot2::scale_y_continuous(labels = label_short_scale) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "#767676"),
      axis.title = ggplot2::element_text(color = "#767676"),
      legend.text = ggplot2::element_text(color = "#767676"),
      text = ggplot2::element_text(
        face = "bold",
        color = "#767676",
        size = 20
      )
    )

  profiles <- ggpubr::ggarrange(
    profile_sensorial,
    profile_chemical,
    nrow = 2,
    align = "v"
  )
  .cascade_check_export_dir(dirname(output))
  profiles |>
    ggplot2::ggsave(
      filename = output,
      width = 15,
      height = 15
    )
  output
}
