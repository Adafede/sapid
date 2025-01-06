start <- Sys.time()

pkgload::load_all()

message("This program plots profiles")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Plot profiles
#'
#' @param input Input
#' @param output Output
#' @param annotation_path_extract Annotation path extract
#' @param annotation_path_fractions Annotation path fractions
#' @param features_path_extract Features path extract
#' @param features_path_fractions Annotation path fractions
#' @param peaks_dir_extract Peaks dir extract
#' @param peaks_dir_fractions Peaks dir fractions
#' @param min_confidence Minimal confidence
#' @param min_similarity_prefilter Minimal similarity prefilter
#' @param min_similarity_filter Minimal similarity filter
#' @param mode Mode
#' @param detector Detector
#' @param type Type
#'
#' @return NULL
#'
#' @examples NULL
plot_profiles <- function(input = system.file("extdata", "profiles.tsv", package = "sapid"),
                          output = "./data/figures/figure_profiles.pdf",
                          annotation_path_extract = "~/Git/sapid/data/processed/241026_103144_extract/extract_results.tsv",
                          annotation_path_fractions = "~/Git/sapid/data/processed/241217_130815_fractions/fractions_results.tsv",
                          features_path_extract = "~/Git/sapid/data/extract_mzmine/extract.csv",
                          features_path_fractions = "~/Git/sapid/data/fractions_mzmine/fractions.csv",
                          peaks_dir_extract = "~/Git/sapid/data/interim/peaks/extract",
                          peaks_dir_fractions = "~/Git/sapid/data/interim/peaks/fractions",
                          min_confidence = 0.45,
                          min_similarity_prefilter = 0.6,
                          min_similarity_filter = 0.8,
                          mode = "pos",
                          detector = "ms",
                          type = "analysis") {
  profiles_consistent <- input |>
    load_consistent_profiles()

  profiles_consistent <- profiles_consistent |>
    tidytable::mutate(
      fraction = fraction |>
        gsub(
          pattern = "fraction_",
          replacement = "",
          fixed = TRUE
        ) |>
        as.character()
    ) |>
    tidytable::group_by(fraction) |>
    tidytable::mutate(sum_name = value |>
      sum(na.rm = TRUE)) |>
    tidytable::ungroup() |>
    tidytable::mutate_rowwise(color = discrete_rainbow_14[[group]], relative = value / sum_name) |>
    tidytable::ungroup()

  profiles_consistent$taste <-
    forcats::fct_reorder(
      .f = profiles_consistent$taste,
      .x = profiles_consistent$sum_taste,
      .desc = FALSE
    )

  profiles_consistent$color <-
    forcats::fct_reorder(
      .f = profiles_consistent$color,
      .x = profiles_consistent$sum_taste,
      .desc = FALSE
    )

  profile_sensorial <- profiles_consistent |>
    tidytable::distinct(fraction, sum, taste, color) |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      x = fraction,
      y = sum,
      fill = taste,
      color = taste,
    )) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      values = levels(profiles_consistent$color) |>
        as.character(),
      guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
    ) +
    ggplot2::scale_color_manual(
      values = levels(profiles_consistent$color) |>
        as.character(),
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
      axis.text = ggplot2::element_text(color = "grey30"),
      axis.title = ggplot2::element_text(color = "grey30"),
      legend.text = ggplot2::element_text(color = "grey30"),
      text = ggplot2::element_text(
        face = "bold",
        color = "grey30",
        size = 20
      )
    )
  profile_sensorial

  # profile_sensorial_facets <- profile_sensorial +
  #   ggplot2::facet_wrap(~taste)
  #
  # profile_sensorial_facets

  prepare_comparison_list <- function(x,
                                      features_informed,
                                      features_not_informed,
                                      candidates_confident,
                                      min_similarity_prefilter,
                                      min_similarity_filter,
                                      mode) {
    compared_peaks_list_fractions <- cascade:::prepare_comparison(
      features_informed = features_informed[[x]],
      features_not_informed = features_not_informed[[x]],
      candidates_confident = candidates_confident,
      min_similarity_prefilter = min_similarity_prefilter,
      min_similarity_filter = min_similarity_filter,
      mode = mode
    )

    compared_peaks_list_fractions$peaks_maj_precor
  }

  features_informed_extract <- peaks_dir_extract |>
    list.files(pattern = "featuresInformed_cad.tsv", full.names = TRUE)
  features_not_informed_extract <- peaks_dir_extract |>
    list.files(pattern = "featuresNotInformed_cad.tsv", full.names = TRUE)
  features_informed_fractions <- peaks_dir_fractions |>
    list.files(pattern = "featuresInformed_cad.tsv", full.names = TRUE)
  features_not_informed_fractions <- peaks_dir_fractions |>
    list.files(pattern = "featuresNotInformed_cad.tsv", full.names = TRUE)

  features_extract <- features_path_extract |>
    tidytable::fread() |>
    tidytable::select(id, tidytable::contains(":area")) |>
    tidytable::pivot_longer(tidytable::contains(":area"), names_prefix = "datafile:") |>
    tidytable::mutate(name = name |> gsub(pattern = ":area", replacement = "")) |>
    tidytable::select(
      feature_id = id,
      sample = name,
      intensity_new = value
    )
  features_fractions <- features_path_fractions |>
    tidytable::fread() |>
    tidytable::select(id, tidytable::contains(":area")) |>
    tidytable::pivot_longer(tidytable::contains(":area"), names_prefix = "datafile:") |>
    tidytable::mutate(name = name |> gsub(pattern = ":area", replacement = "")) |>
    tidytable::select(
      feature_id = id,
      sample = name,
      intensity_new = value
    )

  annotation_table_extract <- annotation_path_extract |>
    tidytable::fread()
  annotation_table_fractions <- annotation_path_fractions |>
    tidytable::fread()

  candidates_confident_extract <- annotation_table_extract |>
    tidytable::mutate(mode = mode) |>
    cascade:::keep_best_candidates() |>
    tidytable::mutate(species = "Swertia chirayita") |>
    tidytable::mutate(feature_id = feature_id |>
      as.numeric()) |>
    cascade:::make_confident(score = min_confidence)
  candidates_confident_fractions <- annotation_table_fractions |>
    tidytable::mutate(mode = mode) |>
    cascade:::keep_best_candidates() |>
    tidytable::mutate(species = "Swertia chirayita") |>
    tidytable::mutate(feature_id = feature_id |>
      as.numeric()) |>
    cascade:::make_confident(score = min_confidence)

  chemicals_informed_extract <- seq_along(features_informed_extract) |>
    purrr::map(
      .f = prepare_comparison_list,
      features_informed = features_informed_extract,
      features_not_informed = features_not_informed_extract,
      candidates_confident = candidates_confident_extract,
      min_similarity_prefilter = min_similarity_prefilter,
      min_similarity_filter = min_similarity_filter,
      mode = mode
    ) |>
    tidytable::bind_rows() |>
    tidytable::inner_join(features_extract) |>
    tidytable::mutate(intensity = intensity_new) |>
    cascade:::make_other() |>
    cascade:::no_other() |>
    cascade:::prepare_hierarchy(type = type, detector = detector) |>
    tidytable::filter(!ids |>
      grepl(pattern = "$", fixed = TRUE)) |>
    tidytable::mutate(ids = ids |>
      gsub(pattern = "-", replacement = "- \n ")) |>
    cascade:::prepare_plot() |>
    tidytable::distinct(sample, values, ids, color)

  chemicals_informed_fractions_list <- seq_along(features_informed_fractions) |>
    purrr::map(
      .f = prepare_comparison_list,
      features_informed = features_informed_fractions,
      features_not_informed = features_not_informed_fractions,
      candidates_confident = candidates_confident_fractions,
      min_similarity_prefilter = min_similarity_prefilter,
      min_similarity_filter = min_similarity_filter,
      mode = mode
    )
  chemicals_informed_fractions <- chemicals_informed_fractions_list |>
    tidytable::bind_rows() |>
    tidytable::inner_join(features_fractions) |>
    tidytable::mutate(intensity = intensity_new) |>
    cascade:::make_other() |>
    cascade:::no_other() |>
    cascade:::prepare_hierarchy(type = type, detector = detector) |>
    tidytable::filter(!ids |>
      grepl(pattern = "$", fixed = TRUE)) |>
    tidytable::mutate(
      ids = ids |>
        gsub(pattern = " and ", replacement = " &\n") |>
        gsub(pattern = " notClassified", replacement = "\nnotClassified") |>
        gsub(pattern = "-", replacement = "\n")
    ) |>
    cascade:::prepare_plot()

  profile_chemical <- chemicals_informed_fractions |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      x = sample |>
        gsub(pattern = ".*M_", replacement = "") |>
        gsub(pattern = "_.*", replacement = ""),
      y = values,
      fill = ids,
      color = ids,
    )) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      values = chemicals_informed_fractions$color |> levels(),
      guide = ggplot2::guide_legend(reverse = FALSE, ncol = 1)
    ) +
    ggplot2::scale_color_manual(
      values = chemicals_informed_fractions$color |> levels(),
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
    ggplot2::scale_y_continuous(labels = scales::label_number(
      scale_cut =
        scales::cut_short_scale()
    )) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "grey30"),
      axis.title = ggplot2::element_text(color = "grey30"),
      legend.text = ggplot2::element_text(color = "grey30"),
      text = ggplot2::element_text(
        face = "bold",
        color = "grey30",
        size = 20
      )
    )
  profile_chemical

  profiles <- ggpubr::ggarrange(profile_sensorial,
    profile_chemical,
    nrow = 2,
    align = "v"
  )
  cascade:::check_export_dir(output)
  profiles |>
    ggplot2::ggsave(
      filename = output,
      width = 15,
      height = 15
    )
  return(output)
}

plot_profiles()

end <- Sys.time()

message("Script finished in ", format(end - start))
