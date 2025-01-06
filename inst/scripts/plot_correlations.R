start <- Sys.time()

pkgload::load_all()

message("This program plots correlations.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Plot correlations
#'
#' @param input_correlations Input correlations
#' @param input_ions Input ions
#' @param input_tastes Input tastes
#' @param output_1 Output 1
#' @param output_2 Output 2
#' @param output_3 Output 3
#' @param annotation_path_fractions Annotation path fractions
#' @param features_path_fractions Features path fractions
#' @param imputation_factor Imputation factor
#' @param max_pval Max p value
#' @param min_area_ion Minimal area ion
#' @param min_confidence Minimal confidence
#' @param min_consistency Minimal consistency
#' @param min_corr Minimal correlation
#' @param min_intense_ions_ratio Minimal intense ions ratio
#' @param min_jury Minimal number of jury
#' @param mode Mode
#' @param widths Widths (of the taste-ion windows)
#'
#' @return NULL
#'
#' @examples NULL
plot_correlations <- function(input_correlations = "./data/correlations.tsv",
                              input_ions = "./data/fractions_mzmine/fractions.csv",
                              input_tastes = system.file("extdata", "profiles.tsv", package = "sapid"),
                              output_1 = "./man/figures/figure_correlations_1.pdf",
                              output_2 = "./man/figures/figure_correlations_2.pdf",
                              output_3 = "./man/figures/figure_correlations_3.pdf",
                              annotation_path_fractions = "./data/processed/241217_130815_fractions/fractions_results.tsv",
                              features_path_fractions = "./data/fractions_mzmine/fractions.csv",
                              imputation_factor = 0.5,
                              max_pval = 0.05,
                              min_area_ion = 1L,
                              min_confidence = 0.45,
                              min_consistency = 0.9,
                              min_corr = 0.95,
                              min_intense_ions_ratio = 0.8,
                              min_jury = 2L,
                              mode = "pos",
                              widths = 5:9) {
  min_width <- widths |>
    min()
  min_min_width <- min_width * min_intense_ions_ratio

  annotation_table_fractions <- annotation_path_fractions |>
    tidytable::fread()

  correlations <- input_correlations |>
    tidytable::fread()

  df_ion_intensities <- input_ions |>
    tidytable::fread() |>
    tidytable::distinct(id, rt, mz, contains(":area")) |>
    tidytable::pivot_longer(contains(":area")) |>
    tidytable::select(-mz, -rt) |>
    tidytable::mutate(
      name = gsub(
        pattern = "datafile:210619_AR_[0-9]{2}_M_",
        replacement = "fraction_",
        x = gsub(
          pattern = "_01.mzML:area",
          replacement = "",
          x = name,
          fixed = TRUE
        )
      )
    ) |>
    tidytable::group_by(id) |>
    tidytable::mutate(non_na_count = with(rle(!is.na(value)), rep(lengths * values, lengths))) |>
    tidytable::ungroup() |>
    tidytable::filter(non_na_count >= min_width) |>
    tidytable::filter(value >= min_area_ion) |>
    tidytable::mutate(value = value |>
      tidytable::replace_na(imputation_factor * min(value, na.rm = TRUE))) |>
    tidytable::select(
      fraction = name,
      id_ion = id,
      intensity_ion = value
    ) |>
    tidytable::distinct() |>
    tidytable::mutate(fraction = fraction |>
      gsub(
        pattern = "fraction_",
        replacement = "",
        fixed = TRUE
      ) |>
      as.integer()) |>
    tidytable::arrange(fraction)

  candidates_confident_fractions <- annotation_table_fractions |>
    tidytable::mutate(mode = mode) |>
    cascade:::keep_best_candidates() |>
    tidytable::mutate(species = "Swertia chirayita") |>
    tidytable::mutate(feature_id = feature_id |>
      as.numeric()) |>
    cascade:::make_confident(score = min_confidence) |>
    tidytable::left_join(
      df_ion_intensities |>
        tidytable::rename(feature_id = id_ion) |>
        tidytable::group_by(feature_id) |>
        tidytable::summarize(intensity_ion = intensity_ion |>
          sum()) |>
        tidytable::ungroup()
    ) |>
    tidytable::group_by(inchikey_2D) |>
    tidytable::filter(intensity_ion == intensity_ion |>
      max(na.rm = TRUE)) |>
    tidytable::ungroup()

  df_taste_intensities <- input_tastes |>
    load_consistent_profiles(min_jury = min_jury) |>
    tidytable::mutate(tidytable::across(
      tidytable::everything(),
      .fns = function(x) {
        tidytable::if_else(condition = x == 0,
          true = NA,
          false = x
        )
      }
    )) |>
    tidytable::mutate(value = value |>
      tidytable::replace_na(imputation_factor * min(value, na.rm = TRUE))) |>
    # tidytable::filter(taste %in% tastes) |>
    tidytable::group_by(fraction, taste) |>
    tidytable::mutate(sum = value |>
      sum(na.rm = TRUE)) |>
    tidytable::ungroup() |>
    tidytable::select(fraction, id_taste = taste, intensity_taste = sum) |>
    tidytable::distinct() |>
    tidytable::mutate(fraction = fraction |>
      gsub(
        pattern = "fraction_",
        replacement = "",
        fixed = TRUE
      ) |>
      as.integer()) |>
    tidytable::arrange(fraction)

  df_ion_selection <- df_ion_intensities |>
    tidytable::rename(intensity_new = intensity_ion) |>
    tidytable::filter(!is.na(intensity_new)) |>
    tidytable::group_by(id_ion) |>
    tidytable::slice_max(
      order_by = intensity_new,
      n = min_width,
      with_ties = FALSE
    ) |>
    tidytable::mutate(
      fraction = fraction |>
        gsub(pattern = ".*M_", replacement = "") |>
        gsub(pattern = "_.*", replacement = "")
    ) |>
    tidytable::arrange(fraction) |>
    tidytable::group_by(id_ion) |>
    tidytable::mutate(fractions = list(fraction)) |>
    tidytable::distinct(id_ion, fractions)

  correlations_significant <- correlations |>
    tidytable::filter(abs(p_adjusted) < max_pval) |>
    tidytable::mutate(fractions_list = fractions |>
      strsplit(split = " ")) |>
    tidytable::inner_join(df_ion_selection, by = c("id_ion" = "id_ion")) |>
    tidytable::filter(tidytable::map2_lgl(fractions.y, fractions_list, ~ sum(.x %in% .y) >= min_min_width))

  nrow(correlations_significant) / nrow(correlations)

  # ggplot2::ggplot(
  #   correlations_significant,
  #   ggplot2::aes(x = correlation, y = id_taste, fill = id_taste)
  # ) +
  #   ggplot2::scale_fill_manual(values = discrete_rainbow_14) +
  #   ggridges::geom_density_ridges(stat = "binline", bins = 100) +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(legend.position = "none")

  correlations_significant_top <- correlations_significant |>
    tidytable::filter(abs(correlation) >= min_corr) |>
    # tidytable::group_by(id_ion, id_taste) |>
    # tidytable::mutate(min = min(p_adjusted, na.rm = TRUE)) |>
    # tidytable::ungroup() |>
    # tidytable::filter(p_adjusted == min) |>
    # tidytable::group_by(fractions.x, id_taste) |>
    # tidytable::mutate(min_2 = min(p_adjusted, na.rm = TRUE)) |>
    # tidytable::ungroup() |>
    # tidytable::filter(p_adjusted == min_2) |>
    tidytable::left_join(candidates_confident_fractions,
      by = c("id_ion" = "feature_id")
    ) |>
    tidytable::mutate(score_mixed = correlation * score_final |>
      as.numeric()) |>
    tidytable::arrange(score_mixed |>
      tidytable::desc()) |>
    # tidytable::distinct(id_taste,inchikey_2D, .keep_all = TRUE) |>
    tidytable::distinct()

  correlations_temp <- correlations_significant_top

  correlations_significant_filtered_pos <- correlations_significant_top |>
    tidytable::filter(correlation > min_corr) |>
    tidytable::group_by(id_taste) |>
    tidytable::filter(score_mixed == max(score_mixed, na.rm = TRUE)) |>
    tidytable::filter(p_adjusted == min(p_adjusted, na.rm = TRUE)) |>
    tidytable::filter(stringi::stri_length(fractions.x) == max(stringi::stri_length(fractions.x), na.rm = TRUE)) |>
    # tidytable::distinct(id_taste, .keep_all = TRUE) |>
    tidytable::ungroup()

  correlations_significant_filtered_neg <- correlations_significant_top |>
    tidytable::group_by(id_taste) |>
    tidytable::filter(correlation < -1 * min_corr) |>
    tidytable::filter(score_mixed == min(score_mixed, na.rm = TRUE)) |>
    tidytable::filter(p_adjusted == min(p_adjusted, na.rm = TRUE)) |>
    tidytable::filter(stringi::stri_length(fractions.x) == max(stringi::stri_length(fractions.x), na.rm = TRUE)) |>
    # tidytable::distinct(id_taste, .keep_all = TRUE) |>
    tidytable::ungroup()

  indices_significant_pos <- correlations_significant_filtered_pos |>
    tidytable::distinct(id_ion, id_taste, fractions.x) |>
    tidytable::mutate(id_ion = id_ion |>
      as.integer()) |>
    tidytable::separate_longer_delim(cols = fractions.x, delim = " ") |>
    tidytable::rename(fraction = fractions.x) |>
    tidytable::mutate(fraction = fraction |>
      as.integer())

  indices_significant_neg <- correlations_significant_filtered_neg |>
    tidytable::distinct(id_ion, id_taste, fractions.x) |>
    tidytable::mutate(id_ion = id_ion |>
      as.integer()) |>
    tidytable::separate_longer_delim(cols = fractions.x, delim = " ") |>
    tidytable::rename(fraction = fractions.x) |>
    tidytable::mutate(fraction = fraction |>
      as.integer())

  df_merged <- df_taste_intensities |>
    tidytable::inner_join(df_ion_intensities)

  df_merged_pos <- df_merged |>
    tidytable::inner_join(indices_significant_pos)

  df_merged_neg <- df_merged |>
    tidytable::inner_join(indices_significant_neg)

  plot_correlations_1 <- df_merged_pos |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = intensity_ion,
        y = intensity_taste,
        color = id_taste,
        label = fraction |>
          as.character(),
        fill = id_taste
      )
    ) +
    ggplot2::facet_wrap("id_taste", scales = "free", ncol = 3) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_smooth(size = 0.5, method = "lm") +
    ggrepel::geom_text_repel(size = 5) +
    # ggplot2::scale_x_log10() +
    ggplot2::xlab("Ion intensity") +
    ggplot2::ylab("Taste intensity") +
    ggplot2::scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si(""))) +
    ggplot2::scale_color_manual(
      values = c(
        # "#882E72", # acid
        "#7BAFDE",
        # astringent
        "#D1BBD7",
        # bitter
        # "#DC050C", # carton
        "#AE76A3",
        # fatty
        "#5289C7",
        # fresh
        # "#E8601C", # metallic
        # "#4EB265", # mouthfilling
        "#F1932D",
        # pungent
        # "#F6C141", # salty
        "#90C987",
        # sweet
        # "#CAE0AB", # umami
        "#1965B0" # volume
        # "#F7F056", # woody
        # "#777777"
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        # "#882E72", # acid
        "#7BAFDE",
        # astringent
        "#D1BBD7",
        # bitter
        # "#DC050C", # carton
        "#AE76A3",
        # fatty
        "#5289C7",
        # fresh
        # "#E8601C", # metallic
        # "#4EB265", # mouthfilling
        "#F1932D",
        # pungent
        # "#F6C141", # salty
        "#90C987",
        # sweet
        # "#CAE0AB", # umami
        "#1965B0" # volume
        # "#F7F056", # woody
        # "#777777"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(color = "grey30"),
      axis.text = ggplot2::element_text(color = "grey30"),
      axis.title = ggplot2::element_text(color = "grey30"),
      plot.margin = ggplot2::margin(
        l = 5,
        t = 5,
        b = 5,
        r = 20
      ),
      legend.position = "none",
      legend.text = ggplot2::element_text(color = "grey30"),
      text = ggplot2::element_text(
        face = "bold",
        color = "grey30",
        size = 20
      )
    )
  plot_correlations_1 |>
    ggplot2::ggsave(
      filename = output_1,
      width = 12,
      height = 12
    )

  candidates_confident_fractions |>
    tidytable::filter(best_candidate_3 |>
      grepl(pattern = "iridoid", ignore.case = TRUE)) |>
    tidytable::pull(smiles_2D) |>
    unique()

  features_fractions <- features_path_fractions |>
    tidytable::fread() |>
    tidytable::select(id, tidytable::contains(":area")) |>
    tidytable::pivot_longer(tidytable::contains(":area"), names_prefix = "datafile:") |>
    tidytable::mutate(name = name |> gsub(pattern = ":area", replacement = "")) |>
    tidytable::select(
      id_ion = id,
      sample = name,
      intensity_new = value
    ) |>
    tidytable::filter(!is.na(intensity_new)) |>
    tidytable::group_by(id_ion) |>
    tidytable::slice_max(intensity_new, n = 5) |>
    tidytable::mutate(
      sample = sample |>
        gsub(pattern = ".*M_", replacement = "") |>
        gsub(pattern = "_.*", replacement = "")
    ) |>
    tidytable::arrange(sample) |>
    tidytable::group_by(id_ion) |>
    tidytable::mutate(fractions = list(sample)) |>
    tidytable::distinct(id_ion, fractions)

  iridoids <- candidates_confident_fractions |>
    tidytable::filter(best_candidate_3 |>
      grepl(pattern = "iridoid", ignore.case = TRUE)) |>
    tidytable::filter(consensus_3 |>
      grepl(pattern = "iridoid", ignore.case = TRUE)) |>
    tidytable::filter(consistency_3 >= min_consistency) |>
    tidytable::distinct(id_ion = feature_id, .keep_all = TRUE)

  correlations_bitter_iridoids <- correlations |>
    tidytable::filter(id_taste == "BITTER") |>
    tidytable::inner_join(iridoids) |>
    tidytable::mutate(fractions_list = fractions |>
      strsplit(split = " ")) |>
    tidytable::inner_join(df_ion_selection, by = c("id_ion" = "id_ion")) |>
    tidytable::filter(tidytable::map2_lgl(fractions.y, fractions_list, ~ sum(.x %in% .y) >= 4)) |>
    tidytable::group_by(inchikey_2D) |>
    tidytable::filter(correlation == max(correlation)) |>
    tidytable::ungroup()

  indices_iridoids <- correlations_bitter_iridoids |>
    tidytable::distinct(
      id_ion,
      id_taste,
      fractions.x,
      correlation,
      p_adjusted,
      smiles_2D
    ) |>
    tidytable::mutate(id_ion = id_ion |>
      as.integer()) |>
    tidytable::separate_longer_delim(fractions.x, delim = " ") |>
    tidytable::rename(fraction = fractions.x) |>
    tidytable::mutate(fraction = fraction |>
      as.integer())

  df_merged_bitter_iridoids <- df_merged |>
    tidytable::inner_join(indices_iridoids)

  df_merged_bitter_iridoids$id_ion <- paste("Ion ID:", df_merged_bitter_iridoids$id_ion)
  df_merged_bitter_iridoids$id_ion <- df_merged_bitter_iridoids$id_ion |>
    as.character() |>
    forcats::fct_reorder(.x = df_merged_bitter_iridoids$correlation, .desc = TRUE)

  plot_correlations_2 <- df_merged_bitter_iridoids |>
    tidytable::mutate(p_adjusted = paste(
      "Adjusted p-value",
      p_adjusted |>
        round(digits = 2) |>
        format(nsmall = 2)
    )) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = intensity_ion,
        y = intensity_taste,
        color = correlation,
        label = fraction,
        fill = correlation
      )
    ) +
    ggplot2::facet_wrap(
      facets = "id_ion",
      # facets = c("id_ion", "p_adjusted"),
      scales = "free",
      ncol = 3
    ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_smooth(size = 0.5, method = "lm") +
    ggrepel::geom_text_repel(size = 5) +
    # ggplot2::scale_x_log10() +
    ggplot2::xlab("Ion intensity") +
    ggplot2::ylab("Bitter intensity") +
    ggplot2::labs(color = "Correlation", fill = "Correlation") +
    ggplot2::scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si(""))) +
    khroma::scale_color_batlow() +
    khroma::scale_fill_batlow() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(color = "grey30"),
      axis.text = ggplot2::element_text(color = "grey30"),
      axis.title = ggplot2::element_text(color = "grey30"),
      plot.margin = ggplot2::margin(
        l = 5,
        t = 5,
        b = 5,
        r = 20
      ),
      # legend.position = "none",
      legend.position = c(0.75, 0.1),
      legend.text = ggplot2::element_text(color = "grey30"),
      text = ggplot2::element_text(
        face = "bold",
        color = "grey30",
        size = 20
      )
    )
  plot_correlations_2 |>
    ggplot2::ggsave(
      filename = output_2,
      width = 12,
      height = 12
    )

  profiles_correlation <- df_merged_pos |>
    tidytable::pivot_longer(cols = tidytable::starts_with("intensity")) |>
    tidytable::mutate(value = ifelse(
      test = name == "intensity_taste",
      yes = value,
      no = 1 * value
    )) |>
    tidytable::distinct(fraction,
      name,
      median = value,
      taste = id_taste,
      color = id_ion
    )

  profile_anti_taste <- df_taste_intensities |>
    tidytable::mutate(fraction = fraction |>
      as.numeric()) |>
    tidytable::distinct(fraction, median = intensity_taste, taste = id_taste) |>
    tidytable::anti_join(profiles_correlation) |>
    tidytable::mutate(
      taste = "OTHER",
      color = "gray",
      name = "intensity_taste"
    )

  profile_anti_ion <- df_ion_intensities |>
    tidytable::distinct(fraction) |>
    tidytable::mutate(median = 0) |>
    tidytable::anti_join(profiles_correlation) |>
    tidytable::mutate(
      taste = "OTHER",
      color = "gray",
      name = "intensity_ion"
    )

  profile_full <- profile_anti_taste |>
    tidytable::bind_rows(profile_anti_ion) |>
    tidytable::bind_rows(profiles_correlation) |>
    tidytable::group_by(taste) |>
    tidytable::mutate(sum = median |>
      sum()) |>
    tidytable::ungroup() |>
    tidytable::mutate(sum = ifelse(
      test = taste == "OTHER",
      yes = Inf,
      no = sum
    )) |>
    tidytable::mutate(fraction = fraction |> as.character())

  profile_full$taste <-
    forcats::fct_reorder(
      .f = profile_full$taste,
      .x = profile_full$sum,
      .desc = TRUE
    )

  profiles_correlation_1 <- profile_full |>
    tidytable::filter(name == "intensity_taste") |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      x = fraction,
      y = median,
      fill = taste,
      color = taste,
    )) +
    ggplot2::geom_col() +
    # ggplot2::geom_col(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_manual(values = c(
      "grey90",
      "#D1BBD7",
      # bitter
      "#F1932D",
      # pungent
      "#90C987",
      # sweet
      "#7BAFDE",
      # astringent
      "#1965B0",
      # volume
      "#AE76A3" # fatty
      # "#882E72", # acid
      # "#DC050C", # carton
      # "#5289C7", # fresh
      # "#E8601C", # metallic
      # "#4EB265", # mouthfilling
      # "#F6C141", # salty
      # "#CAE0AB", # umami
      # "#F7F056", # woody
      # "#777777"
    )) +
    ggplot2::scale_color_manual(values = c(
      "grey90",
      "#D1BBD7",
      # bitter
      "#F1932D",
      # pungent
      "#90C987",
      # sweet
      "#7BAFDE",
      # astringent
      "#1965B0",
      # volume
      "#AE76A3" # fatty
      # "#882E72", # acid
      # "#DC050C", # carton
      # "#5289C7", # fresh
      # "#E8601C", # metallic
      # "#4EB265", # mouthfilling
      # "#F6C141", # salty
      # "#CAE0AB", # umami
      # "#F7F056", # woody
      # "#777777"
    )) +
    ggplot2::labs(fill = "Taste", color = "Taste") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    ) +
    ggplot2::xlab("") +
    # ggforce::facet_zoom(ylim = c(0, 20)) +
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
  profiles_correlation_1

  profiles_correlation_2 <- profile_full |>
    tidytable::filter(name == "intensity_ion") |>
    tidytable::mutate(median = ifelse(
      test = taste == "BITTER",
      yes = median / 300,
      no = median
    )) |>
    tidytable::mutate(median = ifelse(
      test = taste == "PUNGENT",
      yes = median / 100,
      no = median
    )) |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      x = fraction,
      y = median,
      fill = taste,
      color = taste,
    )) +
    ggplot2::geom_col() +
    # ggplot2::geom_col(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_manual(values = c(
      "grey90",
      "#D1BBD7",
      # bitter
      "#F1932D",
      # pungent
      "#90C987",
      # sweet
      "#7BAFDE",
      # astringent
      "#1965B0",
      # volume
      "#AE76A3" # fatty
      # "#882E72", # acid
      # "#DC050C", # carton
      # "#5289C7", # fresh
      # "#E8601C", # metallic
      # "#4EB265", # mouthfilling
      # "#F6C141", # salty
      # "#CAE0AB", # umami
      # "#F7F056", # woody
      # "#777777"
    )) +
    ggplot2::scale_color_manual(values = c(
      "grey90",
      "#D1BBD7",
      # bitter
      "#F1932D",
      # pungent
      "#90C987",
      # sweet
      "#7BAFDE",
      # astringent
      "#1965B0",
      # volume
      "#AE76A3" # fatty
      # "#882E72", # acid
      # "#DC050C", # carton
      # "#5289C7", # fresh
      # "#E8601C", # metallic
      # "#4EB265", # mouthfilling
      # "#F6C141", # salty
      # "#CAE0AB", # umami
      # "#F7F056", # woody
      # "#777777"
    )) +
    ggplot2::labs(fill = "Taste", color = "Taste") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    ) +
    ggplot2::xlab("Fraction") +
    # ggforce::facet_zoom(ylim = c(1E4, 2E5)) +
    # ggplot2::scale_y_continuous(transform = "log10") +
    # ggplot2::ylim(c(1E2,1E10)) +
    # ggbreak::scale_y_cut(
    #   breaks = c(2E5, 5E5),
    #   which = c(1, 2,3),
    #   scales = c( 1,0,1)
    # ) +
    ggplot2::ylab("Feature intensity") +
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
  profiles_correlation_2

  selected_profiles <- ggpubr::ggarrange(
    profiles_correlation_1,
    profiles_correlation_2,
    nrow = 2,
    align = "hv",
    common.legend = TRUE,
    legend = "right"
  )

  selected_profiles |>
    ggplot2::ggsave(
      filename = output_3,
      width = 15,
      height = 15
    )

  return(list(
    "plot_correlations_1" = output_1,
    "plot_correlations_2" = output_2,
    "plot_correlations_3" = output_3
  ))
}

plot_correlations()

end <- Sys.time()

message("Script finished in ", format(end - start))
