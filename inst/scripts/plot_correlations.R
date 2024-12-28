start <- Sys.time()

pkgload::load_all()

message("This program plots correlations.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Plot correlations
#'
#' @param input Input
#' @param output Output
#'
#' @return NULL
#'
#' @examples NULL
plot_correlations <- function(input_correlations = system.file("extdata", "correlations.tsv", package = "sapid"),
                              input_ions = "./data/fractions_mzmine/fractions.csv",
                              input_tastes = system.file("extdata", "profiles.tsv", package = "sapid"),
                              annotation_path_fractions = "./data/processed/241217_130815_fractions/fractions_results.tsv",
                              features_path_fractions = "./data/fractions_mzmine/fractions.csv",
                              imputation_factor = 0.5,
                              max_pval = 0.05,
                              min_area_ion = 1L,
                              min_confidence = 0.45,
                              min_consistency = 0.9,
                              min_corr = 0.95,
                              min_jury = 2L,
                              mode = "pos",
                              widths = 5:9) {
  annotation_table_fractions <- annotation_path_fractions |>
    tidytable::fread()

  candidates_confident_fractions <- annotation_table_fractions |>
    tidytable::mutate(mode = mode) |>
    cascade:::keep_best_candidates() |>
    tidytable::mutate(species = "Swertia chirayita") |>
    tidytable::mutate(feature_id = feature_id |>
      as.numeric()) |>
    cascade:::make_confident(score = min_confidence)

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
    tidytable::filter(non_na_count >= min(widths)) |>
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
      sum()) |>
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
    tidytable::slice_max(intensity_new, n = 5) |>
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
    tidytable::filter(tidytable::map2_lgl(fractions.y, fractions_list, ~ sum(.x %in% .y) >= 4))

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
    tidytable::group_by(id_ion, id_taste) |>
    tidytable::mutate(min = min(p_adjusted, na.rm = TRUE)) |>
    tidytable::ungroup() |>
    tidytable::filter(p_adjusted == min) |>
    tidytable::group_by(fractions.x, id_taste) |>
    tidytable::mutate(min_2 = min(p_adjusted, na.rm = TRUE)) |>
    tidytable::ungroup() |>
    tidytable::filter(p_adjusted == min_2) |>
    tidytable::inner_join(candidates_confident_fractions,
      by = c("id_ion" = "feature_id")
    ) |>
    tidytable::mutate(score_mixed = correlation * score_final |>
      as.numeric()) |>
    tidytable::arrange(score_mixed |> tidytable::desc()) |>
    # tidytable::distinct(id_taste,inchikey_2D, .keep_all = TRUE) |>
    tidytable::distinct()

  correlations_temp <- correlations_significant_top

  correlations_significant_filtered_pos <- correlations_significant_top |>
    tidytable::group_by(id_taste) |>
    tidytable::filter(correlation > min_corr) |>
    tidytable::filter(score_mixed == max(score_mixed, na.rm = TRUE)) |>
    tidytable::filter(p_adjusted == min(p_adjusted, na.rm = TRUE)) |>
    tidytable::filter(stringi::stri_length(fractions.x) == max(stringi::stri_length(fractions.x), na.rm = TRUE)) |>
    tidytable::ungroup()

  correlations_significant_filtered_neg <- correlations_significant_top |>
    tidytable::group_by(id_taste) |>
    tidytable::filter(correlation < -1 * min_corr) |>
    tidytable::filter(score_mixed == min(score_mixed, na.rm = TRUE)) |>
    tidytable::filter(p_adjusted == min(p_adjusted, na.rm = TRUE)) |>
    tidytable::filter(stringi::stri_length(fractions.x) == max(stringi::stri_length(fractions.x), na.rm = TRUE)) |>
    tidytable::ungroup()

  indices_significant_pos <- correlations_significant_filtered_pos |>
    tidytable::distinct(id_ion, id_taste, fractions.x) |>
    tidytable::mutate(id_ion = id_ion |>
      as.integer()) |>
    tidytable::separate_longer_delim(fractions.x, delim = " ") |>
    tidytable::rename(fraction = fractions.x) |>
    tidytable::mutate(fraction = fraction |>
      as.integer())

  indices_significant_neg <- correlations_significant_filtered_neg |>
    tidytable::distinct(id_ion, id_taste, fractions.x) |>
    tidytable::mutate(id_ion = id_ion |>
      as.integer()) |>
    tidytable::separate_longer_delim(fractions.x, delim = " ") |>
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
  plot_correlations_1
  # TODO 12x12

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
  plot_correlations_2
  # TODO 12x12
}

plot_correlations()

end <- Sys.time()

message("Script finished in ", format(end - start))
