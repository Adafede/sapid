#' Plot significant ion-taste correlations and iridoid profiles
#'
#' Filter rolling-window ion-taste correlations by FDR-adjusted p-value,
#' correlation coefficient, and TIMA annotation confidence, and export three
#' publication figures: (1) top positive ion-taste linear regressions per taste,
#' (2) bitter iridoid regressions colored by correlation strength, and (3)
#' stacked sensory and ion intensity profiles across fractions.
#'
#' @concept EDAM:topic_3172 Metabolomics
#' @concept EDAM:topic_0092 Data visualisation
#' @concept EDAM:operation_3465 Correlation
#' @concept EDAM:operation_3803 Natural product identification
#' @concept EDAM:operation_0337 Visualisation
#' @param input_correlations Path to the TSV table produced by
#'   [correlate_ion_taste_intensities()].
#' @param input_ions Path to the mzmine feature table CSV.
#' @param input_tastes Optional path or `data.frame` of sensory profiles
#'   (defaults to built-in [profiles]).
#' @param output_1 Output PDF path for top positive taste correlations.
#' @param output_2 Output PDF path for bitter iridoid correlations.
#' @param output_3 Output PDF path for stacked sensory vs. ion profiles.
#' @param annotation_path_fractions Path to the TIMA fraction annotations TSV.
#' @param imputation_factor Multiplicative factor for missing value imputation
#'   (default `0.5`).
#' @param max_pval Maximum BH-adjusted p-value threshold (default `0.05`).
#' @param min_area_ion Minimum feature peak area threshold (default `1L`).
#' @param min_confidence Minimum TIMA final annotation score (default `0.45`).
#' @param min_consistency Minimum chemical class consistency score
#'   (default `0.9`).
#' @param min_corr Minimum correlation coefficient threshold (default `0.95`).
#' @param min_intense_ions_ratio Minimum overlap ratio with top-intensity
#'   fractions (default `0.8`).
#' @param min_jury Minimum number of distinct jurors per taste (default `2L`).
#' @param mode Ionization mode (`"pos"` or `"neg"`, default `"pos"`).
#' @param widths Integer vector of rolling window widths (default `5:9`).
#' @param rt_window Numeric retention-time bin width in minutes for local
#'   `inchikey_2D` deduplication (default `0.5`).
#' @param greiner_kendall Logical; if `TRUE` (default), map Kendall's \eqn{\tau}
#'   to Pearson-equivalent scale via \eqn{r_{\text{eq}} = \sin(\frac{\pi}{2}\tau)}.
#' @param complete_grid Logical; if `TRUE` (default), complete the
#'   `fraction x taste` grid across evaluated fractions with baseline
#'   imputation (`imputation_factor * min_v`).
#'
#' @return Named list with elements `plot_correlations_1`,
#'   `plot_correlations_2`, and `plot_correlations_3`.
#' @export
#' @examples
#' \dontrun{
#' plot_correlations()
#' }
plot_correlations <- function(
  input_correlations = "./data/correlations.tsv",
  input_ions = "./data/fractions_mzmine/fractions.csv",
  input_tastes = NULL,
  output_1 = "./data/figures/figure_correlations_1.pdf",
  output_2 = "./data/figures/figure_correlations_2.pdf",
  output_3 = "./data/figures/figure_correlations_3.pdf",
  annotation_path_fractions = "./data/processed/241217_130815_fractions/fractions_results.tsv",
  imputation_factor = 0.5,
  max_pval = 0.05,
  min_area_ion = 1L,
  min_confidence = 0.45,
  min_consistency = 0.9,
  min_corr = 0.95,
  min_intense_ions_ratio = 0.8,
  min_jury = 2L,
  mode = "pos",
  widths = 5:9,
  rt_window = 0.5,
  greiner_kendall = TRUE,
  complete_grid = TRUE
) {
  min_width <- min(widths)
  min_min_width <- min_width * min_intense_ions_ratio

  annotation_table_fractions <- data.table::fread(
    annotation_path_fractions,
    data.table = FALSE
  )
  correlations <- data.table::fread(input_correlations, data.table = FALSE)
  if (greiner_kendall && "method" %in% names(correlations)) {
    is_k <- !is.na(correlations$method) & correlations$method == "kendall"
    correlations$correlation[is_k] <- sin(
      pi / 2 * correlations$correlation[is_k]
    )
    correlations$method[is_k] <- "kendall_greiner"
  }

  df_ion_intensities <- prepare_ion_intensities_df(
    input_ions = input_ions,
    min_width = min_width,
    min_area_ion = min_area_ion,
    imputation_factor = imputation_factor
  )

  ann_dt <- data.table::as.data.table(annotation_table_fractions)
  ann_dt[, mode := mode]
  best_cand <- .cascade_keep_best_candidates(ann_dt)
  best_cand[, `:=`(
    species = "Swertia chirayita",
    feature_id = as.numeric(feature_id)
  )]
  conf_cand <- as.data.frame(
    .cascade_make_confident(best_cand, score = min_confidence),
    stringsAsFactors = FALSE
  )

  ion_sums <- stats::aggregate(
    intensity_ion ~ id_ion,
    data = df_ion_intensities,
    FUN = sum
  )
  names(ion_sums)[names(ion_sums) == "id_ion"] <- "feature_id"

  candidates_confident_fractions <- merge(
    conf_cand,
    ion_sums,
    by = "feature_id",
    all.x = TRUE
  )
  rt_col <- if ("rt" %in% names(candidates_confident_fractions)) {
    as.numeric(candidates_confident_fractions$rt)
  } else if ("feature_rt" %in% names(candidates_confident_fractions)) {
    as.numeric(candidates_confident_fractions$feature_rt)
  } else {
    rep(0, nrow(candidates_confident_fractions))
  }
  rt_bin <- if (is.finite(rt_window) && rt_window > 0) {
    round(rt_col / rt_window) * rt_window
  } else {
    rep(0, nrow(candidates_confident_fractions))
  }
  ik_rt_key <- paste(
    candidates_confident_fractions$inchikey_2D,
    rt_bin,
    sep = "\r"
  )
  max_by_ik <- stats::ave(
    candidates_confident_fractions$intensity_ion,
    ik_rt_key,
    FUN = function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  )
  candidates_confident_fractions <- candidates_confident_fractions[
    !is.na(candidates_confident_fractions$intensity_ion) &
      candidates_confident_fractions$intensity_ion == max_by_ik,
    ,
    drop = FALSE
  ]

  # tidytable::filter(taste %in% tastes) |>
  df_taste_intensities <- prepare_taste_intensities_df(
    input_tastes = input_tastes,
    min_jury = min_jury,
    imputation_factor = imputation_factor,
    complete_grid = complete_grid
  )

  non_na_ions <- df_ion_intensities[
    !is.na(df_ion_intensities$intensity_ion),
    ,
    drop = FALSE
  ]
  ion_splits <- split(non_na_ions, non_na_ions$id_ion)
  top_fracs_map <- lapply(ion_splits, function(sub_df) {
    ord <- order(sub_df$intensity_ion, decreasing = TRUE)
    top_n <- sub_df[ord[seq_len(min(nrow(sub_df), min_width))], , drop = FALSE]
    frac_chr <- gsub("_.*", "", gsub(".*M_", "", as.character(top_n$fraction)))
    sort(frac_chr)
  })

  ion_ids_int <- as.integer(names(top_fracs_map))
  u_wins <- unique(as.character(correlations$fractions))
  win_splits <- strsplit(u_wins, " ", fixed = TRUE)
  all_frac_tokens <- unique(c(
    unlist(top_fracs_map, use.names = FALSE),
    unlist(win_splits, use.names = FALSE)
  ))
  m_ion <- matrix(
    0,
    nrow = length(top_fracs_map),
    ncol = length(all_frac_tokens)
  )
  for (i in seq_along(top_fracs_map)) {
    m_ion[i, match(top_fracs_map[[i]], all_frac_tokens)] <- 1
  }
  m_win <- matrix(0, nrow = length(all_frac_tokens), ncol = length(u_wins))
  for (w in seq_along(win_splits)) {
    m_win[match(win_splits[[w]], all_frac_tokens), w] <- 1
  }
  overlap_mat <- (m_ion %*% m_win) >= min_min_width

  corr_sig <- correlations[
    abs(correlations$p_adjusted) < max_pval &
      correlations$id_ion %in% ion_ids_int,
    ,
    drop = FALSE
  ]
  corr_sig$fractions.x <- as.character(corr_sig$fractions)
  r_ion <- match(corr_sig$id_ion, ion_ids_int)
  r_win <- match(corr_sig$fractions.x, u_wins)
  keep_sig <- !is.na(r_ion) &
    !is.na(r_win) &
    overlap_mat[cbind(r_ion, r_win)]
  correlations_significant <- corr_sig[keep_sig, , drop = FALSE]

  # ggplot2::ggplot(
  #   correlations_significant,
  #   ggplot2::aes(x = correlation, y = id_taste, fill = id_taste)
  # ) +
  #   ggplot2::scale_fill_manual(values = discrete_rainbow_14) +
  #   ggridges::geom_density_ridges(stat = "binline", bins = 100) +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(legend.position = "none")

  corr_top <- correlations_significant[
    abs(correlations_significant$correlation) >= min_corr,
    ,
    drop = FALSE
  ]
  # tidytable::group_by(id_ion, id_taste) |>
  # tidytable::mutate(min = min(p_adjusted, na.rm = TRUE)) |>
  # tidytable::ungroup() |>
  # tidytable::filter(p_adjusted == min) |>
  # tidytable::group_by(fractions.x, id_taste) |>
  # tidytable::mutate(min_2 = min(p_adjusted, na.rm = TRUE)) |>
  # tidytable::ungroup() |>
  # tidytable::filter(p_adjusted == min_2) |>
  corr_top <- merge(
    corr_top,
    candidates_confident_fractions,
    by.x = "id_ion",
    by.y = "feature_id",
    all.x = TRUE
  )
  n_win <- lengths(strsplit(
    as.character(corr_top$fractions.x),
    " ",
    fixed = TRUE
  ))
  r_clamped <- pmin(pmax(corr_top$correlation, -0.9999), 0.9999)
  fisher_z <- atanh(abs(r_clamped)) * sqrt(pmax(1, n_win - 3))
  s_tima <- as.numeric(corr_top$score_final)
  s_tima_conf <- ifelse(
    !is.na(s_tima) & s_tima >= min_confidence,
    s_tima,
    0.1 * s_tima
  )
  corr_top$score_mixed <- sign(corr_top$correlation) *
    tanh(fisher_z / 3) *
    s_tima_conf
  # tidytable::distinct(id_taste,inchikey_2D, .keep_all = TRUE) |>
  corr_top <- unique(
    corr_top[order(corr_top$score_mixed, decreasing = TRUE), , drop = FALSE]
  )

  pos_sub <- corr_top[corr_top$correlation > min_corr, , drop = FALSE]
  has_any_score <- !all(is.na(pos_sub$score_mixed))
  pos_splits <- split(pos_sub, pos_sub$id_taste)
  pos_filtered_list <- lapply(pos_splits, function(s) {
    if (has_any_score) {
      if (all(is.na(s$score_mixed))) {
        return(s[0L, , drop = FALSE])
      }
      s <- s[
        !is.na(s$score_mixed) &
          s$score_mixed == max(s$score_mixed, na.rm = TRUE),
        ,
        drop = FALSE
      ]
    }
    if (nrow(s) > 0L && !all(is.na(s$p_adjusted))) {
      s <- s[
        !is.na(s$p_adjusted) & s$p_adjusted == min(s$p_adjusted, na.rm = TRUE),
        ,
        drop = FALSE
      ]
    }
    if (nrow(s) > 0L) {
      nc <- nchar(s$fractions.x)
      s <- s[nc == max(nc, na.rm = TRUE), , drop = FALSE]
    }
    # tidytable::distinct(id_taste, .keep_all = TRUE) |>
    s
  })
  correlations_significant_filtered_pos <- do.call(rbind, pos_filtered_list)

  # correlations_significant_filtered_neg <- correlations_significant_top |>
  #   tidytable::group_by(id_taste) |>
  #   tidytable::filter(correlation < -1 * min_corr) |>
  #   tidytable::filter(score_mixed == min(score_mixed, na.rm = TRUE)) |>
  #   tidytable::filter(p_adjusted == min(p_adjusted, na.rm = TRUE)) |>
  #   tidytable::filter(nchar(fractions.x) == max(nchar(fractions.x), na.rm = TRUE)) |>
  #   # tidytable::distinct(id_taste, .keep_all = TRUE) |>
  #   tidytable::ungroup()

  pos_pairs <- unique(
    correlations_significant_filtered_pos[,
      c("id_ion", "id_taste", "fractions.x"),
      drop = FALSE
    ]
  )
  pos_frac_tokens <- strsplit(
    as.character(pos_pairs$fractions.x),
    " ",
    fixed = TRUE
  )
  indices_significant_pos <- data.frame(
    id_ion = rep(as.integer(pos_pairs$id_ion), lengths(pos_frac_tokens)),
    id_taste = rep(as.character(pos_pairs$id_taste), lengths(pos_frac_tokens)),
    fraction = as.integer(unlist(pos_frac_tokens, use.names = FALSE)),
    stringsAsFactors = FALSE
  )

  # indices_significant_neg <- correlations_significant_filtered_neg |>
  #   tidytable::distinct(id_ion, id_taste, fractions.x) |>
  #   tidytable::mutate(id_ion = id_ion |>
  #     as.integer()) |>
  #   tidytable::separate_longer_delim(cols = fractions.x, delim = " ") |>
  #   tidytable::rename(fraction = fractions.x) |>
  #   tidytable::mutate(fraction = fraction |>
  #     as.integer())

  df_merged <- merge(df_taste_intensities, df_ion_intensities, by = "fraction")
  df_merged_pos <- merge(
    df_merged,
    indices_significant_pos,
    by = c("fraction", "id_ion", "id_taste")
  )

  # df_merged_neg <- df_merged |>
  #   tidytable::inner_join(indices_significant_neg)

  taste_palette <- c(
    "OTHER" = "grey90",
    "ACID" = "#882E72",
    "ASTRINGENT" = "#7BAFDE",
    "BITTER" = "#D1BBD7",
    "CARTON" = "#DC050C",
    "FATTY" = "#AE76A3",
    "FRESH" = "#5289C7",
    "METALLIC" = "#E8601C",
    "MOUTHFILLING" = "#4EB265",
    "PUNGENT" = "#F1932D",
    "SALTY" = "#F6C141",
    "SWEET" = "#90C987",
    "UMAMI" = "#CAE0AB",
    "VOLUME" = "#1965B0",
    "WOODY" = "#F7F056"
  )

  plot_correlations_1 <- df_merged_pos |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = intensity_ion,
        y = intensity_taste,
        color = id_taste,
        fill = id_taste
      )
    ) +
    ggplot2::facet_wrap("id_taste", scales = "free", ncol = 3) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_smooth(linewidth = 0.5, method = "lm", formula = y ~ x) +
    ggrepel::geom_text_repel(
      mapping = ggplot2::aes(label = as.character(fraction)),
      size = 5
    ) +
    # ggplot2::scale_x_log10() +
    ggplot2::xlab("Ion intensity") +
    ggplot2::ylab("Taste intensity") +
    ggplot2::scale_x_continuous(labels = label_si_number) +
    ggplot2::scale_color_manual(values = taste_palette) +
    ggplot2::scale_fill_manual(values = taste_palette) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(color = "#767676"),
      axis.text = ggplot2::element_text(color = "#767676"),
      axis.title = ggplot2::element_text(color = "#767676"),
      plot.margin = ggplot2::margin(
        l = 5,
        t = 5,
        b = 5,
        r = 20
      ),
      legend.position = "none",
      legend.text = ggplot2::element_text(color = "#767676"),
      text = ggplot2::element_text(
        face = "bold",
        color = "#767676",
        size = 20
      )
    )
  .cascade_check_export_dir(dirname(output_1))
  plot_correlations_1 |>
    ggplot2::ggsave(
      filename = output_1,
      width = 12,
      height = 12
    )

  iridoid_mask <- grepl(
    "iridoid",
    candidates_confident_fractions$best_candidate_3,
    ignore.case = TRUE
  ) &
    grepl(
      "iridoid",
      candidates_confident_fractions$consensus_3,
      ignore.case = TRUE
    ) &
    !is.na(candidates_confident_fractions$consistency_3) &
    (candidates_confident_fractions$consistency_3 >= min_consistency)

  iridoids <- candidates_confident_fractions[iridoid_mask, , drop = FALSE]
  iridoids <- iridoids[!duplicated(iridoids$feature_id), , drop = FALSE]
  names(iridoids)[names(iridoids) == "feature_id"] <- "id_ion"

  if (nrow(iridoids) == 0L) {
    stop(
      "No confident iridoid annotations found for fractions.x.",
      call. = FALSE
    )
  }

  bitter_corr <- correlations[
    correlations$id_taste == "BITTER" &
      correlations$id_ion %in% iridoids$id_ion &
      correlations$id_ion %in% ion_ids_int,
    ,
    drop = FALSE
  ]
  bitter_irid <- merge(bitter_corr, iridoids, by = "id_ion")
  if (nrow(bitter_irid) == 0L) {
    stop("No bitter iridoid correlations found for fractions.x.", call. = FALSE)
  }

  bitter_irid$fractions.x <- as.character(bitter_irid$fractions)
  b_ion_idx <- match(bitter_irid$id_ion, ion_ids_int)
  b_win_idx <- match(bitter_irid$fractions.x, u_wins)
  keep_b <- !is.na(b_ion_idx) &
    !is.na(b_win_idx) &
    overlap_mat[cbind(b_ion_idx, b_win_idx)]
  bitter_irid <- bitter_irid[keep_b, , drop = FALSE]

  max_corr_ik <- stats::ave(
    bitter_irid$correlation,
    bitter_irid$inchikey_2D,
    FUN = max
  )
  correlations_bitter_iridoids <- bitter_irid[
    bitter_irid$correlation == max_corr_ik,
    ,
    drop = FALSE
  ]

  irid_distinct <- unique(
    correlations_bitter_iridoids[,
      c(
        "id_ion",
        "id_taste",
        "fractions.x",
        "correlation",
        "p_adjusted",
        "smiles_2D"
      ),
      drop = FALSE
    ]
  )
  irid_tokens <- strsplit(
    as.character(irid_distinct$fractions.x),
    " ",
    fixed = TRUE
  )
  rep_idx <- rep(seq_len(nrow(irid_distinct)), lengths(irid_tokens))
  indices_iridoids <- irid_distinct[rep_idx, , drop = FALSE]
  indices_iridoids$id_ion <- as.integer(indices_iridoids$id_ion)
  indices_iridoids$fraction <- as.integer(unlist(
    irid_tokens,
    use.names = FALSE
  ))
  indices_iridoids$fractions.x <- NULL

  df_merged_bitter_iridoids <- merge(
    df_merged,
    indices_iridoids,
    by = c("fraction", "id_ion", "id_taste")
  )
  df_merged_bitter_iridoids$id_ion <- paste(
    "Ion ID:",
    df_merged_bitter_iridoids$id_ion
  )
  df_merged_bitter_iridoids$id_ion <- fct_reorder_base(
    df_merged_bitter_iridoids$id_ion,
    df_merged_bitter_iridoids$correlation,
    desc = TRUE
  )
  df_merged_bitter_iridoids$p_adjusted <- paste(
    "Adjusted p-value",
    format(round(df_merged_bitter_iridoids$p_adjusted, digits = 2), nsmall = 2)
  )

  plot_correlations_2 <- df_merged_bitter_iridoids |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = intensity_ion,
        y = intensity_taste,
        color = correlation,
        fill = correlation
      )
    ) +
    ggplot2::facet_wrap(
      facets = "id_ion",
      # facets = c("id_ion", "p_adjusted"),
      scales = "free_x",
      ncol = 2
    ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_smooth(linewidth = 0.5, method = "lm", formula = y ~ x) +
    ggrepel::geom_text_repel(
      mapping = ggplot2::aes(label = as.character(fraction)),
      size = 5
    ) +
    # ggplot2::scale_x_log10() +
    ggplot2::ylim(c(0, 100)) +
    ggplot2::xlab("Ion intensity") +
    ggplot2::ylab("Bitter intensity") +
    ggplot2::labs(color = "Correlation", fill = "Correlation") +
    ggplot2::scale_x_continuous(labels = label_si_number) +
    scale_color_batlow() +
    scale_fill_batlow() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(color = "#767676"),
      axis.text = ggplot2::element_text(color = "#767676"),
      axis.title = ggplot2::element_text(color = "#767676"),
      panel.spacing.x = ggplot2::unit(10, "lines"),
      plot.margin = ggplot2::margin(
        l = 5,
        t = 5,
        b = 5,
        r = 125
      ),
      legend.position = "bottom",
      # legend.position = c(0.75, 0.1),
      legend.text = ggplot2::element_text(color = "#767676"),
      text = ggplot2::element_text(
        face = "bold",
        color = "#767676",
        size = 20
      )
    )
  .cascade_check_export_dir(dirname(output_2))
  plot_correlations_2 |>
    ggplot2::ggsave(
      filename = output_2,
      width = 12,
      height = 12
    )

  prof_corr_taste <- unique(data.frame(
    fraction = as.numeric(df_merged_pos$fraction),
    name = "intensity_taste",
    median = as.numeric(df_merged_pos$intensity_taste),
    taste = as.character(df_merged_pos$id_taste),
    color = as.character(df_merged_pos$id_ion),
    stringsAsFactors = FALSE
  ))
  prof_corr_ion <- unique(data.frame(
    fraction = as.numeric(df_merged_pos$fraction),
    name = "intensity_ion",
    median = as.numeric(df_merged_pos$intensity_ion),
    taste = as.character(df_merged_pos$id_taste),
    color = as.character(df_merged_pos$id_ion),
    stringsAsFactors = FALSE
  ))
  profiles_correlation <- rbind(prof_corr_taste, prof_corr_ion)

  all_taste_rows <- unique(data.frame(
    fraction = as.numeric(df_taste_intensities$fraction),
    median = as.numeric(df_taste_intensities$intensity_taste),
    taste = as.character(df_taste_intensities$id_taste),
    stringsAsFactors = FALSE
  ))
  taste_match_key <- paste(
    all_taste_rows$fraction,
    all_taste_rows$median,
    all_taste_rows$taste,
    sep = "\r"
  )
  corr_match_key <- paste(
    profiles_correlation$fraction,
    profiles_correlation$median,
    profiles_correlation$taste,
    sep = "\r"
  )
  profile_anti_taste <- all_taste_rows[
    !taste_match_key %in% corr_match_key,
    ,
    drop = FALSE
  ]
  n_anti_taste <- nrow(profile_anti_taste)
  profile_anti_taste$taste <- rep("OTHER", n_anti_taste)
  profile_anti_taste$color <- rep("gray", n_anti_taste)
  profile_anti_taste$name <- rep("intensity_taste", n_anti_taste)

  u_ion_fracs <- unique(as.numeric(df_ion_intensities$fraction))
  zero_ion_match <- paste(u_ion_fracs, 0, sep = "\r")
  corr_frac_med <- paste(
    profiles_correlation$fraction,
    profiles_correlation$median,
    sep = "\r"
  )
  anti_ion_fracs <- u_ion_fracs[!zero_ion_match %in% corr_frac_med]
  n_anti_ion <- length(anti_ion_fracs)
  profile_anti_ion <- data.frame(
    fraction = anti_ion_fracs,
    name = rep("intensity_ion", n_anti_ion),
    median = rep(0, n_anti_ion),
    taste = rep("OTHER", n_anti_ion),
    color = rep("gray", n_anti_ion),
    stringsAsFactors = FALSE
  )

  profile_full <- rbind(
    profile_anti_taste[, names(profiles_correlation), drop = FALSE],
    profile_anti_ion[, names(profiles_correlation), drop = FALSE],
    profiles_correlation
  )
  profile_full$sum <- stats::ave(
    profile_full$median,
    profile_full$taste,
    FUN = sum
  )
  profile_full$sum[profile_full$taste == "OTHER"] <- Inf
  profile_full$fraction <- order_numeric_labels(profile_full$fraction)
  profile_full$taste <- fct_reorder_base(
    profile_full$taste,
    profile_full$sum,
    desc = TRUE
  )

  prof_sub_1 <- profile_full[
    profile_full$name == "intensity_taste",
    ,
    drop = FALSE
  ]
  profiles_correlation_1 <- prof_sub_1 |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = fraction,
        y = median,
        fill = taste,
        color = taste
      )
    ) +
    ggplot2::geom_col() +
    # ggplot2::geom_col(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_manual(values = taste_palette) +
    ggplot2::scale_color_manual(values = taste_palette) +
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
      axis.text = ggplot2::element_text(color = "#767676"),
      axis.title = ggplot2::element_text(color = "#767676"),
      legend.text = ggplot2::element_text(color = "#767676"),
      text = ggplot2::element_text(
        face = "bold",
        color = "#767676",
        size = 20
      )
    )

  prof_sub_2 <- profile_full[
    profile_full$name == "intensity_ion",
    ,
    drop = FALSE
  ]
  prof_sub_2$median <- ifelse(
    prof_sub_2$taste == "BITTER",
    prof_sub_2$median / 300,
    ifelse(
      prof_sub_2$taste == "PUNGENT",
      prof_sub_2$median / 100,
      prof_sub_2$median
    )
  )

  profiles_correlation_2 <- prof_sub_2 |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = fraction,
        y = median,
        fill = taste,
        color = taste
      )
    ) +
    ggplot2::geom_col() +
    # ggplot2::geom_col(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_manual(values = taste_palette) +
    ggplot2::scale_color_manual(values = taste_palette) +
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
      axis.text = ggplot2::element_text(color = "#767676"),
      axis.title = ggplot2::element_text(color = "#767676"),
      legend.text = ggplot2::element_text(color = "#767676"),
      text = ggplot2::element_text(
        face = "bold",
        color = "#767676",
        size = 20
      )
    )

  selected_profiles <- ggpubr::ggarrange(
    profiles_correlation_1,
    profiles_correlation_2,
    nrow = 2,
    align = "hv",
    common.legend = TRUE,
    legend = "right"
  )

  .cascade_check_export_dir(dirname(output_3))
  selected_profiles |>
    ggplot2::ggsave(
      filename = output_3,
      width = 15,
      height = 15
    )

  list(
    "plot_correlations_1" = output_1,
    "plot_correlations_2" = output_2,
    "plot_correlations_3" = output_3
  )
}
