#' Plot classical vs. chemically informed tasting treemaps
#'
#' Compare relative taste descriptor proportions between classical raw extract
#' tasting and mass-weighted chemically informed fraction tasting using side-by-side
#' treemaps.
#'
#' @concept EDAM:topic_3172 Metabolomics
#' @concept EDAM:topic_0092 Data visualisation
#' @concept EDAM:operation_2942 Treemap visualisation
#' @concept EDAM:operation_0337 Visualisation
#' @param file_fractions_mass Optional path or `data.frame` of fraction masses
#'   (defaults to built-in [fractions]).
#' @param file_taste_raw Optional path or `data.frame` of concentration AFC data
#'   (defaults to built-in [concentration_afc]).
#' @param file_taste_informed Optional path or `data.frame` of fraction sensory
#'   profiles (defaults to built-in [profiles]).
#' @param output Output PDF file path.
#' @param min_panelists Minimum number of distinct jurors required to retain an
#'   original taste descriptor (default `2`).
#'
#' @return Output file path (`output`).
#' @export
#' @examples
#' \donttest{
#' out <- tempfile(fileext = ".pdf")
#' plot_informed_tasting(output = out)
#' }
plot_informed_tasting <- function(
  file_fractions_mass = NULL,
  file_taste_raw = NULL,
  file_taste_informed = NULL,
  output = "./data/figures/figure_chemically_informed_tasting.pdf",
  min_panelists = 2
) {
  # Extract fraction masses
  raw_fractions <- load_dataset_or_file(file_fractions_mass, "fractions")
  fraction_masses <- data.frame(
    fraction = gsub(
      "M_",
      "fraction_",
      as.character(raw_fractions$label),
      fixed = TRUE
    ),
    mass = as.numeric(raw_fractions[[3L]]),
    stringsAsFactors = FALSE
  )

  # Extract raw taste descriptors
  raw_conc <- load_dataset_or_file(file_taste_raw, "concentration_afc")
  conc_sub <- raw_conc[
    as.numeric(raw_conc$concentration) == 3.10,
    ,
    drop = FALSE
  ]
  taste_raw <- stats::aggregate(value ~ taste, data = conc_sub, FUN = sum)
  taste_raw$value <- taste_raw$value / sum(taste_raw$value)
  raw_map <- c(
    "amer" = "BITTER",
    "astringence moyenne" = "ASTRINGENT",
    "beaucoup de volume au depart" = "VOLUME"
  )
  mapped <- raw_map[taste_raw$taste]
  taste_raw$taste <- ifelse(!is.na(mapped), unname(mapped), taste_raw$taste)

  prof_df <- load_dataset_or_file(file_taste_informed, "profiles")
  names(prof_df)[names(prof_df) == "taste_harmonized"] <- "taste"

  # Keep consistent panels only
  jury_counts <- tapply(
    prof_df$jury,
    prof_df$taste_original,
    function(x) length(unique(x))
  )
  keep_orig <- names(jury_counts)[jury_counts >= min_panelists]
  prof_filtered <- prof_df[
    prof_df$taste_original %in% keep_orig,
    ,
    drop = FALSE
  ]

  # Weigh descriptors with fraction mass
  prof_joined <- merge(
    prof_filtered,
    fraction_masses,
    by = "fraction",
    all.x = TRUE
  )
  prof_joined$weighted <- as.numeric(prof_joined$value) * prof_joined$mass
  taste_informed <- stats::aggregate(
    weighted ~ taste,
    data = prof_joined,
    FUN = sum
  )
  names(taste_informed)[names(taste_informed) == "weighted"] <- "value"

  # Scale to relative overall contributions
  taste_informed$value <- taste_informed$value / sum(taste_informed$value)
  taste_informed <- taste_informed[
    order(taste_informed$value, decreasing = TRUE),
    ,
    drop = FALSE
  ]

  u_tastes <- unique(taste_informed$taste)
  col <- stats::setNames(
    discrete_rainbow_14[seq_along(u_tastes)],
    as.character(u_tastes)
  )

  taste_raw_plot <- taste_raw
  taste_raw_plot$value <- taste_raw_plot$value * 100
  taste_raw_plot$taste <- fct_reorder_base(
    taste_raw_plot$taste,
    taste_raw_plot$value,
    desc = TRUE
  )
  taste_raw_plot$label <- sprintf("%0.1f%%", round(taste_raw_plot$value, 2))

  plot_raw <- taste_raw_plot |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        area = value,
        fill = taste,
        label = label
      )
    ) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(
      place = "centre",
      grow = TRUE,
      reflow = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = col,
      guide = "none"
    ) +
    ggplot2::labs(title = "Classical tasting", fill = "Taste") +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(color = "#767676"),
      text = ggplot2::element_text(
        face = "bold",
        color = "#767676",
        size = 20
      )
    )

  taste_informed_plot <- taste_informed
  taste_informed_plot$value <- taste_informed_plot$value * 100
  taste_informed_plot$taste <- fct_reorder_base(
    taste_informed_plot$taste,
    taste_informed_plot$value,
    desc = TRUE
  )
  taste_informed_plot$label <- sprintf(
    "%0.1f%%",
    round(taste_informed_plot$value, 2)
  )

  plot_informed <- taste_informed_plot |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        area = value,
        fill = taste,
        label = label
      )
    ) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(
      place = "centre"
    ) +
    ggplot2::scale_fill_manual(
      values = col
    ) +
    ggplot2::labs(title = "Chemically informed tasting", fill = "Taste") +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(color = "#767676"),
      text = ggplot2::element_text(
        face = "bold",
        color = "#767676",
        size = 20
      )
    )

  .cascade_check_export_dir(dirname(output))
  ggpubr::ggarrange(
    plotlist = list(plot_raw, plot_informed),
    ncol = 2,
    legend = "bottom",
    common.legend = TRUE
  ) |>
    ggplot2::ggsave(
      filename = output,
      width = 10,
      height = 6
    )
  return(output)
}
