start <- Sys.time()

pkgload::load_all()

message("This program plots a comparison between classical and chemically informed analysis.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

plot_informed_tasting <- function(file_fractions_mass = "inst/extdata/fractions.tsv",
                                  file_taste_raw = "inst/extdata/concentration_afc.tsv",
                                  file_taste_informed = "inst/extdata/profiles.tsv",
                                  output = "~/switchdrive/SAPERE/06_figures/figure_chemically_informed_tasting.pdf",
                                  min_panelists = 2) {
  fraction_masses <- file_fractions_mass |>
    tidytable::fread() |>
    tidytable::select(fraction = label, mass = 3) |>
    tidytable::mutate(
      fraction = fraction |> gsub(
        pattern = "M_",
        replacement = "fraction_",
        fixed = TRUE
      ),
      mass = mass |> as.numeric()
    )
  taste_raw <- file_taste_raw |>
    tidytable::fread() |>
    tidytable::filter(concentration == 3.10) |>
    tidytable::group_by(taste) |>
    tidytable::summarize(value = sum(value)) |>
    tidytable::mutate(value = value / sum(value)) |>
    tidytable::ungroup() |>
    tidytable::mutate(
      taste = tidytable::case_when(
        taste == "amer" ~ "BITTER",
        taste == "astringence moyenne" ~ "ASTRINGENT",
        taste == "beaucoup de volume au depart" ~ "VOLUME",
        TRUE ~ taste
      )
    )
  taste_informed <- file_taste_informed |>
    tidytable::fread() |>
    tidytable::rename(taste = taste_harmonized) |>
    tidytable::group_by(taste_original) |>
    tidytable::mutate(n = tidytable::n_distinct(jury)) |>
    tidytable::ungroup() |>
    tidytable::filter(n >= min_panelists) |>
    tidytable::inner_join(fraction_masses) |>
    tidytable::group_by(taste) |>
    tidytable::summarize(value = sum(value * mass)) |>
    tidytable::mutate(value = value / sum(value)) |>
    tidytable::ungroup() |>
    tidytable::arrange(value |>
      tidytable::desc())

  n <- taste_informed$taste |>
    unique() |>
    length()

  colors <- taste_informed |>
    tidytable::distinct(taste) |>
    tidytable::bind_cols(discrete_rainbow_14[1:n]) |>
    tidytable::rename(color = 2)

  col <- as.character(colors$color)
  names(col) <- as.character(colors$taste)

  plot_raw <- taste_raw |>
    tidytable::mutate(taste = taste |>
      forcats::fct_reorder(value, .desc = TRUE)) |>
    tidytable::mutate(value = value * 100) |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      area = value,
      fill = taste,
      label = value |>
        round(digits = 2) |>
        sprintf(fmt = "%0.1f%%")
    )) +
    treemapify::geom_treemap() +
    # treemapify::geom_treemap_subgroup_border() +
    treemapify::geom_treemap_text(place = "centre") +
    ggplot2::scale_fill_manual(values = col, guide = "none") +
    ggplot2::labs(title = "Classical tasting") +
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

  plot_informed <- taste_informed |>
    tidytable::mutate(taste = taste |>
      forcats::fct_reorder(value, .desc = TRUE)) |>
    tidytable::mutate(value = value * 100) |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      area = value,
      fill = taste,
      label = value |>
        round(digits = 2) |>
        sprintf(fmt = "%0.1f%%")
    )) +
    treemapify::geom_treemap() +
    # treemapify::geom_treemap_subgroup_border() +
    treemapify::geom_treemap_text(place = "centre") +
    ggplot2::scale_fill_manual(values = col) +
    ggplot2::labs(title = "Chemically informed tasting", fill = "Taste") +
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
plot_informed_tasting()

end <- Sys.time()

message("Script finished in ", format(end - start))
