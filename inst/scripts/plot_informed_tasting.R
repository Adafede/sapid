start <- Sys.time()

pkgload::load_all()

message("This program plots a comparison between classical and chemically informed analysis.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

plot_informed_tasting <- function(file_taste_raw = "inst/extdata/concentration_afc.tsv",
                                  filte_taste_informed = "inst/extdata/profiles.tsv",
                                  min_panelists = 2) {
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
  taste_informed <- filte_taste_informed |>
    tidytable::fread() |>
    tidytable::rename(taste = taste_harmonized) |>
    tidytable::group_by(taste_original) |>
    tidytable::mutate(n = tidytable::n_distinct(jury)) |>
    tidytable::filter(n >= min_panelists) |>
    tidytable::group_by(taste) |>
    tidytable::summarize(value = sum(value)) |>
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
    ggplot2::ggplot(mapping = ggplot2::aes(
      area = value,
      fill = taste,
      label = value |>
        round(digits = 2) |>
        format(nsmall = 2)
    )) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(place = "centre") +
    ggplot2::scale_fill_manual(values = col, guide = "none") +
    ggplot2::labs(caption = "Classical tasting")

  plot_informed <- taste_informed |>
    tidytable::mutate(taste = taste |>
      forcats::fct_reorder(value, .desc = TRUE)) |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      area = value,
      fill = taste,
      label = value |>
        round(digits = 2) |>
        format(nsmall = 2)
    )) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(place = "centre") +
    ggplot2::scale_fill_manual(values = col) +
    ggplot2::labs(caption = "Chemically informed tasting", fill = "Taste")

  ggpubr::ggarrange(
    plotlist = list(plot_raw, plot_informed),
    ncol = 2,
    legend = "right",
    common.legend = TRUE
  )
}
plot_informed_tasting()

end <- Sys.time()

message("Script finished in ", format(end - start))
