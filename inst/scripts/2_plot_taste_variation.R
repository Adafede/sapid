start <- Sys.time()

pkgload::load_all()

message("This program TODO.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

source(file = "paths.R")
source(file = "params.R")
source(file = "r/colors.R")

message("... files ... \n")
files <- list.files(
  path = analysis_path_04_output,
  pattern = "^chasselas.*.tsv",
  full.names = TRUE
)

message("... profile \n")
filesList <- files |>
  furrr::future_map(.f = tidytable::fread)

deltas <- filesList |>
  tidyfst::rbindlist(fill = TRUE, idcol = FALSE) |>
  tidytable::mutate(name = name |>
    gsub(pattern = "acide.*", replacement = "sourness")) |>
  tidytable::mutate(name = name |>
    gsub(pattern = "amer.*", replacement = "bitterness")) |>
  tidytable::mutate(name = name |>
    gsub(pattern = "sucré.*", replacement = "sweetness")) |>
  tidytable::mutate(name = name |>
    gsub(pattern = "salé.*", replacement = "saltiness")) |>
  tidytable::mutate(name = name |>
    gsub(pattern = "gras.*", replacement = "fatness, volume")) |>
  tidytable::mutate(name = name |>
    gsub(pattern = "équilibre.*", replacement = "balance")) |>
  tidytable::mutate(name = name |>
    gsub(pattern = "fraicheur.*", replacement = "freshness")) |>
  tidytable::mutate(name = name |>
    gsub(pattern = "longueur.*", replacement = "persistency")) |>
  tidytable::mutate(name = name |>
    gsub(pattern = "salivant.*", replacement = "mouthwatering"))

deltas_avant <- deltas |>
  tidytable::filter(grepl(pattern = "avant", x = ProductName))

deltas_après <- deltas |>
  tidytable::filter(grepl(pattern = "après", x = ProductName))

deltas_1 <- deltas_avant |>
  tidytable::filter(name == "sourness" |
    name == "bitterness" |
    name == "sweetness" |
    name == "saltiness")

deltas_2 <- deltas_avant |>
  tidytable::filter(
    name == "fatness, volume" |
      name == "balance" |
      name == "freshness" |
      name == "persistency" |
      name == "mouthwatering"
  )

deltas_3 <- deltas_1 |>
  tidytable::bind_rows(deltas_2) |>
  tidytable::group_by(CJ, name) |>
  tidytable::add_count() |>
  tidytable::filter(n >= 3) |>
  tidytable::ungroup()

p_1 <-
  ggplot2::ggplot(deltas_3, ggplot2::aes(x = CJ, y = value, colour = CJ)) +
  ggplot2::scale_x_discrete() +
  ggthemes::scale_color_tableau(palette = "Tableau 20") +
  ggplot2::geom_violin() +
  ggplot2::geom_jitter(
    position = ggplot2::position_jitter(width = .05),
    alpha = 0.5
  ) +
  ggplot2::facet_wrap(facets = ~ as.character(name), ncol = 3) +
  ggplot2::theme_minimal() +
  ggplot2::ylab(label = "Intensity") +
  ggplot2::ylim(c(0, 10)) +
  ggplot2::labs(colour = "Panelist") +
  ggplot2::theme(
    legend.position = "right",
    title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )
p_1

p_2 <-
  ggplot2::ggplot(deltas_3, ggplot2::aes(x = name, y = value, colour = name)) +
  ggplot2::scale_x_discrete() +
  ggthemes::scale_color_tableau(palette = "Tableau 20") +
  ggplot2::geom_violin() +
  ggplot2::geom_jitter(
    position = ggplot2::position_jitter(width = .05),
    alpha = 0.5
  ) +
  ggplot2::facet_wrap(facets = ~ as.character(CJ), ncol = 3) +
  ggplot2::theme_minimal() +
  ggplot2::ylab(label = "Intensity") +
  ggplot2::ylim(c(0, 10)) +
  ggplot2::labs(colour = "Panelist") +
  ggplot2::theme(
    legend.position = "right",
    title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )
p_2

ggpubr::ggarrange(p_1,
  p_2,
  nrow = 2,
  labels = "AUTO",
  align = "hv"
)

end <- Sys.time()

message("Script finished in ", format(end - start))
