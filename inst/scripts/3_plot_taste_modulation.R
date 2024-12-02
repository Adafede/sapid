start <- Sys.time()

pkgload::load_all()

message("This program TODO.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

message("... files ... \n")
file <- "~/switchdrive/SAPERE/03_analysis/04_fractions-sensory/03_output/deltas_prepared.tsv"

message("... profile \n")
table <- file |>
  tidytable::fread()

deltas <- table |>
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

deltas_1 <- deltas |>
  tidytable::filter(name == "sourness" |
    name == "bitterness" |
    name == "sweetness" |
    name == "saltiness")

deltas_2 <- deltas |>
  tidytable::filter(
    name == "fatness, volume" |
      name == "balance" |
      name == "freshness" |
      name == "persistency" |
      name == "mouthwatering"
  )

deltas_1_new <- deltas_1 |>
  # tidytable::filter(CJ != "Dubois Co") |>
  tidytable::distinct()

deltas_2_new <- deltas_2 |>
  # tidytable::filter(CJ != "Dubois Co") |>
  tidytable::distinct()

deltas_3 <- deltas_1 |>
  tidytable::bind_rows(deltas_2) |>
  tidytable::rowwise() |>
  tidytable::mutate(group = switch(as.character(Date),
    "2021-06-07" = "Fractions 17-21",
    "2021-04-19" = "Fractions 22-31",
    "2021-05-17" = "Fractions 32-39",
    "2021-05-31" = "Fractions 40-45",
    "2021-04-26" = "Fractions 46-54",
    "2021-04-12" = "Fractions 55-63",
    "2021-05-10" = "Fractions 64-71"
  )) |>
  dplyr::ungroup()

p_values_wilcox <- deltas_3 |>
  tidytable::group_by(name, group) |>
  tidytable::summarize(p_value_wilcox = wilcox.test(delta, mu = 0)$p.value)

p_values_sign <- deltas_3 |>
  tidytable::group_by(name, group) |>
  tidytable::summarize(p_value_sign = BSDA::SIGN.test(delta, md = 0)$p.value)

p_values_wilcox <- p_values_wilcox |>
  tidytable::mutate(
    stars_wilcox = tidytable::case_when(
      p_value_wilcox < 0.001 ~ "***",
      p_value_wilcox < 0.01 ~ "**",
      p_value_wilcox < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Now, map significance levels to stars
p_values_sign <- p_values_sign |>
  tidytable::mutate(
    stars = tidytable::case_when(
      p_value_sign < 0.001 ~ "***",
      p_value_sign < 0.01 ~ "**",
      p_value_sign < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

deltas_4 <- deltas_3 |>
  # tidytable::left_join(p_values_wilcox) |>
  tidytable::left_join(p_values_sign)

p_1 <-
  ggplot2::ggplot(deltas_4, ggplot2::aes(x = name, y = delta, colour = name)) +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  ggplot2::geom_violin() +
  ggplot2::geom_jitter(
    position = ggplot2::position_jitter(width = .05),
    alpha = 0.5
  ) +
  ggplot2::facet_wrap(facets = ~group, scales = "free") +
  ggplot2::theme_minimal() +
  ggplot2::ylab(label = "Intensity difference") +
  ggplot2::labs(colour = "Taste") +
  ggplot2::ylim(-8, 8) +
  ggplot2::theme(
    legend.position = "right",
    title = ggplot2::element_text(face = "bold"),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(face = "bold"),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
  ) +
  # ggplot2::geom_text(aes(x = name, y = 5, label = stars_wilcox),
  #                    color = "red", size = 8, vjust = -0.5) +
  ggplot2::geom_text(
    ggplot2::aes(x = name, y = 5, label = stars),
    color = "black",
    size = 8,
    vjust = -0.5
  ) +
  ggplot2::labs(caption = "* = p-value < 0.05 (Sign test)")
p_1

# Plot with stars added for significance
p_2 <- ggplot2::ggplot(deltas_4, ggplot2::aes(x = group, y = delta, colour = group)) +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  ggplot2::geom_violin() +
  ggplot2::geom_jitter(
    position = ggplot2::position_jitter(width = .05),
    alpha = 0.5
  ) +
  ggplot2::facet_wrap(facets = ~name, scales = "free") +
  ggplot2::theme_minimal() +
  ggplot2::ylab(label = "Intensity difference") +
  ggplot2::labs(colour = "Group") +
  ggplot2::ylim(-8, 8) +
  ggplot2::theme(
    legend.position = "right",
    title = ggplot2::element_text(face = "bold"),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(face = "bold"),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) +
  # ggplot2::geom_text(aes(x = group, y = 5, label = stars_wilcox),
  #                    color = "red", size = 8, vjust = -0.5) +
  ggplot2::geom_text(
    ggplot2::aes(x = group, y = 5, label = stars),
    color = "black",
    size = 8,
    vjust = -0.5
  ) +
  ggplot2::labs(caption = "* = p-value < 0.05 (Sign test)")

p_2

ggpubr::ggarrange(p_1,
  p_2,
  nrow = 2,
  labels = "AUTO",
  align = "hv"
)

end <- Sys.time()

message("Script finished in ", format(end - start))
