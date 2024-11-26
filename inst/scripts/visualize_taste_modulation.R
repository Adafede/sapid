cat("This script is made to treat all chasselas together. \n")

start <- Sys.time()

cat("Loading ... \n")
cat("... packages (and installing them if needed) \n")
if (!require(conflicted)) {
  install.packages("conflicted")
}
if (!require(data.table)) {
  install.packages("data.table")
}
if (!require(here)) {
  install.packages("here")
}
if (!require(plotly)) {
  install.packages("plotly")
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
}

cat("... paths and parameters \n")
source(file = here("paths.R"))
source(file = here("params.R"))
source(file = here("r/colors.R"))

cat("... functions \n")

cat("... files ... \n")
files <- list.files(path = analysis_path_04_output,
                    pattern = "^deltas.*.tsv",
                    full.names = TRUE)

cat("... profile \n")
filesList <- lapply(files, read_tsv)

deltas <- rbindlist(l = filesList, fill = TRUE, idcol = FALSE) |>
  mutate(name = gsub(
    pattern = "acide.*",
    replacement = "sourness",
    x = name
  )) |>
  mutate(name = gsub(
    pattern = "amer.*",
    replacement = "bitterness",
    x = name
  )) |>
  mutate(name = gsub(
    pattern = "sucré.*",
    replacement = "sweetness",
    x = name
  )) |>
  mutate(name = gsub(
    pattern = "salé.*",
    replacement = "saltiness",
    x = name
  )) |>
  mutate(name = gsub(
    pattern = "gras.*",
    replacement = "fatness, volume",
    x = name
  )) |>
  mutate(name = gsub(
    pattern = "équilibre.*",
    replacement = "balance",
    x = name
  )) |>
  mutate(name = gsub(
    pattern = "fraicheur.*",
    replacement = "freshness",
    x = name
  )) |>
  mutate(name = gsub(
    pattern = "longueur.*",
    replacement = "persistency",
    x = name
  )) |>
  mutate(name = gsub(
    pattern = "salivant.*",
    replacement = "mouthwatering",
    x = name
  ))

deltas_1 <- deltas |>
  dplyr::filter(name == "sourness" |
                  name == "bitterness" |
                  name == "sweetness" |
                  name == "saltiness")

deltas_2 <- deltas |>
  dplyr::filter(
    name == "fatness, volume" |
      name == "balance" |
      name == "freshness" |
      name == "persistency" |
      name == "mouthwatering"
  )

deltas_1_new <- deltas_1 |>
  dplyr::filter(CJ != "Dubois Co")

deltas_2_new <- deltas_2 |>
  dplyr::filter(CJ != "Dubois Co")

fig_1 <- deltas_1 %>%
  plot_ly(type = "violin") |>
  add_trace(
    x = ~ name[deltas_1$Date == as.Date("2021-04-12")],
    y = ~ delta[deltas_1$Date == as.Date("2021-04-12")],
    legendgroup = "cluster 6",
    scalegroup = "cluster 6",
    name = "cluster 6",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[1])
  ) |>
  add_trace(
    x = ~ name[deltas_1$Date == as.Date("2021-04-19")],
    y = ~ delta[deltas_1$Date == as.Date("2021-04-19")],
    legendgroup = "cluster 2",
    scalegroup = "cluster 2",
    name = "cluster 2",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[2])
  ) |>
  add_trace(
    x = ~ name[deltas_1$Date == as.Date("2021-04-26")],
    y = ~ delta[deltas_1$Date == as.Date("2021-04-26")],
    legendgroup = "cluster 5",
    scalegroup = "cluster 5",
    name = "cluster 5",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[3])
  ) |>
  add_trace(
    x = ~ name[deltas_1$Date == as.Date("2021-05-10")],
    y = ~ delta[deltas_1$Date == as.Date("2021-05-10")],
    legendgroup = "cluster 7",
    scalegroup = "cluster 7",
    name = "cluster 7",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[4])
  ) |>
  add_trace(
    x = ~ name[deltas_1$Date == as.Date("2021-05-17")],
    y = ~ delta[deltas_1$Date == as.Date("2021-05-17")],
    legendgroup = "cluster 3",
    scalegroup = "cluster 3",
    name = "cluster 3",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[5])
  ) |>
  add_trace(
    x = ~ name[deltas_1$Date == as.Date("2021-05-31")],
    y = ~ delta[deltas_1$Date == as.Date("2021-05-31")],
    legendgroup = "cluster 4",
    scalegroup = "cluster 4",
    name = "cluster 4",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[6])
  ) |>
  add_trace(
    x = ~ name[deltas_1$Date == as.Date("2021-06-07")],
    y = ~ delta[deltas_1$Date == as.Date("2021-06-07")],
    legendgroup = "cluster 1",
    scalegroup = "cluster 1",
    name = "cluster 1",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[7])
  ) |>
  # add_segments(
  #   x = "sourness",
  #   xend = "mouthwatering",
  #   y = 1,
  #   yend = ~ 1,
  #   line = list(dash = "dash"),
  #   color = I("black"),
  #   showlegend = FALSE
  # ) |>
  # add_segments(
  #   x = "sourness",
#   xend = "mouthwatering",
#   y = -1,
#   yend = ~ -1,
#   line = list(dash = "dash"),
#   color = I("black"),
#   showlegend = FALSE
# ) |>
plotly::layout(
  yaxis = list(title = "delta", zeroline = F),
  xaxis = list(title = "taste"),
  violinmode = "group"
)

fig_2 <- deltas_2 %>%
  plot_ly(type = "violin") |>
  add_trace(
    x = ~ name[deltas_2$Date == as.Date("2021-04-12")],
    y = ~ delta[deltas_2$Date == as.Date("2021-04-12")],
    legendgroup = "cluster 6",
    scalegroup = "cluster 6",
    name = "cluster 6",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[1])
  ) |>
  add_trace(
    x = ~ name[deltas_2$Date == as.Date("2021-04-19")],
    y = ~ delta[deltas_2$Date == as.Date("2021-04-19")],
    legendgroup = "cluster 2",
    scalegroup = "cluster 2",
    name = "cluster 2",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[2])
  ) |>
  add_trace(
    x = ~ name[deltas_2$Date == as.Date("2021-04-26")],
    y = ~ delta[deltas_2$Date == as.Date("2021-04-26")],
    legendgroup = "cluster 5",
    scalegroup = "cluster 5",
    name = "cluster 5",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[3])
  ) |>
  add_trace(
    x = ~ name[deltas_2$Date == as.Date("2021-05-10")],
    y = ~ delta[deltas_2$Date == as.Date("2021-05-10")],
    legendgroup = "cluster 7",
    scalegroup = "cluster 7",
    name = "cluster 7",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[4])
  ) |>
  add_trace(
    x = ~ name[deltas_2$Date == as.Date("2021-05-17")],
    y = ~ delta[deltas_2$Date == as.Date("2021-05-17")],
    legendgroup = "cluster 3",
    scalegroup = "cluster 3",
    name = "cluster 3",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[5])
  ) |>
  add_trace(
    x = ~ name[deltas_2$Date == as.Date("2021-05-31")],
    y = ~ delta[deltas_2$Date == as.Date("2021-05-31")],
    legendgroup = "cluster 4",
    scalegroup = "cluster 4",
    name = "cluster 4",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[6])
  ) |>
  add_trace(
    x = ~ name[deltas_2$Date == as.Date("2021-06-07")],
    y = ~ delta[deltas_2$Date == as.Date("2021-06-07")],
    legendgroup = "cluster 1",
    scalegroup = "cluster 1",
    name = "cluster 1",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    color = I(paired[7])
  ) |>
  # add_segments(
  #   x = "sourness",
  #   xend = "mouthwatering",
  #   y = 1,
  #   yend = ~ 1,
  #   line = list(dash = "dash"),
  #   color = I("black"),
  #   showlegend = FALSE
  # ) |>
  # add_segments(
  #   x = "sourness",
#   xend = "mouthwatering",
#   y = -1,
#   yend = ~ -1,
#   line = list(dash = "dash"),
#   color = I("black"),
#   showlegend = FALSE
# ) |>
plotly::layout(
  yaxis = list(title = "delta", zeroline = F),
  xaxis = list(title = "taste"),
  violinmode = "group"
)

deltas_3 <- deltas_1 |>
  dplyr::bind_rows(deltas_2) |>
  dplyr::rowwise() |>
  dplyr::mutate(group = switch(
    as.character(Date),
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
  tidytable::summarize(
    p_value_sign = BSDA::SIGN.test(delta, md = 0)$p.value
  )

p_values_wilcox <- p_values_wilcox |> 
  tidytable::mutate(stars_wilcox = tidytable::case_when(
    p_value_wilcox < 0.001 ~ "***",
    p_value_wilcox < 0.01  ~ "**",
    p_value_wilcox < 0.05  ~ "*",
    TRUE            ~ ""
  ))

# Now, map significance levels to stars
p_values_sign <- p_values_sign |> 
  tidytable::mutate(stars = tidytable::case_when(
    p_value_sign < 0.001 ~ "***",
    p_value_sign < 0.01  ~ "**",
    p_value_sign < 0.05  ~ "*",
    TRUE            ~ ""
  ))

deltas_4 <- deltas_3 |> 
  # tidytable::left_join(p_values_wilcox) |> 
  tidytable::left_join(p_values_sign)

p_1 <-
  ggplot2::ggplot(deltas_4, ggplot2::aes(x = name,  y = delta, colour = name)) +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  ggplot2::geom_violin() +
  ggplot2::geom_jitter(position = ggplot2::position_jitter(width = .05),
                       alpha = 0.5) +
  ggplot2::facet_wrap(facets = ~ group, scales = "free")  +
  ggplot2::theme_bw() +
  ggplot2::theme_minimal() +
  ggplot2::ylab(label = "Intensity difference") +
  ggplot2::labs(colour = "Taste") +
  ggplot2::ylim(-8,8) +
  ggplot2::theme(
    legend.position = "right",
    title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
  ) +
  # ggplot2::geom_text(aes(x = name, y = 5, label = stars_wilcox),
  #                    color = "red", size = 8, vjust = -0.5) +
  ggplot2::geom_text(aes(x = name, y = 5, label = stars),
                     color = "black", size = 8, vjust = -0.5) +
  ggplot2::labs(caption = "* = p-value < 0.05 (Sign test)")
p_1

# Plot with stars added for significance
p_2 <- ggplot2::ggplot(deltas_4, ggplot2::aes(x = group,  y = delta, colour = group)) +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  ggplot2::geom_violin() +
  ggplot2::geom_jitter(position = ggplot2::position_jitter(width = .05),
                       alpha = 0.5) +
  ggplot2::facet_wrap(facets = ~ name, scales = "free")  +
  ggplot2::theme_bw() +
  ggplot2::theme_minimal() +
  ggplot2::ylab(label = "Intensity difference") +
  ggplot2::labs(colour = "Group") +
  ggplot2::ylim(-8,8) +
  ggplot2::theme(
    legend.position = "right",
    title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) +
  # ggplot2::geom_text(aes(x = group, y = 5, label = stars_wilcox),
  #                    color = "red", size = 8, vjust = -0.5) +
  ggplot2::geom_text(aes(x = group, y = 5, label = stars),
                     color = "black", size = 8, vjust = -0.5) +
  ggplot2::labs(caption = "* = p-value < 0.05 (Sign test)")

p_2

ggpubr::ggarrange(p_1, p_2, nrow = 2, labels = "AUTO",align = "hv")

