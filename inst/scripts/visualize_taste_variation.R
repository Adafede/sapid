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
                    pattern = "^chasselas.*.tsv",
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

deltas_avant <- deltas |>
  dplyr::filter(grepl(pattern = "avant", x = ProductName))

deltas_après <- deltas |>
  dplyr::filter(grepl(pattern = "après", x = ProductName))

deltas_1 <- deltas_avant |>
  dplyr::filter(name == "sourness" |
                  name == "bitterness" |
                  name == "sweetness" |
                  name == "saltiness")

deltas_2 <- deltas_avant |>
  dplyr::filter(
    name == "fatness, volume" |
      name == "balance" |
      name == "freshness" |
      name == "persistency" |
      name == "mouthwatering"
  )

fig_1 <- deltas_1 %>%
  plot_ly(type = "violin") |>
  add_trace(
    x = ~ name[deltas_1$CJ == "ANOUILH DE MEST"],
    y = ~ value[deltas_1$CJ == "ANOUILH DE MEST"],
    legendgroup = "ANOUILH DE MEST",
    scalegroup = "ANOUILH DE MEST",
    name = "ANOUILH DE MEST",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[1])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "Brabant-Hyronde"],
    y = ~ value[deltas_1$CJ == "Brabant-Hyronde"],
    legendgroup = "Brabant-Hyronde",
    scalegroup = "Brabant-Hyronde",
    name = "Brabant-Hyronde",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[2])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "BUCHILLIER An"],
    y = ~ value[deltas_1$CJ == "BUCHILLIER An"],
    legendgroup = "BUCHILLIER An",
    scalegroup = "BUCHILLIER An",
    name = "BUCHILLIER An",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[3])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "DELAPRAZ Da"],
    y = ~ value[deltas_1$CJ == "DELAPRAZ Da"],
    legendgroup = "DELAPRAZ Da",
    scalegroup = "DELAPRAZ Da",
    name = "DELAPRAZ Da",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[4])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "DENEULIN Pa"],
    y = ~ value[deltas_1$CJ == "DENEULIN Pa"],
    legendgroup = "DENEULIN Pa",
    scalegroup = "DENEULIN Pa",
    name = "DENEULIN Pa",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[5])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "Dubois Co"],
    y = ~ value[deltas_1$CJ == "Dubois Co"],
    legendgroup = "Dubois Co",
    scalegroup = "Dubois Co",
    name = "Dubois Co",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[6])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "GERBER Em"],
    y = ~ value[deltas_1$CJ == "GERBER Em"],
    legendgroup = "GERBER Em",
    scalegroup = "GERBER Em",
    name = "GERBER Em",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[7])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "GROSSEN Fa"],
    y = ~ value[deltas_1$CJ == "GROSSEN Fa"],
    legendgroup = "GROSSEN Fa",
    scalegroup = "GROSSEN Fa",
    name = "GROSSEN Fa",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[8])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "HEVIN Sé"],
    y = ~ value[deltas_1$CJ == "HEVIN Sé"],
    legendgroup = "HEVIN Sé",
    scalegroup = "HEVIN Sé",
    name = "HEVIN Sé",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[9])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "LABOUCHERE Be"],
    y = ~ value[deltas_1$CJ == "LABOUCHERE Be"],
    legendgroup = "LABOUCHERE Be",
    scalegroup = "LABOUCHERE Be",
    name = "LABOUCHERE Be",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[10])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "MORET Sy"],
    y = ~ value[deltas_1$CJ == "MORET Sy"],
    legendgroup = "MORET Sy",
    scalegroup = "MORET Sy",
    name = "MORET Sy",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[11])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "PERNET An"],
    y = ~ value[deltas_1$CJ == "PERNET An"],
    legendgroup = "PERNET An",
    scalegroup = "PERNET An",
    name = "PERNET An",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[12])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "REBENAQUE Pi"],
    y = ~ value[deltas_1$CJ == "REBENAQUE Pi"],
    legendgroup = "REBENAQUE Pi",
    scalegroup = "REBENAQUE Pi",
    name = "REBENAQUE Pi",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[13])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "Stagiaire"],
    y = ~ value[deltas_1$CJ == "Stagiaire"],
    legendgroup = "Stagiaire",
    scalegroup = "Stagiaire",
    name = "Stagiaire",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[14])
  ) |>
  add_trace(
    x = ~ name[deltas_1$CJ == "WEIKERT Mé"],
    y = ~ value[deltas_1$CJ == "WEIKERT Mé"],
    legendgroup = "WEIKERT Mé",
    scalegroup = "WEIKERT Mé",
    name = "WEIKERT Mé",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[15])
  ) |>
  plotly::layout(
    yaxis = list(
      title = "value",
      zeroline = F,
      range = c(0, 10)
    ),
    xaxis = list(title = "taste"),
    violinmode = "group"
  )

fig_2 <- deltas_2 %>%
  plot_ly(type = "violin") |>
  add_trace(
    x = ~ name[deltas_2$CJ == "ANOUILH DE MEST"],
    y = ~ value[deltas_2$CJ == "ANOUILH DE MEST"],
    legendgroup = "ANOUILH DE MEST",
    scalegroup = "ANOUILH DE MEST",
    name = "ANOUILH DE MEST",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[1])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "Brabant-Hyronde"],
    y = ~ value[deltas_2$CJ == "Brabant-Hyronde"],
    legendgroup = "Brabant-Hyronde",
    scalegroup = "Brabant-Hyronde",
    name = "Brabant-Hyronde",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[2])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "BUCHILLIER An"],
    y = ~ value[deltas_2$CJ == "BUCHILLIER An"],
    legendgroup = "BUCHILLIER An",
    scalegroup = "BUCHILLIER An",
    name = "BUCHILLIER An",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[3])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "DELAPRAZ Da"],
    y = ~ value[deltas_2$CJ == "DELAPRAZ Da"],
    legendgroup = "DELAPRAZ Da",
    scalegroup = "DELAPRAZ Da",
    name = "DELAPRAZ Da",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[4])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "DENEULIN Pa"],
    y = ~ value[deltas_2$CJ == "DENEULIN Pa"],
    legendgroup = "DENEULIN Pa",
    scalegroup = "DENEULIN Pa",
    name = "DENEULIN Pa",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[5])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "Dubois Co"],
    y = ~ value[deltas_2$CJ == "Dubois Co"],
    legendgroup = "Dubois Co",
    scalegroup = "Dubois Co",
    name = "Dubois Co",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[6])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "GERBER Em"],
    y = ~ value[deltas_2$CJ == "GERBER Em"],
    legendgroup = "GERBER Em",
    scalegroup = "GERBER Em",
    name = "GERBER Em",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[7])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "GROSSEN Fa"],
    y = ~ value[deltas_2$CJ == "GROSSEN Fa"],
    legendgroup = "GROSSEN Fa",
    scalegroup = "GROSSEN Fa",
    name = "GROSSEN Fa",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[8])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "HEVIN Sé"],
    y = ~ value[deltas_2$CJ == "HEVIN Sé"],
    legendgroup = "HEVIN Sé",
    scalegroup = "HEVIN Sé",
    name = "HEVIN Sé",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[9])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "LABOUCHERE Be"],
    y = ~ value[deltas_2$CJ == "LABOUCHERE Be"],
    legendgroup = "LABOUCHERE Be",
    scalegroup = "LABOUCHERE Be",
    name = "LABOUCHERE Be",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[10])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "MORET Sy"],
    y = ~ value[deltas_2$CJ == "MORET Sy"],
    legendgroup = "MORET Sy",
    scalegroup = "MORET Sy",
    name = "MORET Sy",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[11])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "PERNET An"],
    y = ~ value[deltas_2$CJ == "PERNET An"],
    legendgroup = "PERNET An",
    scalegroup = "PERNET An",
    name = "PERNET An",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[12])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "REBENAQUE Pi"],
    y = ~ value[deltas_2$CJ == "REBENAQUE Pi"],
    legendgroup = "REBENAQUE Pi",
    scalegroup = "REBENAQUE Pi",
    name = "REBENAQUE Pi",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[13])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "Stagiaire"],
    y = ~ value[deltas_2$CJ == "Stagiaire"],
    legendgroup = "Stagiaire",
    scalegroup = "Stagiaire",
    name = "Stagiaire",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[14])
  ) |>
  add_trace(
    x = ~ name[deltas_2$CJ == "WEIKERT Mé"],
    y = ~ value[deltas_2$CJ == "WEIKERT Mé"],
    legendgroup = "WEIKERT Mé",
    scalegroup = "WEIKERT Mé",
    name = "WEIKERT Mé",
    box = list(visible = T),
    meanline = list(visible = T),
    color = I(tableau20[15])
  ) |>
  # add_markers(
  #   x = ~ jitter(name[deltas_2$CJ == "WEIKERT Mé"]),
  #   y = ~ value[deltas_2$CJ == "WEIKERT Mé"],
  #   legendgroup = "WEIKERT Mé",
  #   scalegroup = "WEIKERT Mé"
  # ) |>
  plotly::layout(
    yaxis = list(
      title = "value",
      zeroline = F,
      range = c(0, 10)
    ),
    xaxis = list(title = "taste"),
    violinmode = "group"
  )

fig <- subplot(fig_1, fig_2, nrows = 2)

fig

deltas_3 <- deltas_1 |>
  dplyr::bind_rows(deltas_2) |>
  dplyr::group_by(CJ, name) |>
  dplyr::add_count() |>
  dplyr::filter(n >= 3) |>
  dplyr::ungroup()

p <-
  ggplot2::ggplot(deltas_3, ggplot2::aes(x = CJ,  y = value, colour = CJ)) +
  ggplot2::scale_x_discrete() +
  ggthemes::scale_color_tableau(palette = "Tableau 20") +
  ggplot2::geom_violin() +
  ggplot2::geom_jitter(position = ggplot2::position_jitter(width = .05),
                       alpha = 0.5) +
  ggplot2::facet_wrap(facets = ~ as.character(name), nrow = 3)  +
  ggthemes::theme_clean() +
  ggplot2::ylab(label = "Intensity") +
  ggplot2::ylim(c(0, 10)) +
  ggplot2::theme(
    legend.position = "none",
    title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )
p
