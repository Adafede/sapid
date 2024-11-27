library(corrplot)
library(data.table)
library(dplyr)
library(plotly)
library(stringr)

min_fraction <- 17

max_fraction <- 71

temp_ms_path <- "~/switchdrive/SAPERE/03_analysis/20210701_10043.csv"

temp_nmr_path <- "~/switchdrive/SAPERE/03_analysis/nmr_peaks.tsv"

ms_table <- data.table::fread(file = temp_ms_path)

nmr_table <- data.table::fread(file = temp_nmr_path) |>
  data.frame()

rownames(nmr_table) <-
  gsub(
    pattern = "M",
    replacement = "",
    x = nmr_table$NMRExperiment
  )

nmr_table_2 <- nmr_table %>%
  dplyr::select(-NMRExperiment) %>%
  dplyr::filter(rownames(.) >= min_fraction &
    rownames(.) <= max_fraction) %>%
  scale() %>%
  data.frame()

colnames(ms_table) <-
  gsub(
    pattern = "_[0-9]{2}.mzML Peak area",
    replacement = "",
    x = colnames(ms_table)
  ) |>
  gsub(
    pattern = ".mzML Peak area",
    replacement = ""
  ) |>
  gsub(
    pattern = "210619_AR_[0-9]{2}_[A-Z]{1}_",
    replacement = ""
  )

ms_table <- ms_table[, c(4:(ncol(ms_table) - 1)), with = FALSE] |>
  t() |>
  data.frame()

rownames(ms_table) <-
  gsub(
    pattern = "X",
    replacement = "",
    x = rownames(ms_table)
  )

ms_table_2 <- ms_table %>%
  dplyr::filter(stringr::str_count(rownames(.)) == 2) %>%
  dplyr::filter(rownames(.) >= min_fraction &
    rownames(.) <= max_fraction)

ms_table_2 <- ms_table_2[, which(colSums(ms_table_2) != 0)] %>%
  scale() %>%
  data.frame()

samplesPCA_ms <-
  prcomp(
    x = ms_table_2,
    center = FALSE,
    scale. = FALSE
  )

samplesPCA_nmr <-
  prcomp(
    x = nmr_table_2,
    center = FALSE,
    scale. = FALSE
  )

message("creating scree plot \n")
plot_ly(
  x = seq(1:length(samplesPCA_nmr$sdev)),
  y = samplesPCA_nmr$sdev,
  type = "bar",
  text = round(samplesPCA_nmr$sdev, 1),
) %>%
  plotly::layout(
    xaxis = list(title = "Component number"),
    yaxis = list(title = "Explained variance")
  )

message("creating PCA plot \n")
plot_ly(
  x = samplesPCA_nmr$x[, 1],
  y = samplesPCA_nmr$x[, 2],
  mode = "text",
  text = rownames(nmr_table_2),
  color = rownames(nmr_table_2)
) %>%
  plotly::layout(
    xaxis = list(title = "PC 1"),
    yaxis = list(title = "PC 2")
  )

message("creating scree plot \n")
plotly::plot_ly(
  x = seq(1:length(samplesPCA_ms$sdev)),
  y = samplesPCA_ms$sdev,
  type = "bar",
  text = round(samplesPCA_ms$sdev, 1)
) %>%
  plotly::layout(
    xaxis = list(title = "Component number"),
    yaxis = list(title = "Explained variance")
  )

message("creating PCA plot \n")
plotly::plot_ly(
  x = samplesPCA_ms$x[, 1],
  y = samplesPCA_ms$x[, 2],
  mode = "text",
  text = rownames(ms_table_2),
  color = rownames(ms_table_2)
) %>%
  plotly::layout(
    xaxis = list(title = "PC 1"),
    yaxis = list(title = "PC 2")
  )

cor.mat <- cor(ms_table_2, nmr_table_2)

# corrplot(
#   cor.mat,
#   type = "upper",
#   order = "hclust",
#   tl.col = "black",
#   tl.srt = 45
# )

library(heatmap3)
# heatmap3(
#   x = cor.mat,
#   showColDendro = FALSE,
#   showRowDendro = FALSE,
#   useRaster = TRUE
# )
