cat("This script performs clustering of samples based on H1 NMR data. \n")

start <- Sys.time()

cat("Loading ... \n")
cat("... packages (and installing them if needed) \n")
if (!require(remotes)) {
  install.packages("remotes")
}
if (!require("AlpsNMR")) {
  install_bioc("AlpsNMR")
}
if (!require("conflicted")) {
  install.packages("conflicted")
}
if (!require(dendextend)) {
  install.packages("dendextend")
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

cat("... functions \n")
source(file = here(functions, "fitToNmr.R"))

cat("Enabling parallelization \n")
future::plan(strategy = future::multisession(workers = availableCores() - 2))

cat("NMR data was pre-treated with MestreNova, leading to the matrix below")

mestreNova_sample_matrix <-
  read_delim(
    file = here(data_inhouse_nmr_20210324_files_matrix_path),
    delim = "\t"
  ) %>%
  select(ppm = `...1`, everything(), -ncol(.)) %>%
  group_by(ppm) %>%
  pivot_longer(2:ncol(.)) %>%
  select(
    time = ppm,
    id = name,
    intensity = value
  ) %>%
  dplyr::filter(time >= 0 & time <= 14) %>%
  mutate(id = gsub(
    pattern = "#[0-9]{1}$",
    replacement = "",
    x = id
  )) %>%
  mutate(id = gsub(
    pattern = "#[0-9]{2}$",
    replacement = "",
    x = id
  )) %>%
  mutate(id = gsub(
    pattern = "10043_M[0-9]{2}/proton_",
    replacement = "",
    x = id
  )) %>%
  mutate(across(c("intensity"), abs)) %>%
  ungroup() %>%
  data.frame()

cat("manipulating to fit to NMR package format \n")
dataset <- fitToNmr(dataFrame = mestreNova_sample_matrix)

ppm_res <- AlpsNMR::nmr_ppm_resolution(dataset)[[1]]

cat(
  "The ppm resolution is: ",
  format(ppm_res, digits = 2),
  " ppm. \n",
  "Using it for interplation"
)
dataset_interpolated <-
  AlpsNMR::nmr_interpolate_1D(
    samples = dataset,
    axis = c(
      min = 0,
      max = 14,
      by = ppm_res
    )
  )

if (ENABLEDATAVIZ == TRUE) {
  plot(dataset_interpolated, chemshift_range = c(2, 7))
}

cat("Strange first column, dirty fix \n")
dataset_corrected <- dataset_interpolated

dataset_corrected$data_1r <-
  dataset_corrected$data_1r[1:nrow(dataset_corrected$data_1r), 2:ncol(dataset_corrected$data_1r)]

dataset_corrected$axis <-
  dataset_corrected$axis[2:length(dataset_corrected$axis)]

if (ENABLEDATAVIZ == TRUE) {
  plot(dataset_corrected,
    chemshift_range = c(2, 8),
    interactive = TRUE
  )
}

cat("Detecting peaks \n")
peak_table <- AlpsNMR::nmr_detect_peaks(
  nmr_dataset = dataset_corrected,
  nDivRange_ppm = 0.1,
  scales = seq(1, 16, 2),
  baselineThresh = NULL,
  SNR.Th = 3
)

cat("Integrating peaks \n")
peak_table_integration <- AlpsNMR::nmr_integrate_peak_positions(
  samples = dataset_corrected,
  peak_pos_ppm = peak_table$ppm,
  peak_width_ppm = 0.006
)

peak_table_integration <-
  AlpsNMR::get_integration_with_metadata(peak_table_integration)

cat("Keeping non-null peaks \n")
peak_table_integration_full <-
  peak_table_integration$peak_table[, colSums(is.na(peak_table_integration$peak_table)) != nrow(peak_table_integration$peak_table)] |>
  data.frame()

cat("Quick Principal Component Analysis (PCA) \n")
samplesPCA <-
  stats::prcomp(peak_table_integration_full[, c(2:ncol(peak_table_integration_full))],
    center = TRUE,
    scale. = TRUE
  )

cat("creating scree plot \n")
plotly::plot_ly(
  x = seq(1:length(samplesPCA$sdev)),
  y = samplesPCA$sdev,
  type = "bar",
  text = round(samplesPCA$sdev, 1),
) |>
  plotly::layout(
    xaxis = list(title = "Component number"),
    yaxis = list(title = "Explained variance")
  )

cat("creating PCA plot \n")
plotly::plot_ly(
  x = samplesPCA$x[, 1],
  y = samplesPCA$x[, 2],
  mode = "text",
  text = rownames(peak_table_integration_full),
  color = rownames(peak_table_integration_full)
) |>
  plotly::layout(
    xaxis = list(title = "PC 1"),
    yaxis = list(title = "PC 2")
  )

cat("creating clustered dendrogram on full set \n")
dend_pos_full <-
  peak_table_integration_full[1:69, c(2:ncol(peak_table_integration_full))] |>
  stats::dist(method = "canberra") |>
  stats::hclust() |>
  stats::as.dendrogram() |>
  dendextend::set("branches_k_color", k = 6) |>
  sort()

rownames(peak_table_integration_full) <-
  gsub("M", rownames(peak_table_integration_full), replacement = "")

source("r/colors.R")

cat("creating clustered dendrogram on restricted set \n")
dend_pos <-
  peak_table_integration_full[5:58, c(2:ncol(peak_table_integration_full))] |>
  stats::dist(method = "canberra") |>
  stats::hclust(method = "ward.D2") |>
  stats::as.dendrogram() |>
  dendextend::set("branches_k_color", k = 7, value = paired) |>
  dendextend::set("labels_colors", k = 7, value = paired) |>
  dendextend::set("labels_cex", 1.75) |>
  dendextend::set("branches_lwd", 3) |>
  sort()

cat("exporting figures \n")
# pdf(
#   file = here(figures_nmr_clusters_full_path),
#   width = 16,
#   height = 9
# )
# plot(dend_pos_full)
# dev.off()
# pdf(
#   file = here(figures_nmr_clusters_restricted_path),
#   width = 16,
#   height = 9
# )
plot(dend_pos, horiz = TRUE)
# dev.off()

# cat("additional figure (2D NMR map) \n")
#
# dataset_plot <-
#   nmr_interpolate_1D(samples = dataset,
#                      axis = c(min = 0,
#                               max = 14,
#                               by = ppm_res * 100))
#
# df <- dataset_plot[["data_1r"]] %>%
#   t() %>%
#   data.frame()
#
# df$ppm <- dataset_plot[["axis"]]
#
# df_long <- df %>%
#   group_by(ppm) %>%
#   pivot_longer(cols = c(1:(ncol(.) - 1))) %>%
#   ungroup() %>%
#   mutate(fraction = as.numeric(gsub(
#     pattern = "X",
#     replacement = "",
#     x = name
#   )) + 12) %>%
#   dplyr::filter(!is.na(value))
#
# library(scico)
#
# fig <- plot_ly(
#   data = df_long,
#   y =  ~ value,
#   x = ~ ppm,
#   z = ~ name,
#   color = ~ name,
#   colors = scico::scico(70, palette = "batlow"),
#   line = list(width = 1)
# ) %>%
#   add_lines() %>%
#   plotly::layout(showlegend = FALSE,
#                  scene = list(
#                    xaxis = list(showgrid = FALSE,
#                                 showticklabels = FALSE),
#                    yaxis = list(
#                      title = "Intensity",
#                      range = c(0, 500000),
#                      showgrid = FALSE,
#                      showticklabels = FALSE
#                    ),
#                    zaxis = list(
#                      title = "Fraction",
#                      showgrid = FALSE,
#                      showticklabels = FALSE
#                    )
#                  ))
#
# fig

p <- plot_ly(
  df_long,
  x = ~ppm,
  y = ~fraction,
  z = ~value,
  type = "contour",
  colorscale = "Viridis",
  autocontour = FALSE,
  contours = list(
    start = 0, # This is your "noise" of ELSD, all signals below won't be considered
    end = 200000, # Height of the highest point of your scale, all points above will be considered the same
    size = 20000
  ),
  line = list(width = 0.1)
) %>%
  plotly::layout(
    showlegend = FALSE,
    xaxis = list(
      title = "<b> ppm </b>",
      nticks = 15,
      range = c(14, 0)
    ),
    yaxis = list(
      title = "<b> Fraction </b>",
      nticks = 8,
      range = c(17, 71)
    )
  ) # Adapt it to your datas to give something smooth)

p

orca(p, file = "test.pdf")

cat("exporting matrix \n")
write_tsv(
  x = peak_table_integration_full,
  file = here("../03_analysis/nmr_peaks.tsv")
)

end <- Sys.time()

cat("Script finished in", format(end - start), "\n")
