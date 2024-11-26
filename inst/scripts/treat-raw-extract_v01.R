cat("This script is a first trial with sensorial analysis. \n")

start <- Sys.time()

cat("Loading ... \n")
cat("... packages (and installing them if needed) \n")
if (!require(conflicted)) {
  install.packages("conflicted")
}
if (!require(dendextend)) {
  install.packages("dendextend")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(here)) {
  install.packages("here")
}
if (!require(plotly)) {
  install.packages("plotly")
}
if (!require(RColorBrewer)) {
  install.packages("RColorBrewer")
}
if (!require(readxl)) {
  install.packages("readxl")
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
}

cat("... paths and parameters \n")
source(file = here("paths.R"))
source(file = here("params.R"))

cat("... functions \n")

cat("... files ... \n")

cat("... Pipol \n") ## Where does this funny names come from?
Pipol <- read_xlsx(path = here(file.path(
  data_inhouse_sensory_20210329_files_excel_path
)),
sheet = 4) %>%
  mutate(concentration = factor(format(
    round(x = concentration,
          digits = 2), nsmall = 2
  ))) %>%
  data.frame()

cat("... AFC Pipol \n")
AFC_Pipol <- read_xlsx(
  path = here(data_inhouse_sensory_20210329_files_excel_path),
  sheet = 3
) %>%
  mutate(concentration = format(round(x = concentration,
                                      digits = 2), nsmall = 2)) %>%
  data.frame()

cat(
  "... version with cleaned terms (manually for now), \n",
  "will probably be done automatically later on. \n"
)

cat("... AFC Pipol \n")
AFC_Pipol_cleaned <- read_xlsx(
  path = here(data_inhouse_sensory_20210329_files_excel_path),
  sheet = 6
) %>%
  mutate(concentration = format(round(x = concentration,
                                      digits = 2), nsmall = 2)) %>%
  data.frame()

cleaned <- read_xlsx(
  path = here(data_inhouse_sensory_20210329_files_excel_path),
  sheet = 5
) %>%
  data.frame()

cat("starting manipulation ... \n")
cat("... joining data together \n")
joined <- left_join(Pipol, AFC_Pipol) %>%
  mutate(correct_percent = correct.responses / Total.responses)

cat("... counting terms \n")
counted <- cleaned %>%
  pivot_longer(cols = colnames(.)[grepl(pattern = "attribut",
                                        x = colnames(.),
                                        fixed = TRUE)]) %>%
  mutate(value = gsub(
    pattern = "_.*$",
    replacement = "",
    x = value
  )) %>%
  group_by(concentration,
           value) %>%
  add_count() %>%
  mutate(intensity = mean(intensity)) %>%
  mutate(m = n * intensity) %>%
  distinct(concentration,
           value,
           n,
           m) %>%
  dplyr::filter(!is.na(value)) %>%
  group_by(concentration) %>%
  arrange(m,
          n) %>%
  mutate(value = factor(x = value,
                        levels = value))

groups <- nrow(counted %>% distinct(concentration))

cat("visualizing ... \n")
cat("... intensity and p-value per concentration \n")
boxes <-
  ggplot2::ggplot(data = joined,
                  mapping = ggplot2::aes(x = concentration,  y = intensity)) +
  ggplot2::geom_violin() +
  ggplot2::geom_jitter(position = ggplot2::position_jitter(width = .05),
                       alpha = 0.5) +
  ggplot2::theme_bw() +
  ggplot2::theme_minimal() +
  ggplot2::xlab(label = "Concentration [mg/L]") +
  ggplot2::ylab(label = "Intensity") +
  ggplot2::theme(
    legend.position = "right",
    panel.grid = element_blank(),
    text = ggplot2::element_text(face = "bold")
  )
boxes

scurve <-
  ggplot2::ggplot(joined, ggplot2::aes(as.numeric(concentration), correct_percent)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_log10() +
  ggbump::geom_sigmoid(data = joined, ggplot2::aes(
    x = min(as.numeric(concentration)),
    xend = max(as.numeric(concentration)),
    y = min(correct_percent),
    yend = max(correct_percent)
  )) +
  ggplot2::xlab(label = "Concentration [mg/L]") +
  ggplot2::ylab("Correct answers [%]") +
  ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme_minimal() +
  ggplot2::geom_hline(yintercept = 0.5,
                      linetype = "dashed",
                      color = "grey") +
  ggplot2::geom_vline(xintercept = 0.12,
                      linetype = "dashed",
                      color = "grey") +
  ggplot2::theme(text = ggplot2::element_text(face = "bold"))
scurve

cat("... terms per concentration \n")
dots <- ggplot(counted) +
  geom_segment(aes(
    x = value,
    xend = value,
    y = 0,
    yend = n
  ), color = "grey") +
  geom_point(aes(x = value, y = n, color = concentration), size = 3) +
  scale_color_gradient2(low = "#f7fcf5", mid = "#74c476", high = "#00441b") +
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Count") +
  facet_wrap(
    facets = ~ round(x = concentration, digits = 2),
    ncol = 1,
    scales = "free_y"
  )

dots

cat("... terms multiplied by mean intensity per concentration \n")
my7greens <- c("#edf8e9",
               "#c7e9c0",
               "#a1d99b",
               "#74c476",
               "#41ab5d",
               "#238b45",
               "#005a32")
dots_corrected <- ggplot(counted) +
  geom_segment(aes(
    x = value,
    xend = value,
    y = 0,
    yend = m
  ), color = "grey") +
  geom_point(aes(x = value,
                 # color = concentration,
                 y = m),
             size = 3) +
  scale_color_gradient2(low = "#f7fcf5", mid = "#74c476", high = "#00441b") +
  coord_flip() +
  xlab("") +
  ylab("Value") +
  facet_wrap(
    facets = ~ paste(format(
      round(x = concentration, digits = 2), nsmall = 2
    ), "[mg/L]"),
    ncol = 1,
    scales = "free_y"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    text = element_text(face = "bold"),
    panel.border = element_blank(),
    panel.spacing = unit(0.2, "lines"),
    strip.text.x = element_text(size = 10),
    strip.background = element_rect(fill = "white")
  )
dots_corrected

ggpubr::ggarrange(
  ggpubr::ggarrange(boxes, scurve, ncol = 2, labels = "AUTO"),
  dots_corrected,
  nrow = 2,
  labels = c("", "C"),
  heights = c(1, 2)
)

cat("exporting figures \n")

## needs orca to be installed
# setwd(dir = here())
# orca(
#   p = boxes,
#   file = figures_boxes_raw_extract_path,
#   width = 800,
#   height = 450
# )
#
# orca(
#   p = bars,
#   file = figures_bars_raw_extract_path,
#   width = 800,
#   height = 450
# )
#
# ggsave(
#   filename = here(figures_lollipop_raw_extract_path),
#   plot = dots,
#   limitsize = FALSE
# )
#
# ggsave(
#   filename = here(figures_lollipop_corrected_raw_extract_path),
#   plot = dots_corrected,
#   limitsize = FALSE
# )

end <- Sys.time()

cat("Script finished in", format(end - start), "\n")
