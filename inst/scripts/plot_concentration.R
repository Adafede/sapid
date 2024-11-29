start <- Sys.time()

pkgload::load_all()

message("This program TODO.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")


source(file = "paths.R")
source(file = "params.R")

message("... Pipol \n") ## Where does this funny names come from?
Pipol <- readxl::read_xlsx(
  path = file.path(data_inhouse_sensory_20210329_files_excel_path),
  sheet = 4
) |>
  tidytable::mutate(concentration = factor(format(
    round(x = concentration, digits = 2),
    nsmall = 2
  ))) |>
  data.frame()

message("... AFC Pipol \n")
AFC_Pipol <- readxl::read_xlsx(path = data_inhouse_sensory_20210329_files_excel_path, sheet = 3) |>
  tidytable::mutate(concentration = format(round(x = concentration, digits = 2), nsmall = 2)) |>
  data.frame()

message(
  "... version with cleaned terms (manually for now), \n",
  "will probably be done automatically later on. \n"
)

message("... AFC Pipol \n")
AFC_Pipol_cleaned <- readxl::read_xlsx(path = data_inhouse_sensory_20210329_files_excel_path, sheet = 6) |>
  tidytable::mutate(concentration = format(round(x = concentration, digits = 2), nsmall = 2)) |>
  data.frame()

cleaned <- readxl::read_xlsx(path = data_inhouse_sensory_20210329_files_excel_path, sheet = 5) |>
  data.frame()

message("starting manipulation ... \n")
message("... joining data together \n")
joined <- Pipol |>
  tidytable::left_join(AFC_Pipol) |>
  tidytable::mutate(correct_percent = correct.responses / Total.responses)

message("... counting terms \n")
counted <- cleaned |>
  tidytable::pivot_longer(cols = tidytable::contains("attribut")) |>
  tidytable::mutate(value = gsub(
    pattern = "_.*$",
    replacement = "",
    x = value
  )) |>
  tidytable::group_by(concentration, value) |>
  tidytable::add_count() |>
  tidytable::mutate(intensity = intensity |>
    mean()) |>
  tidytable::mutate(m = n * intensity) |>
  tidytable::distinct(concentration, value, n, m) |>
  tidytable::filter(!is.na(value)) |>
  tidytable::group_by(concentration) |>
  tidytable::arrange(m, n) |>
  tidytable::mutate(value = factor(x = value, levels = value))

groups <- counted |>
  tidytable::distinct(concentration) |>
  nrow()

message("visualizing ... \n")
message("... intensity and p-value per concentration \n")
boxes <- joined |>
  ggplot2::ggplot(mapping = ggplot2::aes(
    x = concentration,
    y = intensity,
    color = as.numeric(concentration)
  )) +
  ggplot2::geom_violin() +
  ggplot2::geom_jitter(
    position = ggplot2::position_jitter(width = .05),
    alpha = 0.5
  ) +
  ggplot2::scale_color_gradient2(
    low = "#f7fcf5",
    mid = "#74c476",
    high = "#00441b"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme_minimal() +
  ggplot2::xlab(label = "Concentration [mg/L]") +
  ggplot2::ylab(label = "Intensity") +
  ggplot2::theme(
    legend.position = "none",
    panel.grid = ggplot2::element_blank(),
    text = ggplot2::element_text(face = "bold")
  )
boxes

scurve <- joined |>
  ggplot2::ggplot(mapping = ggplot2::aes(
    x = as.numeric(concentration),
    y = correct_percent,
    color = as.numeric(concentration)
  )) +
  ggplot2::geom_point() +
  ggplot2::scale_x_log10() +
  ggbump::geom_sigmoid(data = joined, ggplot2::aes(
    x = min(as.numeric(concentration)),
    xend = max(as.numeric(concentration)),
    y = min(correct_percent),
    yend = max(correct_percent)
  )) +
  ggplot2::scale_color_gradient2(
    low = "#f7fcf5",
    mid = "#74c476",
    high = "#00441b"
  ) +
  ggplot2::xlab(label = "Concentration [mg/L]") +
  ggplot2::ylab("Correct answers [%]") +
  ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme_minimal() +
  ggplot2::geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "grey"
  ) +
  ggplot2::geom_vline(
    xintercept = 0.12,
    linetype = "dashed",
    color = "grey"
  ) +
  ggplot2::theme(
    legend.position = "none",
    text = ggplot2::element_text(face = "bold")
  )
scurve

message("... terms per concentration \n")
dots <- counted |>
  ggplot2::ggplot() +
  ggplot2::geom_segment(mapping = ggplot2::aes(
    x = value,
    xend = value,
    y = 0,
    yend = n
  ), color = "grey") +
  ggplot2::geom_point(mapping = ggplot2::aes(x = value, y = n, color = concentration), size = 3) +
  ggplot2::scale_color_gradient2(
    low = "#f7fcf5",
    mid = "#74c476",
    high = "#00441b"
  ) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    panel.border = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(0.1, "lines"),
    strip.text.x = ggplot2::element_text(size = 8)
  ) +
  ggplot2::xlab("") +
  ggplot2::ylab("Count") +
  ggplot2::facet_wrap(
    facets = ~ round(x = concentration, digits = 2),
    ncol = 1,
    scales = "free_y"
  )

dots

message("... terms multiplied by mean intensity per concentration \n")
my7greens <- c(
  "#edf8e9",
  "#c7e9c0",
  "#a1d99b",
  "#74c476",
  "#41ab5d",
  "#238b45",
  "#005a32"
)
dots_corrected <- counted |>
  ggplot2::ggplot() +
  ggplot2::geom_segment(mapping = ggplot2::aes(
    x = value,
    xend = value,
    y = 0,
    yend = m
  ), color = "grey") +
  ggplot2::geom_point(mapping = ggplot2::aes(x = value, color = concentration, y = m), size = 3) +
  ggplot2::scale_color_gradient2(
    low = "#f7fcf5",
    mid = "#74c476",
    high = "#00441b"
  ) +
  ggplot2::coord_flip() +
  ggplot2::xlab("") +
  ggplot2::ylab("Value") +
  ggplot2::facet_wrap(
    facets = ~ paste(format(
      round(x = concentration, digits = 2),
      nsmall = 2
    ), "[mg/L]"),
    ncol = 1,
    scales = "free_y"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid = ggplot2::element_blank(),
    text = ggplot2::element_text(face = "bold"),
    panel.border = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(0.2, "lines"),
    strip.text.x = ggplot2::element_text(size = 10),
    strip.background = ggplot2::element_rect(fill = "white")
  )
dots_corrected

ggpubr::ggarrange(
  ggpubr::ggarrange(boxes, scurve, ncol = 2, labels = "AUTO"),
  dots_corrected,
  nrow = 2,
  labels = c("", "C"),
  heights = c(1, 2)
)

message("exporting figures \n")

# ggsave(
#   filename = figures_lollipop_raw_extract_path,
#   plot = dots,
#   limitsize = FALSE
# )
#
# ggsave(
#   filename = figures_lollipop_corrected_raw_extract_path,
#   plot = dots_corrected,
#   limitsize = FALSE
# )

end <- Sys.time()

message("Script finished in ", format(end - start))
