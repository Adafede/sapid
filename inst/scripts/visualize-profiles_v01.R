cat("This script is made to treat FLASH PROFILE results. \n")

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
if (!require(microshades)) {
  install.packages("microshades")
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
files <- list.files(
  path = analysis_path_04_output,
  pattern = "^profile.*.tsv",
  full.names = TRUE
)

cat("... profile \n")
filesList <- lapply(files, read_tsv)

profiles <-
  data.table::rbindlist(l = filesList, fill = TRUE, idcol = FALSE)

n_panelists <- profiles |>
  dplyr::distinct(ProductName, J) |>
  dplyr::group_by(ProductName) |>
  dplyr::count()

presence <- profiles |>
  dplyr::distinct(ProductName, CJ) |>
  dplyr::group_by(CJ) |>
  dplyr::count(sort = TRUE)

consistent_descriptors <- profiles %>%
  dplyr::distinct(J, taste = newName) %>%
  dplyr::group_by(taste) %>%
  dplyr::count() %>%
  dplyr::filter(n >= MIN_PANELISTS) %>%
  dplyr::filter(!is.na(taste))

nice_colors <- rev(
  list(
    microshades_palette("micro_cvd_green", lightest = FALSE),
    microshades_palette("micro_cvd_orange", lightest = FALSE),
    microshades_palette("micro_cvd_blue", lightest = FALSE),
    microshades_palette("micro_cvd_turquoise", lightest = FALSE),
    microshades_palette("micro_cvd_purple", lightest = FALSE),
    microshades_palette("micro_cvd_gray", lightest = FALSE),
    microshades_palette("micro_brown", lightest = FALSE),
    microshades_palette("micro_purple", lightest = FALSE),
    microshades_palette("micro_orange", lightest = FALSE)
  )
)

profiles_consistent <- profiles |>
  dplyr::left_join(n_panelists) |>
  dplyr::select(ProductName, J, descriptors, name, value, newName, n) |>
  dplyr::rename(taste = newName) |>
  dplyr::filter(taste %in% consistent_descriptors$taste) |>
  # dplyr::filter(value > 0) |>
  dplyr::group_by(ProductName, taste) |>
  ## problem due to FIZZ export
  dplyr::mutate(median = median(value[value <= 10])) |>
  dplyr::ungroup() |>
  dplyr::mutate(value = if_else(
    condition = value > 10,
    true = median,
    false = value
  )) |>
  dplyr::mutate(value = if_else(
    condition = ProductName > 31 &
      ProductName < 40,
    true = 50 * value,
    false = value
  )) |>
  dplyr::mutate(value = if_else(
    condition = ProductName > 63 &
      ProductName < 72,
    true = 0.5 * value,
    false = value
  )) |>
  dplyr::group_by(ProductName, taste) |>
  dplyr::mutate(median = median(value)) |>
  ## problem due to FIZZ export
  # dplyr::mutate(
  #   value = dplyr::if_else(
  #     condition = as.numeric(ProductName) >= min_diluted_fraction &
  #       as.numeric(ProductName) <= max_diluted_fraction,
  #     true = dilution_factor * value / n,
  #     false = value / n
  #   )
  # ) |>
  ## diluted
  dplyr::select(-descriptors, -name, -n) |>
  dplyr::group_by(taste) |>
  dplyr::mutate(sum_taste = sum(value)) |>
  dplyr::group_by(desc(sum_taste)) |>
  dplyr::mutate(group = dplyr::cur_group_id()) |>
  dplyr::group_by(ProductName) |>
  dplyr::mutate(sum_name = sum(value)) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    color = paired[[group]],
    relative = value / sum_name
  )

profiles_consistent$taste <-
  forcats::fct_reorder(
    .f = profiles_consistent$taste,
    .x = profiles_consistent$sum_taste,
    .desc = FALSE
  )

profiles_consistent$color <-
  forcats::fct_reorder(
    .f = profiles_consistent$color,
    .x = profiles_consistent$sum_taste,
    .desc = FALSE
  )

test_basis <- ggplot2::ggplot(
  profiles_consistent,
  ggplot2::aes(
    x = ProductName,
    y = value,
    fill = taste,
    color = taste,
    group = ProductName
  )
) +
  ggplot2::geom_violin(ggplot2::aes(fill = taste)) +
  ggplot2::scale_fill_manual(
    values = levels(profiles_consistent$color) |>
      as.character(),
    guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
  ) +
  ggplot2::scale_color_manual(
    values = levels(profiles_consistent$color) |>
      as.character(),
    guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
  ) +
  ggplot2::geom_jitter(
    position = ggplot2::position_jitter(width = .05),
    alpha = 0.5
  ) +
  ggplot2::labs(fill = "taste") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
  ) +
  ggplot2::xlab("sample") +
  ggplot2::ylab("absolute")

test_zoom <- test_basis +
  ggplot2::ylim(0, 10) +
  ggplot2::facet_wrap(facets = ~taste)
test_zoom

test_zoom2 <- test_basis +
  ggplot2::ylim(0, 500) +
  ggplot2::facet_wrap(facets = ~taste)
test_zoom2

test_zoom <- test_basis +
  ggplot2::ylim(0, 10) +
  ggplot2::facet_wrap(facets = ~taste)
test_zoom

test_2 <- ggplot2::ggplot(
  profiles_consistent |>
    dplyr::distinct(ProductName, median, taste, color),
  ggplot2::aes(
    x = ProductName,
    y = median,
    fill = taste,
    color = taste,
  )
) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(
    values = levels(profiles_consistent$color) |>
      as.character(),
    guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
  ) +
  ggplot2::scale_color_manual(
    values = levels(profiles_consistent$color) |>
      as.character(),
    guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
  ) +
  ggplot2::labs(fill = "taste") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
  ) +
  ggplot2::xlab("sample") +
  ggplot2::ylab("Median score") +
  ggbreak::scale_y_cut(breaks = c(35, 760), which = c(1, 3), scales = c(0, 1))

test_2

test_3 <- ggplot2::ggplot(
  profiles_consistent |>
    dplyr::distinct(ProductName, median, taste, color) |>
    dplyr::filter(taste == "MOUTHFILLING" |
      taste == "VOLUME" |
      # taste == "FATTY" |
      taste == "ASTRINGENT"),
  ggplot2::aes(
    x = ProductName,
    y = median,
    fill = taste,
    color = taste,
  )
) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(
    values = levels(profiles_consistent$color) |>
      as.character(),
    guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
  ) +
  ggplot2::scale_color_manual(
    values = levels(profiles_consistent$color) |>
      as.character(),
    guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
  ) +
  ggplot2::labs(fill = "taste") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
  ) +
  ggplot2::xlab("sample") +
  ggplot2::ylab("Median score") +
  ggbreak::scale_y_cut(breaks = c(20, 245), which = c(1, 3), scales = c(0, 1))

test_3

test_4 <- ggplot2::ggplot(
  profiles_consistent |>
    dplyr::distinct(ProductName, median, taste, color) |>
    dplyr::filter(taste != "BITTER") |>
    dplyr::filter(taste != "MOUTHFILLING"),
  ggplot2::aes(
    x = ProductName,
    y = median,
    fill = taste,
    color = taste,
  )
) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(
    values = levels(profiles_consistent$color) |>
      as.character(),
    guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
  ) +
  ggplot2::scale_color_manual(
    values = levels(profiles_consistent$color) |>
      as.character(),
    guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
  ) +
  ggplot2::labs(fill = "taste") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
  ) +
  ggplot2::xlab("sample") +
  ggplot2::ylab("Median score") +
  ggbreak::scale_y_cut(breaks = c(30, 360), which = c(1, 3), scales = c(0, 1))

test_4

test_5 <- ggplot2::ggplot(
  profiles_consistent |>
    dplyr::distinct(ProductName, median, taste, color) |>
    # dplyr::filter(taste == "ASTRINGENT"),
    # dplyr::filter(taste == "VOLUME"),
    # dplyr::filter(taste == "SWEET"),
    dplyr::filter(taste == "SALTY"),
  # dplyr::filter(taste == "WOODY"),
  # dplyr::filter(taste == "UMAMI"),
  ggplot2::aes(
    x = ProductName,
    y = median,
    fill = taste,
    color = taste,
  )
) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(
    values = levels(profiles_consistent$color) |>
      as.character(),
    guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
  ) +
  ggplot2::scale_color_manual(
    values = levels(profiles_consistent$color) |>
      as.character(),
    guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
  ) +
  ggplot2::labs(fill = "taste") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
  ) +
  ggplot2::xlab("sample") +
  ggplot2::ylab("Median score")

test_5
