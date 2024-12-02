start <- Sys.time()

pkgload::load_all()

message("This program TODO.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

message("Loading ... \n")
message("... packages (and installing them if needed) \n")
if (!require(tidyverse)) {
  install.packages("tidyverse")
}

message("... paths and parameters \n")
source(file = "paths.R")
source(file = "params.R")
source(file = "r/colors.R")

message("... functions \n")

message("... files ... \n")
files <- analysis_path_04_output |>
  list.files(pattern = "^profile.*.tsv", full.names = TRUE)

message("... profile \n")
filesList <- files |>
  furrr::future_map(.f = tidytable::fread)

profiles <- filesList |>
  tidytable::bind_rows()

n_panelists <- profiles |>
  tidytable::distinct(ProductName, J) |>
  tidytable::group_by(ProductName) |>
  tidytable::count()

presence <- profiles |>
  tidytable::distinct(ProductName, CJ) |>
  tidytable::group_by(CJ) |>
  tidytable::count(sort = TRUE)

consistent_descriptors <- profiles |>
  tidytable::distinct(J, taste = newName) |>
  tidytable::group_by(taste) |>
  tidytable::count() |>
  tidytable::filter(n >= MIN_PANELISTS) |>
  tidytable::filter(!is.na(taste))

profiles_consistent <- profiles |>
  tidytable::left_join(n_panelists) |>
  tidytable::select(ProductName, J, descriptors, name, value, newName, n) |>
  tidytable::rename(taste = newName) |>
  tidytable::filter(taste %in% consistent_descriptors$taste) |>
  # tidytable::filter(value > 0) |>
  tidytable::group_by(ProductName, taste) |>
  ## problem due to FIZZ export
  tidytable::mutate(median = median(value[value <= 10])) |>
  tidytable::ungroup() |>
  tidytable::mutate(value = tidytable::if_else(
    condition = value > 10,
    true = median,
    false = value
  )) |>
  tidytable::mutate(
    value = tidytable::if_else(
      condition = ProductName > 31 &
        ProductName < 40,
      true = 50 * value,
      false = value
    )
  ) |>
  tidytable::mutate(
    value = tidytable::if_else(
      condition = ProductName > 63 &
        ProductName < 72,
      true = 0.5 * value,
      false = value
    )
  ) |>
  tidytable::group_by(ProductName, taste) |>
  tidytable::mutate(median = value |>
    median()) |>
  ## problem due to FIZZ export
  # tidytable::mutate(
  #   value = tidytable::if_else(
  #     condition = as.numeric(ProductName) >= min_diluted_fraction &
  #       as.numeric(ProductName) <= max_diluted_fraction,
  #     true = dilution_factor * value / n,
  #     false = value / n
  #   )
  # ) |>
  ## diluted
  tidytable::select(-descriptors, -name, -n) |>
  tidytable::group_by(taste) |>
  tidytable::mutate(sum_taste = value |>
    sum()) |>
  tidytable::arrange(tidytable::desc(sum_taste)) |>
  tidytable::group_by(sum_taste) |>
  tidytable::mutate(group = tidytable::cur_group_id()) |>
  tidytable::group_by(ProductName) |>
  tidytable::mutate(sum_name = value |>
    sum()) |>
  tidytable::ungroup() |>
  tidytable::mutate_rowwise(color = paired[[group]], relative = value / sum_name) |>
  tidytable::ungroup()

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
    tidytable::distinct(ProductName, median, taste, color),
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
  ggbreak::scale_y_cut(
    breaks = c(35, 760),
    which = c(1, 3),
    scales = c(0, 1)
  )

test_2

test_3 <- ggplot2::ggplot(
  profiles_consistent |>
    tidytable::distinct(ProductName, median, taste, color) |>
    tidytable::filter(taste == "MOUTHFILLING" |
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
  ggbreak::scale_y_cut(
    breaks = c(20, 245),
    which = c(1, 3),
    scales = c(0, 1)
  )

test_3

test_4 <- ggplot2::ggplot(
  profiles_consistent |>
    tidytable::distinct(ProductName, median, taste, color) |>
    tidytable::filter(taste != "BITTER") |>
    tidytable::filter(taste != "MOUTHFILLING"),
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
  ggbreak::scale_y_cut(
    breaks = c(30, 360),
    which = c(1, 3),
    scales = c(0, 1)
  )

test_4

test_5 <- ggplot2::ggplot(
  profiles_consistent |>
    tidytable::distinct(ProductName, median, taste, color) |>
    # tidytable::filter(taste == "ASTRINGENT"),
    # tidytable::filter(taste == "VOLUME"),
    # tidytable::filter(taste == "SWEET"),
    tidytable::filter(taste == "SALTY"),
  # tidytable::filter(taste == "WOODY"),
  # tidytable::filter(taste == "UMAMI"),
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

end <- Sys.time()

message("Script finished in ", format(end - start))
