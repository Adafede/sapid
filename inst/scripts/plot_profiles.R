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

input <- system.file("extdata", "profiles.tsv", package = "sapid")

profiles_consistent <- input |>
  load_consistent_profiles()

profiles_consistent <- profiles_consistent |>
  tidytable::mutate(fraction = fraction |>
    gsub(
      pattern = "fraction_",
      replacement = "",
      fixed = TRUE
    ) |>
    as.character()) |>
  tidytable::group_by(fraction) |>
  tidytable::mutate(sum_name = value |>
    sum()) |>
  tidytable::ungroup() |>
  tidytable::mutate_rowwise(color = discrete_rainbow_14[[group]], relative = value / sum_name) |>
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

test_2 <- ggplot2::ggplot(
  profiles_consistent |>
    tidytable::distinct(fraction, median, taste, color),
  ggplot2::aes(
    x = fraction,
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
  ggplot2::labs(fill = "Taste", color = "Taste") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
  ) +
  ggplot2::xlab("Fraction") +
  # ggbreak::scale_y_cut(
  #   breaks = c(10, 30),
  #   which = c(1, 3),
  #   scales = c(0, 1)
  # ) +
  ggplot2::ylab("Taste intensity") +
  ggplot2::theme(
    axis.text = ggplot2::element_text(color = "grey30"),
    axis.title = ggplot2::element_text(color = "grey30"),
    legend.text = ggplot2::element_text(color = "grey30"),
    text = ggplot2::element_text(
      face = "bold",
      color = "grey30",
      size = 20
    )
  )
test_2

test_3 <- test_2 +
  ggplot2::facet_wrap(~taste)

test_3

end <- Sys.time()

message("Script finished in ", format(end - start))
