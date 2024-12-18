start <- Sys.time()

pkgload::load_all()

message("This program plots chasselas taste modulation.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Plot chasselas modulation
#'
#' @param input Input
#' @param output Output
#'
#' @return NULL
#'
#' @examples NULL
plot_chasselas_modulation <- function(input = system.file("extdata", "chasselas.tsv", package = "sapid"),
                                      output = "~/switchdrive/SAPERE/06_figures/figure_modulation.pdf") {
  deltas <- input |>
    tidytable::fread() |>
    tidytable::pivot_wider(names_from = product, values_from = value) |>
    tidytable::mutate(delta = product_2after - product_1before)

  deltas_1 <- deltas |>
    tidytable::filter(taste %in% c("sourness", "bitterness", "sweetness", "saltiness"))

  deltas_2 <- deltas |>
    tidytable::filter(
      taste %in% c(
        "fatness, volume",
        "balance",
        "freshness",
        "persistency",
        "mouthwatering"
      )
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
    tidytable::mutate(group = switch(as.character(date),
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
    tidytable::group_by(taste, group) |>
    tidytable::summarize(p_value_wilcox = wilcox.test(delta, mu = 0)$p.value)

  p_values_sign <- deltas_3 |>
    tidytable::group_by(taste, group) |>
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
    tidytable::left_join(p_values_sign)

  p_1 <- deltas_4 |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = group, y = delta, colour = group)) +
    ggplot2::scale_color_manual(values = discrete_rainbow_14[rep(c(FALSE, TRUE), length = length(discrete_rainbow_14))]) +
    ggplot2::geom_violin() +
    ggplot2::geom_jitter(
      position = ggplot2::position_jitter(width = .05),
      alpha = 0.5
    ) +
    ggplot2::facet_wrap(facets = ~taste, scales = "free") +
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
      vjust = 0
    ) +
    ggplot2::labs(caption = "* = p-value < 0.05 (Sign test)")
  p_1

  p_2 <- deltas_4 |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = taste, y = delta, colour = taste)) +
    ggplot2::scale_color_brewer(palette = "Paired") +
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
      ggplot2::aes(x = taste, y = 5, label = stars),
      color = "black",
      size = 8,
      vjust = 0
    ) +
    ggplot2::labs(caption = "* = p-value < 0.05 (Sign test)")
  p_2

  ggpubr::ggarrange(p_1,
    p_2,
    nrow = 2,
    labels = "AUTO",
    align = "hv"
  ) |>
    ggplot2::ggsave(
      filename = output,
      width = 9,
      height = 12
    )

  return(output)
}

plot_chasselas_modulation()

end <- Sys.time()

message("Script finished in ", format(end - start))
