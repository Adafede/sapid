start <- Sys.time()

pkgload::load_all()

message("This program plots chasselas taste variation.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Plot chasselas variation
#'
#' @param input Input
#' @param output_jury Output jury
#' @param output_session Output session
#'
#' @return NULL
#'
#' @examples NULL
plot_chasselas_variation <- function(input = system.file("extdata", "chasselas.tsv", package = "sapid"),
                                     output_jury = "~/switchdrive/SAPERE/06_figures/figure_variation_jury.pdf",
                                     output_session = "~/switchdrive/SAPERE/06_figures/figure_variation_session.pdf") {
  deltas <- input |>
    tidytable::fread() |>
    tidytable::mutate(tidytable::across(tidytable::everything(), function(x) {
      x |>
        gsub(pattern = "_", replacement = " ")
    })) |>
    tidytable::mutate(value = value |>
      as.numeric())

  deltas_before <- deltas |>
    tidytable::filter(grepl(pattern = "before", x = product))

  # deltas_after <- deltas |>
  #   tidytable::filter(grepl(pattern = "after", x = product))

  deltas_1 <- deltas_before |>
    tidytable::filter(taste %in% c("sourness", "bitterness", "sweetness", "saltiness"))

  deltas_2 <- deltas_before |>
    tidytable::filter(
      taste %in% c(
        "fatness, volume",
        "balance",
        "freshness",
        "persistency",
        "mouthwatering"
      )
    )

  deltas_3 <- deltas_1 |>
    tidytable::bind_rows(deltas_2) |>
    tidytable::filter(!is.na(value)) |>
    tidytable::group_by(jury, taste) |>
    tidytable::add_count() |>
    tidytable::filter(n >= 3) |>
    tidytable::ungroup()

  p_1 <- deltas_3 |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = jury, y = value, colour = jury)) +
    ggplot2::scale_x_discrete() +
    ggplot2::scale_color_manual(values = discrete_rainbow_14) +
    ggplot2::geom_violin() +
    ggplot2::geom_jitter(
      position = ggplot2::position_jitter(width = .05),
      alpha = 0.5
    ) +
    ggplot2::facet_wrap(facets = ~ as.character(taste), ncol = 3) +
    ggplot2::theme_minimal() +
    ggplot2::ylab(label = "Intensity") +
    ggplot2::ylim(c(0, 10)) +
    ggplot2::labs(colour = "Jury") +
    ggplot2::theme(
      legend.position = "right",
      title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  p_1

  p_2 <- deltas_3 |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = taste, y = value, colour = taste)) +
    ggplot2::scale_x_discrete() +
    ggplot2::scale_color_brewer(palette = "Paired") +
    ggplot2::geom_violin() +
    ggplot2::geom_jitter(
      position = ggplot2::position_jitter(width = .05),
      alpha = 0.5
    ) +
    ggplot2::facet_wrap(facets = ~ as.character(jury), ncol = 3) +
    ggplot2::theme_minimal() +
    ggplot2::ylab(label = "Intensity") +
    ggplot2::ylim(c(0, 10)) +
    ggplot2::labs(colour = "Taste") +
    ggplot2::theme(
      legend.position = "right",
      title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  p_2

  p_3 <- deltas_3 |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = session, y = value, colour = session)) +
    ggplot2::scale_x_discrete() +
    ggplot2::scale_color_manual(values = discrete_rainbow_14[rep(c(FALSE, TRUE), length = length(discrete_rainbow_14))]) +
    ggplot2::geom_violin() +
    ggplot2::geom_jitter(
      position = ggplot2::position_jitter(width = .05),
      alpha = 0.5
    ) +
    ggplot2::facet_wrap(facets = ~ as.character(taste), ncol = 3) +
    ggplot2::theme_minimal() +
    ggplot2::ylab(label = "Intensity") +
    ggplot2::ylim(c(0, 10)) +
    ggplot2::labs(colour = "Session") +
    ggplot2::theme(
      legend.position = "right",
      title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  p_3

  p_4 <- deltas_3 |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = taste, y = value, colour = taste)) +
    ggplot2::scale_x_discrete() +
    ggplot2::scale_color_brewer(palette = "Paired") +
    ggplot2::geom_violin() +
    ggplot2::geom_jitter(
      position = ggplot2::position_jitter(width = .05),
      alpha = 0.5
    ) +
    ggplot2::facet_wrap(facets = ~ as.character(session), ncol = 3) +
    ggplot2::theme_minimal() +
    ggplot2::ylab(label = "Intensity") +
    ggplot2::ylim(c(0, 10)) +
    ggplot2::labs(colour = "Taste") +
    ggplot2::theme(
      legend.position = "right",
      title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  p_4

  ggpubr::ggarrange(p_1,
    p_2,
    nrow = 2,
    labels = "AUTO",
    align = "hv"
  ) |>
    ggplot2::ggsave(
      filename = output_jury,
      width = 9,
      height = 12
    )

  ggpubr::ggarrange(p_3,
    p_4,
    nrow = 2,
    labels = "AUTO",
    align = "hv"
  ) |>
    ggplot2::ggsave(
      filename = output_session,
      width = 9,
      height = 12
    )

  return(list(jury = output_jury, session = output_session))
}

plot_chasselas_variation()

end <- Sys.time()

message("Script finished in ", format(end - start))
