#' Plot Chasselas baseline sensory variation across jurors and sessions
#'
#' Generate violin/jitter distribution plots of baseline (`before`) Chasselas
#' ratings stratified by juror and by session.
#'
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:topic_0092 Data visualisation
#' @concept EDAM:operation_0337 Visualisation
#' @param input Optional path or `data.frame` of Chasselas ratings (defaults to
#'   built-in [chasselas]).
#' @param output_jury Output PDF file path for juror variation plots.
#' @param output_session Output PDF file path for session variation plots.
#'
#' @return Named list with elements `jury` and `session`.
#' @export
#' @examples
#' \donttest{
#' out_j <- tempfile(fileext = ".pdf")
#' out_s <- tempfile(fileext = ".pdf")
#' plot_chasselas_variation(output_jury = out_j, output_session = out_s)
#' }
plot_chasselas_variation <- function(
  input = NULL,
  output_jury = "./data/figures/figure_variation_jury.pdf",
  output_session = "./data/figures/figure_variation_session.pdf"
) {
  input_data <- load_dataset_or_file(input, "chasselas")

  deltas <- as.data.frame(
    lapply(input_data, function(col_v) {
      gsub("_", " ", as.character(col_v), fixed = TRUE)
    }),
    stringsAsFactors = FALSE
  )
  deltas$value <- as.numeric(deltas$value)

  deltas_before <- deltas[
    grepl(pattern = "before", x = deltas$product, fixed = TRUE),
    ,
    drop = FALSE
  ]

  deltas_3 <- deltas_before[
    deltas_before$taste %in% chasselas_tastes & !is.na(deltas_before$value),
    ,
    drop = FALSE
  ]

  jt_key <- paste(deltas_3$jury, deltas_3$taste, sep = "\r")
  deltas_3$n <- stats::ave(deltas_3$value, jt_key, FUN = length)
  deltas_3 <- deltas_3[deltas_3$n >= 3L, , drop = FALSE]

  build_variation_panel <- function(
    mapping,
    facet_formula,
    colour_label,
    color_scale
  ) {
    deltas_3 |>
      ggplot2::ggplot(mapping = mapping) +
      ggplot2::scale_x_discrete() +
      color_scale +
      ggplot2::geom_violin() +
      ggplot2::geom_jitter(
        position = ggplot2::position_jitter(width = 0.05),
        alpha = 0.5
      ) +
      ggplot2::facet_wrap(facets = facet_formula, ncol = 3) +
      ggplot2::theme_minimal() +
      ggplot2::ylab(label = "Intensity") +
      ggplot2::ylim(c(0, 10)) +
      ggplot2::labs(colour = colour_label) +
      ggplot2::theme(
        legend.position = "right",
        title = ggplot2::element_text(face = "bold"),
        strip.text = ggplot2::element_text(face = "bold"),
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
  }

  p_1 <- build_variation_panel(
    mapping = ggplot2::aes(x = jury, y = value, colour = jury),
    facet_formula = ~ as.character(taste),
    colour_label = "Jury",
    color_scale = ggplot2::scale_color_manual(values = discrete_rainbow_14)
  )
  p_2 <- build_variation_panel(
    mapping = ggplot2::aes(x = taste, y = value, colour = taste),
    facet_formula = ~ as.character(jury),
    colour_label = "Taste",
    color_scale = ggplot2::scale_color_brewer(palette = "Paired")
  )
  p_3 <- build_variation_panel(
    mapping = ggplot2::aes(x = session, y = value, colour = session),
    facet_formula = ~ as.character(taste),
    colour_label = "Session",
    color_scale = ggplot2::scale_color_manual(values = discrete_rainbow_7)
  )
  p_4 <- build_variation_panel(
    mapping = ggplot2::aes(x = taste, y = value, colour = taste),
    facet_formula = ~ as.character(session),
    colour_label = "Taste",
    color_scale = ggplot2::scale_color_brewer(palette = "Paired")
  )

  save_variation_pair <- function(p_a, p_b, out_path) {
    .cascade_check_export_dir(dirname(out_path))
    ggpubr::ggarrange(p_a, p_b, nrow = 2, labels = "AUTO", align = "hv") |>
      ggplot2::ggsave(filename = out_path, width = 9, height = 12)
  }
  save_variation_pair(p_1, p_2, output_jury)
  save_variation_pair(p_3, p_4, output_session)

  list(jury = output_jury, session = output_session)
}
