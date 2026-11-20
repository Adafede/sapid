#' Plot Chasselas sensory taste modulation
#'
#' Compute before/after rating differences (`delta`) for Chasselas sensory
#' sessions, evaluate two-sided paired tests around median zero using Pratt's
#' zero-preserving signed-rank test (or exact binomial sign test), and save a
#' 2-panel violin/jitter summary figure.
#'
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:topic_0092 Data visualisation
#' @concept EDAM:operation_0337 Visualisation
#' @param input Optional path or `data.frame` of Chasselas ratings (defaults to
#'   built-in [chasselas]).
#' @param output Output PDF file path.
#' @param test_method Paired test method: `"pratt"` (default, Pratt's
#'   zero-preserving Wilcoxon signed-rank test) or `"sign"` (binomial sign test).
#'
#' @return Output file path (`output`).
#' @export
#' @examples
#' \donttest{
#' out <- tempfile(fileext = ".pdf")
#' plot_chasselas_modulation(output = out)
#' }
plot_chasselas_modulation <- function(
  input = NULL,
  output = "./data/figures/figure_modulation.pdf",
  test_method = c("pratt", "sign")
) {
  test_method <- match.arg(test_method)
  input_data <- load_dataset_or_file(input, "chasselas")

  wide <- stats::reshape(
    input_data,
    idvar = c("date", "jury", "session", "taste"),
    timevar = "product",
    direction = "wide"
  )
  wide$delta <- wide$value.product_2after - wide$value.product_1before

  deltas_3 <- wide[wide$taste %in% chasselas_tastes, , drop = FALSE]

  date_map <- c(
    "2021-06-07" = "Fractions 17-21",
    "2021-04-19" = "Fractions 22-31",
    "2021-05-17" = "Fractions 32-39",
    "2021-05-31" = "Fractions 40-45",
    "2021-04-26" = "Fractions 46-54",
    "2021-04-12" = "Fractions 55-63",
    "2021-05-10" = "Fractions 64-71"
  )
  date_keys <- substr(as.character(deltas_3$date), 1L, 10L)
  deltas_3$group <- unname(date_map[date_keys])

  grp_key <- paste(deltas_3$taste, deltas_3$group, sep = "\r")
  is_pratt <- test_method == "pratt"
  test_fn <- if (is_pratt) pratt_wilcox_pvalue else sign_test_pvalue
  caption_label <- sprintf(
    "* = p-value < 0.05 (%s)",
    if (is_pratt) "Pratt signed-rank test" else "Sign test"
  )
  p_vals <- stats::ave(deltas_3$delta, grp_key, FUN = test_fn)

  deltas_3$stars <- ifelse(
    p_vals < 0.001,
    "***",
    ifelse(p_vals < 0.01, "**", ifelse(p_vals < 0.05, "*", ""))
  )

  build_modulation_panel <- function(
    mapping,
    text_mapping,
    facet_formula,
    colour_label,
    color_scale
  ) {
    deltas_3 |>
      ggplot2::ggplot(mapping = mapping) +
      color_scale +
      ggplot2::geom_violin() +
      ggplot2::geom_jitter(
        position = ggplot2::position_jitter(width = 0.05),
        alpha = 0.5
      ) +
      ggplot2::facet_wrap(facets = facet_formula, scales = "free") +
      ggplot2::theme_minimal() +
      ggplot2::ylab(label = "Intensity difference") +
      ggplot2::labs(colour = colour_label, caption = caption_label) +
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
      ggplot2::geom_text(
        text_mapping,
        color = "black",
        size = 8,
        vjust = 0
      )
  }

  p_1 <- build_modulation_panel(
    mapping = ggplot2::aes(x = group, y = delta, colour = group),
    text_mapping = ggplot2::aes(x = group, y = 5, label = stars),
    facet_formula = ~taste,
    colour_label = "Group",
    color_scale = ggplot2::scale_color_manual(values = discrete_rainbow_7)
  )
  p_2 <- build_modulation_panel(
    mapping = ggplot2::aes(x = taste, y = delta, colour = taste),
    text_mapping = ggplot2::aes(x = taste, y = 5, label = stars),
    facet_formula = ~group,
    colour_label = "Taste",
    color_scale = ggplot2::scale_color_brewer(palette = "Paired")
  )

  .cascade_check_export_dir(dirname(output))
  ggpubr::ggarrange(p_1, p_2, nrow = 2, labels = "AUTO", align = "hv") |>
    ggplot2::ggsave(
      filename = output,
      width = 9,
      height = 12
    )

  output
}
