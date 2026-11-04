#' Plot concentration-response and Ascending Forced Choice (AFC) results
#'
#' Visualize perceived taste intensity, AFC psychometric detection curve, and
#' raw/weighted descriptor counts across tested extract concentrations.
#'
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:topic_0092 Data visualisation
#' @concept EDAM:operation_0337 Visualisation
#' @param input Optional path or `data.frame` of concentration AFC data
#'   (defaults to built-in [concentration_afc]).
#' @param output Output PDF file path.
#'
#' @return Output file path (`output`).
#' @export
#' @examples
#' \donttest{
#' out <- tempfile(fileext = ".pdf")
#' plot_concentration(output = out)
#' }
plot_concentration <- function(
  input = NULL,
  output = "./data/figures/figure_raw_extract.pdf"
) {
  message("Loading file...\n")
  prepared <- load_dataset_or_file(input, "concentration_afc")

  prepared$correct_percent <- prepared$afc_correct / prepared$afc_total
  ct_key <- paste(prepared$concentration, prepared$taste, sep = "\r")
  prepared$n <- stats::ave(prepared$value, ct_key, FUN = length)
  prepared$value <- stats::ave(prepared$value, ct_key, FUN = mean)
  prepared$m <- prepared$n * prepared$value

  dedup_key <- paste(
    prepared$concentration,
    prepared$jury,
    prepared$value,
    prepared$n,
    prepared$m,
    sep = "\r"
  )
  counted <- prepared[
    !duplicated(dedup_key) & !is.na(prepared$value),
    ,
    drop = FALSE
  ]
  counted <- unique(counted[order(counted$m, counted$n), , drop = FALSE])

  message("visualizing ... \n")
  message("... intensity and p-value per concentration \n")
  boxes <- counted |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = as.numeric(concentration),
        y = value,
        color = as.numeric(concentration),
        group = as.numeric(concentration)
      )
    ) +
    ggplot2::geom_violin() +
    ggplot2::scale_x_log10() +
    ggplot2::geom_jitter(
      position = ggplot2::position_jitter(width = 0.05),
      alpha = 0.5
    ) +
    ggplot2::scale_color_gradient2(
      low = "#f7fcf5",
      mid = "#74c476",
      high = "#00441b"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::xlab(label = "Concentration [mg/L]") +
    ggplot2::ylab(label = "Intensity") +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(face = "bold")
    )
  boxes

  scurve <- counted |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = as.numeric(concentration),
        y = correct_percent,
        color = as.numeric(concentration)
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_log10() +
    geom_sigmoid(
      data = counted,
      mapping = ggplot2::aes(
        x = min(as.numeric(concentration)),
        xend = max(as.numeric(concentration)),
        y = min(correct_percent),
        yend = max(correct_percent)
      )
    ) +
    ggplot2::scale_color_gradient2(
      low = "#f7fcf5",
      mid = "#74c476",
      high = "#00441b"
    ) +
    ggplot2::xlab(label = "Concentration [mg/L]") +
    ggplot2::ylab("Correct answers [%]") +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      color = "#19597e"
    ) +
    ggplot2::geom_vline(
      xintercept = 0.12,
      linetype = "dashed",
      color = "#19597e"
    ) +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(face = "bold")
    )
  scurve

  message("... terms per concentration \n")
  dots <- counted |>
    ggplot2::ggplot() +
    ggplot2::geom_segment(
      mapping = ggplot2::aes(
        x = taste,
        xend = taste,
        y = 0,
        yend = n
      ),
      color = "#19597e"
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = taste,
        color = as.numeric(concentration),
        y = n
      ),
      size = 3
    ) +
    ggplot2::scale_color_gradient2(
      low = "#f7fcf5",
      mid = "#74c476",
      high = "#00441b"
    ) +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab("Count") +
    ggplot2::facet_wrap(
      facets = ~ paste(
        concentration |>
          round(digits = 2) |>
          format(nsmall = 2),
        "[mg/L]"
      ),
      ncol = 1,
      scales = "free_y"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(face = "bold"),
      panel.spacing = ggplot2::unit(0.2, "lines"),
      strip.text.x = ggplot2::element_text(size = 10),
      strip.background = ggplot2::element_rect(fill = "white")
    )
  dots

  message("... terms multiplied by mean intensity per concentration \n")
  dots_corrected <- counted |>
    ggplot2::ggplot() +
    ggplot2::geom_segment(
      mapping = ggplot2::aes(
        x = taste,
        xend = taste,
        y = 0,
        yend = m
      ),
      color = "#19597e"
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = taste,
        color = as.numeric(concentration),
        y = m
      ),
      size = 3
    ) +
    ggplot2::scale_color_gradient2(
      low = "#f7fcf5",
      mid = "#74c476",
      high = "#00441b"
    ) +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab("Value") +
    ggplot2::facet_wrap(
      facets = ~ paste(
        concentration |>
          round(digits = 2) |>
          format(nsmall = 2),
        "[mg/L]"
      ),
      ncol = 1,
      scales = "free_y"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(face = "bold"),
      panel.spacing = ggplot2::unit(0.2, "lines"),
      strip.text.x = ggplot2::element_text(size = 10),
      strip.background = ggplot2::element_rect(fill = "white")
    )
  dots_corrected

  .cascade_check_export_dir(dirname(output))
  ggpubr::ggarrange(
    ggpubr::ggarrange(boxes, scurve, ncol = 2, labels = "AUTO"),
    dots_corrected,
    nrow = 2,
    labels = c("", "C"),
    heights = c(1, 2)
  ) |>
    ggplot2::ggsave(
      filename = output,
      width = 9,
      height = 12
    )

  return(output)
}
