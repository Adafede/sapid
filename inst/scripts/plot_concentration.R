start <- Sys.time()

pkgload::load_all()

message("This program plots concentration evaluation.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Plot concentration
#'
#' @param input Input
#' @param output Output
#'
#' @return NULL
#'
#' @examples NULL
plot_concentration <- function(
  input = system.file("extdata", "concentration_afc.tsv", package = "sapid"),
  output = "./data/figures/figure_raw_extract.pdf"
) {
  message("Loading file...\n")
  prepared <- input |>
    tidytable::fread()

  counted <- prepared |>
    tidytable::mutate(correct_percent = afc_correct / afc_total) |>
    tidytable::group_by(concentration, taste) |>
    tidytable::add_count() |>
    tidytable::mutate(
      value = value |>
        mean()
    ) |>
    tidytable::mutate(m = n * value) |>
    tidytable::distinct(concentration, jury, value, n, m, .keep_all = TRUE) |>
    tidytable::filter(!is.na(value)) |>
    tidytable::group_by(jury, concentration) |>
    tidytable::arrange(m, n) |>
    tidytable::distinct() |>
    tidytable::ungroup()

  message("visualizing ... \n")
  message("... intensity and p-value per concentration \n")
  boxes <- counted |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = concentration |> as.numeric(),
        y = value,
        color = concentration |> as.numeric(),
        group = concentration |> as.numeric()
      )
    ) +
    ggplot2::geom_violin() +
    ggplot2::scale_x_log10() +
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
      text = ggplot2::element_text(face = "bold")
    )
  boxes

  scurve <- counted |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = concentration |> as.numeric(),
        y = correct_percent,
        color = concentration |> as.numeric()
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_log10() +
    geom_sigmoid(
      data = counted,
      mapping = ggplot2::aes(
        x = min(concentration |> as.numeric()),
        xend = max(concentration |> as.numeric()),
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
    ggplot2::theme_bw() +
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
        y = n,
        color = concentration |> as.numeric()
      ),
      size = 3
    ) +
    ggplot2::scale_color_gradient2(
      low = "#f7fcf5",
      mid = "#74c476",
      high = "#00441b"
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      panel.spacing = ggplot2::unit(0.1, "lines"),
      strip.text.x = ggplot2::element_text(size = 8)
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("Count") +
    ggplot2::facet_wrap(
      facets = ~concentration,
      ncol = 1,
      scales = "free_y"
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
        color = concentration |> as.numeric(),
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
        format(
          round(x = concentration |> as.numeric(), digits = 2),
          nsmall = 2
        ),
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

  cascade:::check_export_dir(output)
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

plot_concentration()

end <- Sys.time()

message("Script finished in ", format(end - start))
