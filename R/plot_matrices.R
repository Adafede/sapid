#' Plot sensory matrices before and after vocabulary harmonization
#'
#' Render side-by-side heatmaps of fraction-by-juror bitterness ratings before
#' (`"AMER"`) and after (`"BITTER"`) descriptor vocabulary harmonization.
#'
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:topic_0092 Data visualisation
#' @concept EDAM:operation_0337 Visualisation
#' @param input Optional path or `data.frame` of sensory profiles (defaults to
#'   built-in [profiles]).
#' @param output Output PDF file path.
#'
#' @return Output file path (`output`).
#' @export
#' @examples
#' \donttest{
#' out <- tempfile(fileext = ".pdf")
#' plot_matrices(output = out)
#' }
plot_matrices <- function(
  input = NULL,
  output = "./data/figures/figure_matrices.pdf"
) {
  input_data <- load_dataset_or_file(input, "profiles")

  # tidytable::mutate(value = tidytable::if_else(
  #   condition = session == "session_03",
  #   true = value / 500,
  #   false = value
  # )) |>
  # tidytable::mutate(value = tidytable::if_else(
  #   condition = session == "session_03",
  #   true = value / 500,
  #   false = value
  # )) |>
  table_profiles <- unique(input_data)

  extract_matrix_df <- function(col_name, target_val) {
    sub_df <- table_profiles[
      !is.na(table_profiles[[col_name]]) &
        table_profiles[[col_name]] == target_val,
      ,
      drop = FALSE
    ]
    unique(data.frame(
      fraction = as.numeric(
        gsub("fraction_", "", sub_df$fraction, fixed = TRUE)
      ),
      jury = as.numeric(gsub("jury_", "", sub_df$jury, fixed = TRUE)),
      value = as.numeric(sub_df$value),
      stringsAsFactors = FALSE
    ))
  }

  tables_profiles_original <- extract_matrix_df("taste_original", "AMER")
  tables_profiles_curated <- extract_matrix_df("taste_harmonized", "BITTER")

  matrix_original <- tables_profiles_original |>
    ggplot2::ggplot(mapping = ggplot2::aes(fraction, jury, fill = value)) +
    ggplot2::geom_tile() +
    scale_fill_batlow() +
    ggplot2::labs(fill = "Intensity") +
    ggplot2::xlab("Fraction") +
    ggplot2::ylab("Jury") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "#767676"),
      axis.title = ggplot2::element_text(color = "#767676"),
      legend.text = ggplot2::element_text(color = "#767676"),
      text = ggplot2::element_text(
        face = "bold",
        color = "#767676",
        size = 20
      )
    )

  matrix_curated <- tables_profiles_curated |>
    ggplot2::ggplot(mapping = ggplot2::aes(fraction, jury, fill = value)) +
    ggplot2::geom_tile() +
    scale_fill_batlow() +
    ggplot2::labs(fill = "Intensity") +
    ggplot2::xlab("Fraction") +
    ggplot2::ylab("Jury") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "#767676"),
      axis.title = ggplot2::element_text(color = "#767676"),
      legend.text = ggplot2::element_text(color = "#767676"),
      text = ggplot2::element_text(
        face = "bold",
        color = "#767676",
        size = 20
      )
    )

  .cascade_check_export_dir(dirname(output))
  ggpubr::ggarrange(
    matrix_original,
    matrix_curated,
    labels = "AUTO",
    nrow = 2,
    common.legend = TRUE,
    legend = "right"
  ) |>
    ggplot2::ggsave(
      filename = output,
      width = 9,
      height = 12
    )

  return(output)
}
