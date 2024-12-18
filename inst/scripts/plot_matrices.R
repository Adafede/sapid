start <- Sys.time()

pkgload::load_all()

message("This program plots matrices before and after vocabulary curation.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Plot matrices
#'
#' @param input Input
#' @param output Output
#'
#' @return NULL
#'
#' @examples NULL
#'
plot_matrices <- function(input = system.file("extdata", "profiles.tsv", package = "sapid"),
                          output = "./man/figures/figure_matrices.pdf") {
  table_profiles <- input |>
    tidytable::fread() |>
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
    tidytable::distinct()

  tables_profiles_original <- table_profiles |>
    tidytable::filter(taste_original == "AMER") |>
    tidytable::mutate(
      fraction = fraction |>
        gsub(
          pattern = "fraction_",
          replacement = "",
          fixed = TRUE
        ) |>
        as.numeric(),
      jury = jury |>
        gsub(
          pattern = "jury_",
          replacement = "",
          fixed = TRUE
        ) |>
        as.numeric()
    ) |>
    tidytable::distinct(fraction, jury, value)

  tables_profiles_curated <- table_profiles |>
    tidytable::filter(taste_harmonized == "BITTER") |>
    tidytable::mutate(
      fraction = fraction |>
        gsub(
          pattern = "fraction_",
          replacement = "",
          fixed = TRUE
        ) |>
        as.numeric(),
      jury = jury |>
        gsub(
          pattern = "jury_",
          replacement = "",
          fixed = TRUE
        ) |>
        as.numeric()
    ) |>
    tidytable::distinct(fraction, jury, value)

  matrix_original <- tables_profiles_original |>
    ggplot2::ggplot(mapping = ggplot2::aes(fraction, jury, fill = value)) +
    ggplot2::geom_tile() +
    khroma::scale_fill_batlow() +
    ggplot2::labs(fill = "Intensity") +
    ggplot2::xlab("Fraction") +
    ggplot2::ylab("Jury") +
    ggplot2::theme_minimal() +
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

  matrix_curated <- tables_profiles_curated |>
    ggplot2::ggplot(mapping = ggplot2::aes(fraction, jury, fill = value)) +
    ggplot2::geom_tile() +
    khroma::scale_fill_batlow() +
    ggplot2::labs(fill = "Intensity") +
    ggplot2::xlab("Fraction") +
    ggplot2::ylab("Jury") +
    ggplot2::theme_minimal() +
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
}

plot_matrices()

end <- Sys.time()

message("Script finished in ", format(end - start))
