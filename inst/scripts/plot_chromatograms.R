start <- Sys.time()

pkgload::load_all()

message("This program plots chromatograms.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Plot chromatograms
#'
#' @param input_dir Input dir
#' @param input_groups Input groups
#' @param raw_index Raw index
#' @param fractions_indices Fractions indices
#' @param xlim Xlim
#' @param output Output
#'
#' @return NULL
#'
#' @examples NULL
plot_chromatograms <- function(input_dir = "./data/20210619",
                               input_groups = system.file("extdata", "groups.tsv", package = "sapid"),
                               raw_index = 7,
                               fractions_indices = 12:65,
                               xlim = c(2, 25),
                               output = "./man/figures/figure_chromatograms.pdf") {
  files <- input_dir |>
    list.files(
      path = ,
      pattern = ".mzML",
      full.names = TRUE
    )

  groups <- input_groups |>
    tidytable::fread()

  extract_chromatogram_df <- function(file) {
    cascade:::preprocess_chromatograms(
      list = file |>
        cascade:::load_chromatograms() |>
        purrr::pluck(3) |>
        list(),
      name = file |>
        cascade:::load_name()
    )$chromatograms_baselined |>
      purrr::pluck(1) |>
      tidytable::distinct(time, intensity) |>
      tidytable::mutate(id = file)
  }

  data_raw <- files[raw_index] |>
    purrr::map(.f = extract_chromatogram_df) |>
    tidytable::bind_rows()

  data_fractions <- files[fractions_indices] |>
    purrr::map(.f = extract_chromatogram_df) |>
    tidytable::bind_rows()

  data_full <- data_raw |>
    tidytable::bind_rows(data_fractions |>
      tidytable::mutate(intensity = -1 * intensity)) |>
    tidytable::mutate(
      id = id |>
        gsub(pattern = ".*_M_", replacement = "") |>
        gsub(pattern = "_.*", replacement = "")
    ) |>
    tidytable::full_join(
      groups |>
        tidytable::mutate(id = rowname |>
          as.character()),
      by = c("id" = "id")
    )
  plot <- data_full |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = time, y = intensity, color = id)) +
    ggplot2::geom_line(size = 0.2, show.legend = FALSE) +
    ggplot2::scale_color_manual(
      values = data_full |>
        tidytable::distinct(id, group) |>
        tidytable::pull(group),
      na.value = "grey30"
    ) +
    ggplot2::xlim(xlim) +
    ggplot2::ylim(c(-32, 32)) +
    ggplot2::xlab("Time [min]") +
    ggplot2::ylab("Intensity") +
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

  plot |>
    ggplot2::ggsave(
      filename = output,
      width = 16,
      height = 12
    )
  return(output)
}
plot_chromatograms()

end <- Sys.time()

message("Script finished in ", format(end - start))
