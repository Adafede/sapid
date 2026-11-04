#' Plot raw extract and fraction chromatograms
#'
#' Load and baseline-correct `.mzML` chromatograms for the raw plant extract
#' and its fractions (mirrored along the y-axis), colored by NMR cluster group.
#'
#' @concept EDAM:topic_3172 Metabolomics
#' @concept EDAM:operation_3203 Chromatogram visualisation
#' @concept EDAM:data_2536 Mass spectrometry data
#' @concept EDAM:format_3244 mzML
#' @param input_dir Directory containing `.mzML` files.
#' @param input_groups Optional path or `data.frame` of sample cluster groups
#'   (defaults to built-in [groups]).
#' @param raw_index Integer index of the raw extract file in `input_dir`.
#' @param fractions_indices Integer vector of fraction file indices in `input_dir`.
#' @param xlim Numeric length-2 retention time range in minutes (default `c(2, 25)`).
#' @param output Output PDF file path.
#'
#' @return Output file path (`output`).
#' @export
#' @examples
#' \dontrun{
#' plot_chromatograms()
#' }
plot_chromatograms <- function(
  input_dir = "./data/20210619",
  input_groups = NULL,
  raw_index = 7,
  fractions_indices = 12:65,
  xlim = c(2, 25),
  output = "./data/figures/figure_chromatograms.pdf"
) {
  table_groups <- load_dataset_or_file(input_groups, "groups")

  files <- list.files(
    path = input_dir,
    pattern = "\\.mzML$",
    full.names = TRUE
  )

  # Continue with rest of function using table_groups instead of re-reading
  groups <- table_groups

  extract_chromatogram_df <- function(file) {
    chrom_loaded <- .cascade_load_chromatograms(file)
    preprocessed <- .cascade_preprocess_chromatograms(
      list = list(chrom_loaded[[3L]]),
      name = .cascade_load_name(file)
    )
    baselined <- as.data.frame(
      preprocessed$chromatograms_baselined[[1L]],
      stringsAsFactors = FALSE
    )
    out <- unique(baselined[, c("time", "intensity"), drop = FALSE])
    out$id <- file
    out
  }

  data_raw <- do.call(rbind, lapply(files[raw_index], extract_chromatogram_df))
  data_fractions <- do.call(
    rbind,
    lapply(files[fractions_indices], extract_chromatogram_df)
  )
  data_fractions$intensity <- -1 * data_fractions$intensity

  data_full <- rbind(data_raw, data_fractions)
  data_full$id <- gsub("_.*", "", gsub(".*_M_", "", data_full$id))

  groups_lookup <- data.frame(
    id = as.character(table_groups$rowname),
    group = as.character(table_groups$group),
    stringsAsFactors = FALSE
  )
  data_full <- merge(data_full, groups_lookup, by = "id", all = TRUE)

  id_group_pairs <- unique(data_full[, c("id", "group"), drop = FALSE])
  color_values <- stats::setNames(id_group_pairs$group, id_group_pairs$id)

  plot <- ggplot2::ggplot(
    data_full,
    mapping = ggplot2::aes(x = time, y = intensity, color = id)
  ) +
    ggplot2::geom_line(linewidth = 0.2, show.legend = FALSE) +
    ggplot2::scale_color_manual(
      values = color_values,
      na.value = "#767676"
    ) +
    ggplot2::xlim(xlim) +
    ggplot2::ylim(c(-32, 32)) +
    ggplot2::xlab("Time [min]") +
    ggplot2::ylab("Intensity") +
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
  ggplot2::ggsave(
    filename = output,
    plot = plot,
    width = 16,
    height = 12
  )
  output
}
