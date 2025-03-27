start <- Sys.time()

pkgload::load_all()

message("This program plots descriptors network.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Plot descriptors network
#'
#' @param input Input
#' @param output Output
#'
#' @return NULL
#'
#' @examples NULL
#'
plot_descriptors_network <- function(
  input = system.file("extdata", "napping_descriptors.tsv", package = "sapid"),
  output = "./data/figures/figure_network.pdf"
) {
  table_descriptors <- input |>
    tidytable::fread() |>
    tidytable::filter(taste_harmonized != "") |>
    tidytable::distinct(fraction, jury, taste_original, taste_harmonized) |>
    tidytable::group_by(taste_original) |>
    tidytable::mutate(count = tidytable::n_distinct(jury)) |>
    tidytable::ungroup() |>
    tidytable::filter(count >= 2)

  table_edges_1 <- table_descriptors |>
    tidytable::select(from = fraction, to = taste_original)

  table_edges_2 <- table_descriptors |>
    tidytable::select(from = taste_original, to = taste_harmonized)

  table_edges <- table_edges_1 |>
    tidytable::bind_rows(table_edges_2)

  table_edges_size <- table_edges |>
    tidytable::group_by(to) |>
    tidytable::count(name = "size")

  table_vertices <- table_edges |>
    tidytable::distinct(name = from) |>
    tidytable::bind_rows(
      table_edges_size |>
        tidytable::distinct(name = to, size)
    ) |>
    tidytable::arrange(tidytable::desc(size)) |>
    tidytable::distinct(name, .keep_all = TRUE) |>
    tidytable::full_join(
      tidytable::bind_rows(
        table_descriptors |>
          tidytable::distinct(taste = taste_original, color = taste_harmonized),
        table_descriptors |>
          tidytable::mutate(taste = taste_harmonized) |>
          tidytable::distinct(taste, color = taste_harmonized)
      ) |>
        tidytable::distinct(taste, color),
      by = c("name" = "taste")
    ) |>
    tidytable::mutate(
      size = tidytable::if_else(
        condition = name |>
          grepl(pattern = "fraction", fixed = TRUE),
        true = 1,
        false = size
      )
    ) |>
    tidytable::group_by(color) |>
    tidytable::mutate(
      sum = size |>
        sum()
    )

  table_vertices$color <- table_vertices$color |>
    forcats::fct_reorder(table_vertices$sum, .desc = TRUE)
  table_vertices$name <- table_vertices$name |>
    forcats::fct_reorder(table_vertices$sum, .desc = TRUE)

  graph <- table_edges |>
    igraph::graph_from_data_frame(directed = FALSE, vertices = table_vertices)

  igraph::V(graph)$name <- table_vertices$name
  igraph::V(graph)$color <- table_vertices$color

  plot <- graph |>
    ggraph::ggraph(layout = "stress") +
    ggplot2::scale_color_manual(
      values = discrete_rainbow_14,
      na.value = "grey60"
    ) +
    ggplot2::scale_fill_manual(
      values = discrete_rainbow_14,
      na.value = "grey60"
    ) +
    ggraph::scale_edge_color_manual(
      values = discrete_rainbow_14,
      na.value = "grey60"
    ) +
    ggraph::geom_edge_bundle_force(
      edge_alpha = 0.1,
      edge_color = "grey30",
      edge_width = 0.01,
      n_cycle = 10,
      threshold = 0.3,
      show.legend = FALSE
    ) +
    ggraph::geom_node_point(
      mapping = ggplot2::aes(size = size, color = color),
      show.legend = FALSE
    ) +
    ggraph::geom_node_text(
      mapping = ggplot2::aes(
        label = name,
        size = size,
        color = color
      ),
      repel = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::theme_void()

  cascade:::check_export_dir(output)
  plot |>
    ggplot2::ggsave(
      filename = output,
      width = 7,
      height = 7
    )
}

plot_descriptors_network()

end <- Sys.time()

message("Script finished in ", format(end - start))
