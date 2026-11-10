#' Plot sensory descriptors network
#'
#' Construct and render a network connecting sample fractions, original panelist
#' taste terms, and harmonized taste descriptors for descriptors used by at
#' least two panelists.
#'
#' @details
#' Duplicate parallel edges (when multiple panelists assign the same descriptor
#' to the same fraction, or when multiple original terms map to the same
#' harmonized taste) are collapsed into a single weighted directed graph
#' (`weight` attribute) prior to Fruchterman--Reingold layout computation, while
#' preserving total vertex in-strength (`size`) for node scaling.
#'
#' @concept EDAM:topic_0092 Data visualisation
#' @concept EDAM:operation_3925 Network visualisation
#' @concept EDAM:operation_0337 Visualisation
#' @param input Optional path or `data.frame` of napping descriptors (defaults
#'   to built-in [napping_descriptors]).
#' @param output Output PDF file path.
#'
#' @return Output file path (`output`).
#' @export
#' @examples
#' \donttest{
#' out <- tempfile(fileext = ".pdf")
#' plot_descriptors_network(output = out)
#' }
plot_descriptors_network <- function(
  input = NULL,
  output = "./data/figures/figure_network.pdf"
) {
  input_data <- load_dataset_or_file(input, "napping_descriptors")

  non_empty <- input_data[
    !is.na(input_data$taste_harmonized) & input_data$taste_harmonized != "",
    c("fraction", "jury", "taste_original", "taste_harmonized"),
    drop = FALSE
  ]
  table_descriptors <- unique(non_empty)

  jury_per_orig <- tapply(
    table_descriptors$jury,
    table_descriptors$taste_original,
    function(x) length(unique(x))
  )
  keep_orig <- names(jury_per_orig)[jury_per_orig >= 2L]
  table_descriptors <- table_descriptors[
    table_descriptors$taste_original %in% keep_orig,
    ,
    drop = FALSE
  ]

  table_edges_raw <- rbind(
    data.frame(
      from = table_descriptors$fraction,
      to = table_descriptors$taste_original,
      stringsAsFactors = FALSE
    ),
    data.frame(
      from = table_descriptors$taste_original,
      to = table_descriptors$taste_harmonized,
      stringsAsFactors = FALSE
    )
  )

  to_counts <- table(table_edges_raw$to)
  table_edges_size <- data.frame(
    name = names(to_counts),
    size = as.numeric(to_counts),
    stringsAsFactors = FALSE
  )

  edge_key <- paste(table_edges_raw$from, table_edges_raw$to, sep = "\r")
  edge_weights <- stats::ave(
    rep(1, nrow(table_edges_raw)),
    edge_key,
    FUN = sum
  )
  table_edges <- table_edges_raw[!duplicated(edge_key), , drop = FALSE]
  table_edges$weight <- as.numeric(edge_weights[!duplicated(edge_key)])
  rownames(table_edges) <- NULL

  from_only <- setdiff(unique(table_edges$from), table_edges_size$name)
  vertices_raw <- rbind(
    table_edges_size,
    data.frame(
      name = from_only,
      size = rep(NA_real_, length(from_only)),
      stringsAsFactors = FALSE
    )
  )
  vertices_raw <- vertices_raw[
    order(vertices_raw$size, decreasing = TRUE, na.last = TRUE),
    ,
    drop = FALSE
  ]
  vertices_raw <- vertices_raw[!duplicated(vertices_raw$name), , drop = FALSE]

  color_map_df <- unique(rbind(
    data.frame(
      name = table_descriptors$taste_original,
      color = table_descriptors$taste_harmonized,
      stringsAsFactors = FALSE
    ),
    data.frame(
      name = table_descriptors$taste_harmonized,
      color = table_descriptors$taste_harmonized,
      stringsAsFactors = FALSE
    )
  ))
  color_map_df <- color_map_df[!duplicated(color_map_df$name), , drop = FALSE]

  table_vertices <- merge(
    vertices_raw,
    color_map_df,
    by = "name",
    all.x = TRUE,
    sort = FALSE
  )
  is_frac <- grepl("fraction", table_vertices$name, fixed = TRUE)
  table_vertices$size[is_frac] <- 1
  table_vertices$sum <- stats::ave(
    table_vertices$size,
    ifelse(is.na(table_vertices$color), "__NA__", table_vertices$color),
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  table_vertices$sum[is.na(table_vertices$color)] <- NA_real_

  table_vertices$color <- fct_reorder_base(
    table_vertices$color,
    table_vertices$sum,
    desc = TRUE
  )

  mygraph <- igraph::graph_from_data_frame(
    d = table_edges,
    vertices = table_vertices
  )

  plot <- ggraph::ggraph(mygraph) +
    ggraph::geom_edge_arc(
      ggplot2::aes(edge_alpha = weight),
      strength = 0.2,
      width = 0.5,
      show.legend = FALSE
    ) +
    ggraph::scale_edge_alpha(range = c(0.15, 0.7)) +
    ggraph::geom_node_point(
      ggplot2::aes(size = size, color = color),
      show.legend = FALSE
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(label = name, size = size, color = color),
      repel = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = discrete_rainbow_14,
      na.value = "#767676"
    ) +
    ggplot2::theme_void()

  .cascade_check_export_dir(dirname(output))
  plot |>
    ggplot2::ggsave(
      filename = output,
      width = 7,
      height = 7
    )
  output
}
