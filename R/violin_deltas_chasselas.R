require(plotly)

#' Title
#'
#' @return
#' @export
#'
#' @examples
violin_deltas_chasselas <-
  function() {
    figure <- plotly::plot_ly(
      data = chasselas_treated[[2]],
      x = ~name,
      y = ~delta,
      type = "violin",
      box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      colors = "Paired",
      points = "suspectedoutliers",
      marker = list(
        outliercolor = "e31a1c",
        line = list(
          outliercolor = "e31a1c",
          outlierwidth = 2
        )
      )
    ) |>
      plotly::layout(yaxis = list(range = c(-5, 5)))

    return(figure)
  }
