require(plotly)

#' Title
#'
#' @return
#' @export
#'
#' @examples
violin_chasselas <-
  function() {
    figure <- plot_ly(
      data = chasselas_treated[[1]],
      x = ~name,
      y = ~value,
      color = ~ProductName,
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
      plotly::layout(yaxis = list(range = c(0, 10)))

    return(figure)
  }
