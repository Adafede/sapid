# Taken from https://raw.githubusercontent.com/davidsjoberg/ggbump/refs/heads/master/R/geom_sigmoid.R
# as it was removed from CRAN
# simply changed `dplyr` to `tidytable` and `%>%` to `|>`

# ** StatSigmoid ------------------------------------------------------------------

StatSigmoid <- ggplot2::ggproto(
  "StatSigmoid",
  ggplot2::Stat,
  setup_data = function(data, params) {
    data <- data |>
      tidytable::group_by(PANEL) |>
      tidytable::mutate(group = tidytable::row_number()) |>
      as.data.frame()
    data
  },
  compute_group = function(data, scales, smooth = 8, direction = "x") {
    out <- sigmoid(
      data$x,
      data$xend,
      data$y,
      data$yend,
      smooth = smooth,
      direction = direction
    )
    out
  },
  required_aes = c("x", "y", "xend", "yend")
)

# ** geom_sigmoid -----------------------------------------------------------------

#' geom_sigmoid
#' @param mapping provide you own mapping. both x, xend, y and yend need to be numeric.
#' @param data provide you own data
#' @param geom xhange geom
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param smooth how much smooth should the curve have? More means steeper curve.
#' @param direction the character x or y depending of smoothing direction
#' @param inherit.aes should the geom inherits aestethics
#' @param ... other arguments to be passed to the geom
#'
#' @return ggplot layer
geom_sigmoid <- function(
  mapping = NULL,
  data = NULL,
  geom = "line",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  smooth = 8,
  direction = "x",
  inherit.aes = TRUE,
  ...
) {
  ggplot2::layer(
    stat = StatSigmoid,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, smooth = smooth, direction = direction, ...)
  )
}

#' sigmoid
#'
#' Creates a longer dataframe with coordinates for a smoothed line.
#'
#' @param x_from start x value
#' @param x_to end x value
#' @param y_from start y value
#' @param y_to end y values
#' @param n number of point that should be smoothed
#' @param smooth smooth parameter. Higher means less smoothing
#' @param direction the character x or y depending on direction of smoothing
#'
#' @return a data frame
#'
sigmoid <- function(
  x_from,
  x_to,
  y_from,
  y_to,
  smooth = 5,
  n = 100,
  direction = "x"
) {
  if (!direction %in% c("x", "y")) {
    stop("Only the directions x or y is allowed.")
  }

  if (direction == "x") {
    x <- seq(-smooth, smooth, length = n)
    y <- exp(x) / (exp(x) + 1)
    out <- data.frame(
      x = (x + smooth) / (smooth * 2) * (x_to - x_from) + x_from,
      y = y * (y_to - y_from) + y_from
    )
  }

  if (direction == "y") {
    y <- seq(-smooth, smooth, length = n)
    x <- exp(y) / (exp(y) + 1)
    out <- data.frame(
      y = (y + smooth) / (smooth * 2) * (y_to - y_from) + y_from,
      x = x * (x_to - x_from) + x_from
    )
  }
  out
}
