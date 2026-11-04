# Taken from https://raw.githubusercontent.com/davidsjoberg/ggbump/refs/heads/master/R/geom_sigmoid.R
# as it was removed from CRAN
# adapted to use base R

# ** StatSigmoid ------------------------------------------------------------------

StatSigmoid <- ggplot2::ggproto(
  "StatSigmoid",
  ggplot2::Stat,
  setup_data = function(data, params) {
    data$group <- stats::ave(
      seq_len(nrow(data)),
      data$PANEL,
      FUN = seq_along
    )
    as.data.frame(data)
  },
  compute_group = function(data, scales, smooth = 8, direction = "x") {
    sigmoid(
      data$x,
      data$xend,
      data$y,
      data$yend,
      smooth = smooth,
      direction = direction
    )
  },
  required_aes = c("x", "y", "xend", "yend")
)

# ** geom_sigmoid -----------------------------------------------------------------

#' Smooth sigmoid curve layer for ggplot2
#'
#' @concept EDAM:topic_0092 Data visualisation
#' @concept EDAM:operation_0337 Visualisation
#' @param mapping Aesthetic mapping (`x`, `xend`, `y`, and `yend` must be numeric).
#' @param data Layer data frame.
#' @param geom Geom to use (default `"line"`).
#' @param position Position adjustment (default `"identity"`).
#' @param na.rm Logical; remove missing values if `TRUE`.
#' @param show.legend Logical; include layer in legend.
#' @param smooth Numeric steepness parameter for the sigmoid curve (default `8`).
#' @param direction Character smoothing direction: `"x"` or `"y"`.
#' @param inherit.aes Logical; inherit aesthetics from the parent plot.
#' @param ... Additional arguments passed to the layer.
#'
#' @return A `ggplot2` layer.
#' @export
#' @examples
#' df <- data.frame(x = 1, xend = 5, y = 0, yend = 1)
#' ggplot2::ggplot(df) +
#'   geom_sigmoid(ggplot2::aes(x = x, xend = xend, y = y, yend = yend))
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

#' Generate coordinates for a smooth sigmoid curve
#'
#' @concept EDAM:topic_0092 Data visualisation
#' @concept EDAM:operation_0337 Visualisation
#' @param x_from Numeric start x coordinate.
#' @param x_to Numeric end x coordinate.
#' @param y_from Numeric start y coordinate.
#' @param y_to Numeric end y coordinate.
#' @param smooth Numeric steepness parameter (default `5`).
#' @param n Integer number of interpolated points (default `100`).
#' @param direction Character smoothing direction: `"x"` or `"y"`.
#'
#' @return A `data.frame` with interpolated `x` and `y` coordinates.
#' @export
#' @examples
#' head(sigmoid(0, 10, 0, 1))
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

  t_seq <- seq(-smooth, smooth, length.out = n)
  s_val <- stats::plogis(t_seq)
  lin_val <- (t_seq + smooth) / (smooth * 2)

  if (direction == "x") {
    data.frame(
      x = lin_val * (x_to - x_from) + x_from,
      y = s_val * (y_to - y_from) + y_from
    )
  } else {
    data.frame(
      y = lin_val * (y_to - y_from) + y_from,
      x = s_val * (x_to - x_from) + x_from
    )
  }
}
