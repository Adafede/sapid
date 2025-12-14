# Taken from https://raw.githubusercontent.com/davidsjoberg/ggbump/refs/heads/master/R/geom_sigmoid.R
# as it was removed from CRAN
# simply changed `dplyr::` to `tidytable::` and `%>%` to `|>`

# ** StatSigmoid ------------------------------------------------------------------

StatSigmoid <- ggplot2::ggproto(
  "StatSigmoid",
  ggplot2::Stat,
  setup_data = function(data, params) {
    data <- data |>
      tidytable::group_by(PANEL) |>
      tidytable::mutate(group = dplyr::row_number()) |>
      as.data.frame()
    data |> print()
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
