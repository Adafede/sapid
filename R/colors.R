paired <- c(
  "#a6cee3",
  "#1f78b4",
  "#b2df8a",
  "#33a02c",
  "#fb9a99",
  "#e31a1c",
  "#fdbf6f",
  "#ff7f00",
  "#cab2d6",
  "#6a3d9a",
  "#ffff99",
  "#b15928"
)

discrete_rainbow_14 <- c(
  "#D1BBD7",
  "#AE76A3",
  "#882E72",
  "#1965B0",
  "#5289C7",
  "#7BAFDE",
  "#4EB265",
  "#90C987",
  "#CAE0AB",
  "#F7F056",
  "#F6C141",
  "#F1932D",
  "#E8601C",
  "#DC050C",
  "#777777"
)

batlow <- c(
  "#011959",
  "#144D62",
  "#3C6D56",
  "#818231",
  "#D29343",
  "#FDAC9C",
  "#FACCFA"
)

scale_fill_batlow <- function(...) {
  ggplot2::scale_fill_gradientn(colours = batlow, ...)
}

scale_color_batlow <- function(...) {
  ggplot2::scale_color_gradientn(colours = batlow, ...)
}

label_si_number <- function(x) {
  vapply(
    x,
    function(v) {
      if (is.na(v)) {
        return(NA_character_)
      }
      av <- abs(v)
      if (av >= 1e9) {
        paste0(format(round(v / 1e9, 2), trim = TRUE), "G")
      } else if (av >= 1e6) {
        paste0(format(round(v / 1e6, 2), trim = TRUE), "M")
      } else if (av >= 1e3) {
        paste0(format(round(v / 1e3, 2), trim = TRUE), "k")
      } else {
        format(round(v, 2), trim = TRUE)
      }
    },
    character(1L)
  )
}

label_short_scale <- function(x) {
  vapply(
    x,
    function(v) {
      if (is.na(v)) {
        return(NA_character_)
      }
      av <- abs(v)
      if (av >= 1e9) {
        paste0(format(round(v / 1e9, 2), trim = TRUE), "B")
      } else if (av >= 1e6) {
        paste0(format(round(v / 1e6, 2), trim = TRUE), "M")
      } else if (av >= 1e3) {
        paste0(format(round(v / 1e3, 2), trim = TRUE), "K")
      } else {
        format(round(v, 2), trim = TRUE)
      }
    },
    character(1L)
  )
}
