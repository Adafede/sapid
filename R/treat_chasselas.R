#' Treat chasselas
#'
#' @param path Path
#' @param product_name Product name
#' @param cluster Cluster
#' @param date Date
#'
#' @return NULL
#' @export
#'
#' @examples NULL
treat_chasselas <-
  function(path = data_inhouse_sensory_path,
           product_name = PRODUCTNAME,
           cluster = CLUSTER,
           date = DATE) {
    xls <- list.files(
      path =
        file.path(path, paste0(date, "_cluster", cluster), "03_files"),
      pattern = ".xlsx",
      full.names = TRUE
    )

    chasselas <-
      readxl::read_xlsx(path = xls, sheet = 1) |>
      tidytable::mutate(
        ProductName = tidytable::if_else(
          condition = ProductName == product_name,
          true = paste("1_avant_cluster", cluster, sep = "_"),
          false = paste("2_après_cluster", cluster, sep = "_")
        )
      ) |>
      data.frame()

    chasselas_pivoted <- chasselas |>
      tidytable::group_by(CJ, ProductName) |>
      tidytable::pivot_longer(tidytable::where(is.numeric)) |>
      tidytable::ungroup() |>
      data.frame()

    chasselas_deltas <- chasselas_pivoted |>
      tidytable::group_by(CJ, name) |>
      tidytable::pivot_wider(names_from = ProductName, values_from = value) |>
      data.frame()

    chasselas_deltas[, "delta"] <-
      chasselas_deltas[, grep(
        pattern = "2_après_cluster",
        x = colnames(chasselas_deltas),
        value = TRUE
      )] - chasselas_deltas[, grep(
        pattern = "1_avant_cluster",
        x = colnames(chasselas_deltas),
        value = TRUE
      )]

    chasselas_deltas <- chasselas_deltas |>
      distinct(Date, CJ, name, delta)

    results <- list(chasselas_pivoted, chasselas_deltas)

    return(results)
  }
