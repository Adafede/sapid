#' Prepare chasselas
#'
#' @param path Path
#' @param cluster Cluster
#' @param date Date
#' @param product_name Product name
#' @param path_output Path output
#'
#' @return NULL
#'
#' @examples NULL
prepare_chasselas <-
  function(path = data_inhouse_sensory_path,
           path_output = analysis_path_04_output,
           date = DATE,
           product_name = PRODUCTNAME,
           cluster = CLUSTER) {
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
      tidytable::distinct(Date, CJ, name, delta)

    results <- list(chasselas_pivoted, chasselas_deltas)

    tidytable::fwrite(
      x = results[[1]],
      file = file.path(path_output, paste0(
        "chasselas_", "cluster_", cluster, ".tsv"
      )),
      sep = "\t"
    )
    tidytable::fwrite(
      x = results[[2]],
      file = file.path(path_output, paste0("deltas_", "cluster_", cluster, ".tsv")),
      sep = "\t"
    )

    return(results)
  }
