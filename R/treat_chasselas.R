#' Title
#'
#' @param path
#' @param product_name
#' @param cluster
#' @param date
#'
#' @return
#' @export
#'
#' @examples
treat_chasselas <-
  function(path = data_inhouse_sensory_path,
           product_name = PRODUCTNAME,
           cluster = CLUSTER,
           date = DATE) {
    xls <- list.files(
      path =
        file.path(
          path,
          paste0(date, "_cluster", cluster),
          "03_files"
        ),
      pattern = ".xlsx",
      full.names = TRUE
    )

    chasselas <-
      read_xlsx(
        path = xls,
        sheet = 1
      ) %>%
      mutate(ProductName = if_else(
        condition = ProductName == product_name,
        true = paste("1_avant_cluster",
          cluster,
          sep = "_"
        ),
        false = paste("2_après_cluster",
          cluster,
          sep = "_"
        )
      )) %>%
      data.frame()

    chasselas_pivoted <- chasselas %>%
      group_by(CJ, ProductName) %>%
      pivot_longer(4:ncol(.)) %>%
      ungroup()

    chasselas_deltas <- chasselas_pivoted %>%
      group_by(CJ, name) %>%
      pivot_wider(names_from = ProductName, values_from = value)

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

    chasselas_deltas <- chasselas_deltas %>%
      distinct(Date, CJ, name, delta)

    results <- list(chasselas_pivoted, chasselas_deltas)

    return(results)
  }
