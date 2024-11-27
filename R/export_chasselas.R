#' Export chasselas
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
export_chasselas <-
  function(path = analysis_path_04_output,
           product_name = PRODUCTNAME,
           cluster = CLUSTER,
           date = DATE) {
    plotly::orca(
      p = chasselas_boxplots,
      file = file.path(path, paste0(
        "boxplots_", "cluster_", cluster, ".pdf"
      )),
      width = 800,
      height = 450
    )
    plotly::orca(
      p = chasselas_deltas,
      file = file.path(path, paste0("deltas_", "cluster_", cluster, ".pdf")),
      width = 800,
      height = 450
    )
    tidytable::fwrite(
      x = chasselas_treated[[1]],
      file = file.path(path, paste0(
        "chasselas_", "cluster_", cluster, ".tsv"
      )),
      sep = "\t"
    )
    tidytable::fwrite(
      x = chasselas_treated[[2]],
      file = file.path(path, paste0("deltas_", "cluster_", cluster, ".tsv")),
      sep = "\t"
    )
  }
