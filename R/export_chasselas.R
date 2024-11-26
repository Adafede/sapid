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
export_chasselas <-
  function(path = analysis_path_04_output,
           product_name = PRODUCTNAME,
           cluster = CLUSTER,
           date = DATE) {
    setwd(dir = here())
    orca(
      p = chasselas_boxplots,
      file = file.path(path, paste0(
        "boxplots_",
        "cluster_",
        cluster,
        ".pdf"
      )),
      width = 800,
      height = 450
    )
    orca(
      p = chasselas_deltas,
      file = file.path(path, paste0(
        "deltas_",
        "cluster_",
        cluster,
        ".pdf"
      )),
      width = 800,
      height = 450
    )
    write_tsv(x = chasselas_treated[[1]], file = file.path(path, paste0(
      "chasselas_",
      "cluster_",
      cluster,
      ".tsv"
    )))
    write_tsv(x = chasselas_treated[[2]], file = file.path(path, paste0(
      "deltas_",
      "cluster_",
      cluster,
      ".tsv"
    )))
  }
