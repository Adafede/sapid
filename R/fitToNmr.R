#' Title
#'
#' @param dataFrame
#'
#' @return
#' @export
#'
#' @examples
fitToNmr <- function(dataFrame) {
  samplesList <-
    split(dataFrame, with(dataFrame, interaction(id)), drop = TRUE)

  samplesLength <- seq(seq_along(samplesList))

  metadata_1D <-
    list(external = data.frame(NMRExperiment = names(samplesList)))

  metadata_1D[["external"]][["NMRExperiment"]] <-
    as.character(metadata_1D[["external"]][["NMRExperiment"]])

  data_1r <- list()
  for (i in samplesLength) {
    data_1r[[i]] <- as.list(samplesList[[i]][["intensity"]])
  }

  minimum <- data.frame()
  for (i in samplesLength) {
    minimum[i, 1] <- length(data_1r[[i]])
  }

  minimum <- min(minimum$V1)

  for (i in samplesLength) {
    data_1r[[i]] <- data_1r[[i]][1:minimum]
  }

  for (i in samplesLength) {
    data_1r[[i]] <- as.numeric(unlist(data_1r[[i]]))
  }

  data_fields_1D <- list(data_1r)

  axis <- list()
  for (i in samplesLength) {
    axis[[i]] <- as.list(samplesList[[i]][["time"]])
  }

  minimum <- data.frame()
  for (i in samplesLength) {
    minimum[i, 1] <- length(axis[[i]])
  }

  minimum <- min(minimum$V1)

  for (i in samplesLength) {
    axis[[i]] <- axis[[i]][1:minimum]
  }

  for (i in samplesLength) {
    axis[[i]] <- as.numeric(unlist(axis[[i]]))
  }

  axis_1D <- axis

  for (i in samplesLength) {
    axis_1D[[i]] <- list(axis_1D[[i]])
  }

  my_1D_data <-
    new_nmr_dataset(metadata_1D, data_fields_1D, axis_1D)

  names(my_1D_data)[2] <- "data_1r"

  return(my_1D_data)
}
