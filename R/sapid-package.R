#' @concept EDAM:topic_3172 Metabolomics
#' @concept EDAM:topic_0154 Small molecules
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:topic_2258 Cheminformatics
#' @concept EDAM:topic_0780 Plant biology
#' @concept EDAM:topic_0593 NMR
#' @concept EDAM:topic_0092 Data visualisation
#' @concept EDAM:operation_3803 Natural product identification
#' @concept EDAM:operation_3214 Spectral analysis
#' @concept EDAM:operation_3432 Clustering
#' @concept EDAM:operation_3465 Correlation
#' @concept EDAM:operation_0337 Visualisation
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table :=
## usethis namespace: end
NULL

.datatable.aware <- TRUE

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to ", pkgname)
  packageStartupMessage(format(utils::citation(pkgname)))
}
