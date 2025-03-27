#' Get session info
#'
#' @param session Session
#'
#' @return NULL
#'
#' @examples NULL
get_session_info <- function(session) {
  cluster <- switch(
    as.character(session),
    "1" = 6,
    "2" = 2,
    "3" = 5,
    "4" = 7,
    "5" = 3,
    "6" = 4,
    "7" = 1,
    "8" = "All"
  )

  date <- switch(
    as.character(session),
    "1" = 20210412,
    "2" = 20210419,
    "3" = 20210426,
    "4" = 20210510,
    "5" = 20210517,
    "6" = 20210531,
    "7" = 20210607,
    "8" = 20210614
  )

  product_name <- switch(
    as.character(date),
    "20210412" = "CHASAVANT",
    "20210419" = "Chas avant",
    "20210426" = "CHASAVANT",
    "20210510" = "CHASAVANT",
    "20210517" = "CHASAVANT",
    "20210531" = "CHASAVANT",
    "20210607" = "CHASAVANT"
  )

  return(list(
    cluster = cluster,
    date = date,
    product_name = product_name
  ))
}
