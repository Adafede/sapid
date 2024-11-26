###############################################################################
################################  Parameters  #################################
###############################################################################

## TO FILL
ENABLEDATAVIZ <- FALSE

SESSION <- 6

## AUTOMATIC
CLUSTER <- switch(as.character(SESSION),
  "1" = 6,
  "2" = 2,
  "3" = 5,
  "4" = 7,
  "5" = 3,
  "6" = 4,
  "7" = 1,
  "8" = "All"
)

DATE <- switch(as.character(SESSION),
  "1" = 20210412,
  "2" = 20210419,
  "3" = 20210426,
  "4" = 20210510,
  "5" = 20210517,
  "6" = 20210531,
  "7" = 20210607,
  "8" = 20210614,
)

PRODUCTNAME <- switch(as.character(DATE),
  "20210412" = "CHASAVANT",
  "20210419" = "Chas avant",
  "20210426" = "CHASAVANT",
  "20210510" = "CHASAVANT",
  "20210517" = "CHASAVANT",
  "20210531" = "CHASAVANT",
  "20210607" = "CHASAVANT"
)

min_fraction <- 17

max_fraction <- 71

min_diluted_fraction <- 32

max_diluted_fraction <- 39

n_fraction <- 63

temp_head <- 13

min_fc <- 0.5

dilution_factor <- 50

iHaveNoTime <- TRUE

min_rv_coeff <- 0.3

MIN_PANELISTS <- 2 ## really hard
