cat("This script is made to treat Chasselas before/aftere fractions tasting. \n")

start <- Sys.time()

cat("Loading ... \n")
cat("... packages (and installing them if needed) \n")
if (!require(conflicted)) {
  install.packages("conflicted")
}
if (!require(here)) {
  install.packages("here")
}
if (!require(plotly)) {
  install.packages("plotly")
}
if (!require(readxl)) {
  install.packages("readxl")
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
}

cat("... paths and parameters \n")
source(file = here("paths.R"))
source(file = here("params.R"))

cat("... functions \n")
source(file = here("r/violin_chasselas.R"))
source(file = here("r/violin_deltas_chasselas.R"))
source(file = here("r/export_chasselas.R"))
source(file = here("r/treat_chasselas.R"))

chasselas_treated <- treat_chasselas()

chasselas_boxplots <- violin_chasselas()

chasselas_deltas <- violin_deltas_chasselas()

export_chasselas()
