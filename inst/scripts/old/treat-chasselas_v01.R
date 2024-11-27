message("This script is made to treat Chasselas before/aftere fractions tasting. \n")

start <- Sys.time()

message("Loading ... \n")
message("... packages (and installing them if needed) \n")
if (!require(conflicted)) {
  install.packages("conflicted")
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

message("... paths and parameters \n")
source(file = "paths.R"))
source(file = "params.R"))

message("... functions \n")
source(file = "r/violin_chasselas.R")
source(file = "r/violin_deltas_chasselas.R")
source(file = "r/export_chasselas.R")
source(file = "r/treat_chasselas.R")

chasselas_treated <- treat_chasselas()

chasselas_boxplots <- violin_chasselas()

chasselas_deltas <- violin_deltas_chasselas()

export_chasselas()
