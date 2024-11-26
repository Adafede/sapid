###############################################################################
##################################   Paths   ##################################
###############################################################################

# root is 03_analysis

## data
data_path <- file.path("~/switchdrive/SAPERE/02_raw-data")

### inhouse
data_inhouse_path <- file.path(data_path, "inhouse")

#### nmr
data_inhouse_nmr_path <- file.path(data_inhouse_path, "01_nmr")

##### 20210324_fractions
data_inhouse_nmr_20210324_path <-
  file.path(data_inhouse_nmr_path, "20210324_fractions")

###### files
data_inhouse_nmr_20210324_files_path <-
  file.path(data_inhouse_nmr_20210324_path, "03_files")

###### matrix
data_inhouse_nmr_20210324_files_matrix_path <-
  file.path(
    data_inhouse_nmr_20210324_files_path,
    "20210324_10043-nmr-matrix.csv"
  )

#### sensory
data_inhouse_sensory_path <-
  file.path(data_inhouse_path, "02_sensory")

data_inhouse_sensory_20210329_files_excel_path <-
  file.path(
    data_inhouse_sensory_path,
    "20210329_raw-extract/03_files/20210329_raw-extract.xlsx"
  )

### public
data_public_path <- file.path(data_path, "public")

## figures
figures_path <- file.path("~/switchdrive/SAPERE/06_figures")

### clusters_full
figures_nmr_clusters_full_path <-
  file.path(figures_path, "hierarchial_clustering_canberra_full_6.pdf")

### clusters_restricted
figures_nmr_clusters_restricted_path <-
  file.path(
    figures_path,
    "hierarchial_clustering_canberra_restricted_7.pdf"
  )

### boxes raw extract
figures_boxes_raw_extract_path <-
  file.path(figures_path, "20210329_boxes-raw-extract.pdf")

### bars raw extract
figures_bars_raw_extract_path <-
  file.path(figures_path, "20210329_bars-raw-extract.pdf")

### lollipop raw extract
figures_lollipop_raw_extract_path <-
  file.path(figures_path, "20210329_lollipop-raw-extract.pdf")

### lollipop raw extract corrected
figures_lollipop_corrected_raw_extract_path <-
  file.path(figures_path, "20210329_lolipop-corrected-raw-extract.pdf")

## analysis
analysis_path <- file.path("~/switchdrive/SAPERE/03_analysis")

### fractions-concentration
analysis_path_01 <-
  file.path(analysis_path, "01_fractions-concentration")

### fractions-concentration
analysis_path_02 <-
  file.path(analysis_path, "02_fractions-clustering")

### fractions-concentration
analysis_path_03 <-
  file.path(analysis_path, "03_raw-extract-sensory")

### fractions-concentration
analysis_path_04 <-
  file.path(analysis_path, "04_fractions-sensory")

#### output
analysis_path_04_output <-
  file.path(analysis_path_04, "03_output")

## r
functions <- file.path("r")

## dictionary
dictionary_generic_path <- "inst/extdata/dictionary_generic.tsv"

dictionary_napping_path <- "inst/extdata/dictionary_napping.tsv"

dictionary_specific_path <- "inst/extdata/dictionary_specific.tsv"
