cat("This script is made to treat NAPPING results. \n")

start <- Sys.time()

cat("Loading ... \n")
cat("... packages (and installing them if needed) \n")
if (!require(conflicted)) {
  install.packages("conflicted")
}
if (!require(here)) {
  install.packages("here")
}
if (!require(readxl)) {
  install.packages("readxl")
}
if (!require(splitstackshape)) {
  install.packages("splitstackshape")
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
}
if (!require(SensoMineR)) {
  install.packages("SensoMineR")
}

cat("... paths and parameters \n")
source(file = here("paths.R"))
source(file = here("params.R"))

cat("... functions \n")
source(file = here("r/clean_terms.R"))
source(file = here("r/y_as_na.R"))

cat("... files ... \n")
xls <- list.files(
  path = file.path(
    data_inhouse_sensory_path,
    paste0(DATE, "_cluster", CLUSTER),
    "03_files"
  ),
  pattern = ".xlsx",
  full.names = TRUE
)

cat("... text \n")
file_text <- readxl::read_xlsx(
  path = xls,
  sheet = 3
)

clean_text_napping <- function(df) {
  file_text_cleaned <- df |>
    tidyr::pivot_longer(2:ncol(df)) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(value_2 = clean_terms(
      x = value,
      dictionary = dictionary_specific_path
    )) |>
    dplyr::mutate(value_3 = clean_terms_2(
      x = value_2,
      dictionary = dictionary_napping_path
    )) |>
    splitstackshape::cSplit("value_3",
      sep = " ",
      direction = "long"
    ) |>
    splitstackshape::cSplit("value_3",
      sep = "_",
      direction = "wide"
    ) |>
    dplyr::mutate(value_4 = value_3_1, intensity = value_3_2) |>
    dplyr::filter(!is.na(value_4)) |>
    dplyr::mutate(value_5 = clean_terms(value_4,
      dictionary = dictionary_generic_path
    )) |>
    dplyr::mutate(newValue = clean_terms(value_5,
      dictionary = dictionary_specific_path
    )) |>
    dplyr::mutate(intensity = y_as_na(intensity, "")) |>
    dplyr::mutate(taste = if_else(
      condition = !is.na(intensity),
      true = paste(newValue,
        intensity,
        sep = "_"
      ),
      false = newValue
    )) |>
    dplyr::filter(!is.na(taste)) |>
    dplyr::relocate(taste, .after = name)

  return(file_text_cleaned)
}

file_text_cleaned <- clean_text_napping(df = file_text)

file_text_raw <- file_text |>
  tidyr::pivot_longer(2:ncol(file_text)) |>
  dplyr::filter(!is.na(value)) |>
  splitstackshape::cSplit("value",
    sep = " ",
    direction = "long"
  ) |>
  dplyr::filter(!is.na(value))

words_cleaned <- FactoMineR::textual(
  tab = file_text_cleaned,
  maj.in.min = TRUE,
  sep.word = c(" "),
  num.text = 3,
  contingence.by = c(1) ## ask Pascale
)

words_raw <- FactoMineR::textual(
  tab = file_text_raw,
  maj.in.min = TRUE,
  sep.word = c(" "),
  num.text = 3,
  contingence.by = c(1) ## ask Pascale
)

df_words_cleaned <- words_cleaned$cont.table |>
  data.frame()

df_words_raw <- words_raw$cont.table |>
  data.frame()

# import the coordinate data set
df_coord <- readxl::read_xlsx(
  path = xls,
  sheet = 2
) |>
  data.frame()

rownames(df_coord) <- df_coord$Produit

df_coord <- df_coord %>%
  dplyr::select(-Produit)

if (SESSION == 8) {
  df_coord <- df_coord |>
    dplyr::filter(rownames(df_coord) != 23 &
      rownames(df_coord) != 32) ## discarded

  df_words_cleaned <- df_words_cleaned |>
    dplyr::filter(rownames(df_words_cleaned) != 23 &
      rownames(df_words_cleaned) != 32) ## discarded

  df_words_raw <- df_words_raw |>
    dplyr::filter(rownames(df_words_raw) != 23 &
      rownames(df_words_raw) != 32) ## discarded
}

df_coord <-
  df_coord[, colSums(is.na(df_coord)) < nrow(df_coord)]

df_words_cleaned <-
  df_words_cleaned[, colSums(df_words_cleaned) != 0]

df_words_raw <-
  df_words_raw[, colSums(df_words_raw) != 0]

if (iHaveNoTime != TRUE) {
  nap.tot <- cbind(df_coord, df_words_cleaned)

  ##### PMFA (Procuste Multiple Factor Analysis to obtain individual tableclothes  ####
  single.nap <- SensoMineR::pmfa(
    matrice = df_coord,
    matrice.illu = df_words_cleaned,
    mean.conf = NULL,
    dilat = TRUE,
    graph.ind = TRUE,
    graph.mfa = TRUE,
    coord = c(1, 2),
    cex = 0.8
  )

  ###
  ## decide to use this part or not
  # single.nap_high <- single.nap |>
  #   data.frame() |>
  #   dplyr::filter(RV.coeff >= min_rv_coeff)
  #
  # nappes_ok <-
  #   gsub(
  #     pattern = "Y",
  #     replacement = "",
  #     x = rownames(single.nap_high)
  #   )
  #
  # df_coord_clean <- df_coord |>
  #   select(contains(match = nappes_ok))
  #
  # clean.nap <- SensoMineR::pmfa(
  #   matrice = df_coord_clean,
  #   matrice.illu = df_words_cleaned,
  #   mean.conf = NULL,
  #   dilat = TRUE,
  #   graph.ind = TRUE,
  #   graph.mfa = TRUE,
  #   coord = c(1, 2)
  # )
  ###

  ##### MFA with descriptors assessment   ####
  res.nap <- FactoMineR::MFA(
    base = nap.tot,
    group = c(rep(2, ncol(df_coord) / 2), ncol(df_words_cleaned)),
    type = c(rep("c", ncol(df_coord) / 2), "s"),
    axes = c(1, 2)
  )

  ##### MFA with word frequencies   ####
  res.mfa <- FactoMineR::MFA(
    base = nap.tot,
    group = c(rep(2, ncol(df_coord) / 2), ncol(df_words_cleaned)),
    type = c(rep("c", ncol(df_coord) / 2), "f"),
    ncp = Inf,
    axes = c(1, 2)
  )

  ## later
  # write.infile(res.Nap, file.path(exportDir, "res_Nap.csv"),  sep = ";")

  # to obtain better graphes
  plot(res.nap,
    choix = "ind",
    title = "Napping - ..."
  )

  plot(res.nap,
    choix = "var",
    invisible = "ind.sup",
    title = "Napping - ..."
  )

  ####   Clustering analysis based on coordinates of products ####

  res.HCPC <-
    FactoMineR::HCPC(
      res = res.nap,
      nb.clust = -1,
      consol = TRUE,
      iter.max = 100,
      metric = "euclidean",
      method = "centroid",
      proba = 0.05
    )

  ## later
  # write.infile(res.HCPC, file.path(exportDir, "res_hcpc.csv"),  sep = ";")
}

SensoMineR::nappeplot(
  donnee = df_coord,
  numr = 3,
  numc = 4
)

# SensoMineR::indscal(matrice = df_coord,
#                     matrice.illu = df_words_raw,
#                     maxit = 100)

res <- SensoMineR::indscal(
  matrice = df_coord,
  matrice.illu = df_words_cleaned,
  maxit = 100
)

FactoMineR::prefpls(donnee = cbind.data.frame(res$points, df_coord), choix = "ind")

FactoMineR::prefpls(donnee = cbind.data.frame(res$points, df_words_cleaned), choix = "var")
