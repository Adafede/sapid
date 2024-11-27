message("This script is made to treat FLASH PROFILE results. \n")

start <- Sys.time()

message("Loading ... \n")
message("... packages (and installing them if needed) \n")
if (!require(conflicted)) {
  install.packages("conflicted")
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

message("... paths and parameters \n")
source(file = "paths.R")
source(file = "params.R")

message("... functions \n")
source(file = "r/clean_terms.R")

message("... files ... \n")
xls <- list.files(
  path = file.path(
    data_inhouse_sensory_path,
    paste0(DATE, "_cluster", CLUSTER),
    "03_files"
  ),
  pattern = ".xlsx",
  full.names = TRUE
)

message("... profile \n")
cluster_profile <- read_xlsx(
  path = xls,
  sheet = 4
)

cluster_profile_pivoted <- cluster_profile %>%
  group_by(CJ, ProductName) %>%
  pivot_longer(4:ncol(.)) %>%
  dplyr::filter(!is.na(value)) %>%
  group_by(CJ) %>%
  mutate(J = cur_group_id()) %>%
  ungroup() %>%
  add_count(J) %>%
  mutate(descriptors = n / (nrow(
    cluster_profile %>% distinct(ProductName)
  ))) %>%
  dplyr::select(ProductName, J, CJ, descriptors, name, value) %>%
  arrange(J) %>%
  mutate(name_2 = clean_terms(
    x = name,
    dictionary = dictionary_specific_path
  )) %>%
  cSplit("name_2", sep = " ", direction = "long") %>%
  cSplit("name_2", sep = "_", direction = "wide") %>%
  mutate(newName = clean_terms(
    x = name_2_1,
    dictionary = dictionary_generic_path
  )) |>
  distinct(ProductName, J, newName, .keep_all = TRUE) |>
  group_by(ProductName, newName) |>
  add_count() |>
  dplyr::filter(n >= MIN_PANELISTS)

if (iHaveNoTime != TRUE) {
  cluster_profile_pivoted_mfa <- cluster_profile_pivoted %>%
    dplyr::filter(descriptors > 1)

  cluster_profile_pivoted_distinct <-
    cluster_profile_pivoted_mfa %>%
    distinct(J, .keep_all = TRUE)

  attributes <- cluster_profile_pivoted_distinct$descriptors

  cluster_profile_pivoted_wide <- cluster_profile_pivoted_mfa %>%
    select(ProductName, J, newName, value) %>%
    arrange(newName) %>%
    pivot_wider(names_from = c(newName, J), values_from = value) %>%
    data.frame()

  panelists <- nrow(cluster_profile_pivoted_wide)

  rownames(cluster_profile_pivoted_wide) <-
    cluster_profile_pivoted_wide$ProductName

  cluster_profile_pivoted_wide <-
    cluster_profile_pivoted_wide %>%
    select(-ProductName)

  ## alternative
  cluster_profile_pivoted_wide_2 <- cluster_profile_pivoted %>%
    select(ProductName, newName, value) %>%
    pivot_wider(
      names_from = newName,
      values_from = value,
      values_fn = sum
    ) %>%
    data.frame()

  rownames(cluster_profile_pivoted_wide_2) <-
    cluster_profile_pivoted_wide_2$ProductName

  cluster_profile_pivoted_wide_2 <-
    cluster_profile_pivoted_wide_2 %>%
    select(-ProductName)

  res.MFA <-
    MFA(
      base = cluster_profile_pivoted_wide,
      group = c(rep(
        panelists, ncol(cluster_profile_pivoted_wide)
      )),
      type = c(rep(
        "s", ncol(cluster_profile_pivoted_wide)
      )),
      ncp = 12,
      axes = c(1, 2)
    )

  res.MFA.2 <-
    MFA(
      base = cluster_profile_pivoted_wide_2,
      group = c(rep(
        1, ncol(cluster_profile_pivoted_wide_2)
      )),
      type = c(rep(
        "s", ncol(cluster_profile_pivoted_wide_2)
      )),
      ncp = Inf,
      axes = c(1, 2)
    )

  ## later
  # to export listing
  # write.infile(res.MFA, file.path(exportDir, "res_PF_cheese.csv"),  sep = ";")

  # to obtain better graphes
  plot(
    res.MFA,
    choix = "ind",
    axes = c(1, 2),
    title = "Flash profile - ...",
    cex = 0.8
  )

  plot(
    res.MFA.2,
    choix = "ind",
    axes = c(1, 2),
    title = "Flash profile - ...",
    cex = 0.8
  )

  plot(
    res.MFA,
    choix = "ind",
    partial = "all",
    axes = c(1, 2),
    title = "Flash profile - ...",
    cex = 0.8
  )

  plot(
    res.MFA.2,
    choix = "ind",
    partial = "all",
    axes = c(1, 2),
    title = "Flash profile - ...",
    cex = 0.8
  )

  plot(
    res.MFA,
    choix = "var",
    cex = 0.8,
    axes = c(1, 2),
    title = "Flash profile - ..."
  )

  plot(
    res.MFA.2,
    choix = "var",
    cex = 0.8,
    axes = c(1, 2),
    title = "Flash profile - ..."
  )

  plot(
    res.MFA,
    choix = "group",
    cex = 0.8,
    axes = c(1, 2),
    title = "Flash profile - ..."
  )

  plot(
    res.MFA.2,
    choix = "group",
    cex = 0.8,
    axes = c(1, 2),
    title = "Flash profile - ..."
  )

  ####   Clustering analysis based on coordinates of products ####

  res.HCPC <- HCPC(res.MFA, nb.clust = -1, proba = 0.10)

  res.HCPC.2 <- HCPC(res.MFA.2, nb.clust = -1, proba = 0.10)

  ## later
  # write.infile(res.HCPC, file.path(exportDir, "res_hcpc.csv"),  sep = ";")
}

if (CLUSTER != "All") {
  write_tsv(
    x = cluster_profile_pivoted,
    file = file.path(
      analysis_path_04_output,
      paste0(
        "profile_",
        "cluster_",
        CLUSTER,
        ".tsv"
      )
    )
  )
} else {
  write_tsv(
    x = cluster_profile_pivoted,
    file = file.path(
      analysis_path_04_output,
      paste0(
        "FullProfile_",
        "all_clusters",
        ".tsv"
      )
    )
  )
}
