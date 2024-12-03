start <- Sys.time()

pkgload::load_all()

message("This program treats flash profiles results.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

source(file = "paths.R")
source(file = "params.R")

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
cluster_profile <- readxl::read_xlsx(
  path = xls,
  sheet = 4
)

cluster_profile_pivoted <- cluster_profile |>
  tidytable::mutate(ProductName = ProductName |>
    as.character()) |>
  tidytable::group_by(CJ, ProductName) |>
  tidytable::select(-tidytable::where(is.logical)) |>
  tidytable::pivot_longer(cols = tidytable::where(is.numeric)) |>
  tidytable::filter(!is.na(value)) |>
  tidytable::group_by(CJ) |>
  tidytable::mutate(J = tidytable::cur_group_id()) |>
  tidytable::ungroup() |>
  tidytable::add_count(J) |>
  tidytable::mutate(descriptors = n / (nrow(
    cluster_profile |> tidytable::distinct(ProductName)
  ))) |>
  tidytable::select(ProductName, J, CJ, descriptors, name, value) |>
  tidytable::arrange(J) |>
  tidytable::mutate(name_2 = harmonize_terms(
    x = name,
    dictionary = dictionary_specific_path
  )) |>
  tidytable::separate_longer_delim(cols = "name_2", delim = " ") |>
  tidytable::separate_wider_delim(cols = "name_2", delim = "_") |>
  tidytable::mutate(newName = harmonize_terms(
    x = name_21,
    dictionary = dictionary_generic_path
  )) |>
  tidytable::distinct(ProductName, J, newName, .keep_all = TRUE) |>
  tidytable::group_by(ProductName, newName) |>
  tidytable::add_count() |>
  tidytable::ungroup() |>
  tidytable::filter(n >= MIN_PANELISTS)

if (iHaveNoTime != TRUE) {
  cluster_profile_pivoted_mfa <- cluster_profile_pivoted |>
    tidytable::filter(descriptors > 1)

  cluster_profile_pivoted_distinct <- cluster_profile_pivoted_mfa |>
    tidytable::distinct(J, ProductName, newName, .keep_all = TRUE)

  attributes <- cluster_profile_pivoted_distinct$descriptors

  cluster_profile_pivoted_wide <- cluster_profile_pivoted_mfa |>
    tidytable::select(ProductName, J, newName, value) |>
    tidytable::arrange(newName) |>
    tidytable::mutate(newcol = paste0(newName, "_", J)) |>
    tidytable::distinct(ProductName, newcol, value) |>
    tidyr::pivot_wider(names_from = newcol, values_from = value) |>
    data.frame()

  panelists <- nrow(cluster_profile_pivoted_wide)

  rownames(cluster_profile_pivoted_wide) <-
    cluster_profile_pivoted_wide$ProductName

  cluster_profile_pivoted_wide <- cluster_profile_pivoted_wide |>
    tidytable::select(-ProductName)

  ## alternative
  cluster_profile_pivoted_wide_2 <- cluster_profile_pivoted |>
    tidytable::select(ProductName, newName, value) |>
    tidytable::pivot_wider(
      names_from = newName,
      values_from = value,
      values_fn = sum
    ) |>
    data.frame()

  rownames(cluster_profile_pivoted_wide_2) <-
    cluster_profile_pivoted_wide_2$ProductName

  cluster_profile_pivoted_wide_2 <- cluster_profile_pivoted_wide_2 |>
    tidytable::select(-ProductName)

  res.MFA <-
    FactoMineR::MFA(
      base = cluster_profile_pivoted_wide,
      group = c(rep(
        1, ncol(cluster_profile_pivoted_wide)
      )),
      type = c(rep(
        "s", ncol(cluster_profile_pivoted_wide)
      )),
      ncp = Inf,
      axes = c(1, 2)
    )

  res.MFA.2 <-
    FactoMineR::MFA(
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
  # FactoMineR::write.infile(res.MFA, file.path(exportDir, "res_PF_cheese.csv"),  sep = ";")

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

  res.HCPC <- FactoMineR::HCPC(res.MFA, nb.clust = -1, proba = 0.10)

  res.HCPC.2 <- FactoMineR::HCPC(res.MFA.2, nb.clust = -1, proba = 0.10)

  ## later
  # FactoMineR::write.infile(res.HCPC, file.path(exportDir, "res_hcpc.csv"),  sep = ";")
}

# if (CLUSTER != "All") {
#   write_tsv(
#     x = cluster_profile_pivoted,
#     file = file.path(
#       analysis_path_04_output,
#       paste0(
#         "profile_",
#         "cluster_",
#         CLUSTER,
#         ".tsv"
#       )
#     )
#   )
# } else {
#   write_tsv(
#     x = cluster_profile_pivoted,
#     file = file.path(
#       analysis_path_04_output,
#       paste0(
#         "FullProfile_",
#         "all_clusters",
#         ".tsv"
#       )
#     )
#   )
# }
