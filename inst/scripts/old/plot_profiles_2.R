start <- Sys.time()

pkgload::load_all()

message("This program prepares flash profiles results.")
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
cluster_profile <- readxl::read_xlsx(path = xls, sheet = 4)

input_dir <- "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory"
sessions <- seq(1, 7)

session_infos <- sessions |>
  purrr::map(.f = get_session_info)

table_profile <- session_infos |>
  purrr::map(
    .f = load_session,
    input_dir = input_dir,
    tab = "profiles"
  )

cluster_profile <- table_profile |>
  tidytable::bind_rows()

cluster_profile_pivoted <- cluster_profile |>
  tidytable::mutate(ProductName = ProductName |>
    as.character()) |>
  tidytable::group_by(CJ, ProductName) |>
  tidytable::select(-tidytable::where(is.logical)) |>
  tidytable::pivot_longer(cols = tidytable::where(is.numeric)) |>
  tidytable::filter(!is.na(value)) |>
  tidytable::arrange(CJ) |>
  tidytable::group_by(CJ) |>
  tidytable::mutate(CJ = paste0(
    "jury_",
    tidytable::cur_group_id() |>
      stringi::stri_pad(pad = "0", width = 2)
  )) |>
  tidytable::ungroup() |>
  tidytable::select(ProductName, session, CJ, name, value) |>
  tidytable::mutate(name_2 = harmonize_terms(x = name, dictionary = dictionary_specific_path)) |>
  tidytable::separate_longer_delim(cols = "name_2", delim = " ") |>
  tidytable::separate_wider_delim(cols = "name_2", delim = "_") |>
  tidytable::mutate(newName = harmonize_terms(x = name_21, dictionary = dictionary_generic_path)) |>
  tidytable::distinct(ProductName, CJ, newName, .keep_all = TRUE) |>
  tidytable::group_by(ProductName, name) |>
  ## problem due to FIZZ export
  ## replace by the median of correct values or divide by 10
  tidytable::mutate(median = tidytable::coalesce(value / 10, median(value[value <= 10]))) |>
  tidytable::ungroup() |>
  tidytable::mutate(value = tidytable::if_else(
    condition = value > 10,
    true = median,
    false = value
  )) |>
  ## Due to dilutions
  ## Session 03 was diluted 50 times
  ## Session 07 was concentrated 2 times
  tidytable::mutate(
    value = tidytable::if_else(
      condition = session == "session_03",
      true = 50 * value,
      false = value
    )
  ) |>
  tidytable::mutate(
    value = tidytable::if_else(
      condition = session == "session_07",
      true = 0.5 * value,
      false = value
    )
  ) |>
  tidytable::select(
    fraction = ProductName,
    session = session,
    jury = CJ,
    taste_original = name,
    taste_harmonized = newName,
    value = value
  ) |>
  tidytable::distinct() |>
  tidytable::arrange(fraction)


MIN_PANELISTS <- 2


cluster_profile_pivoted_mfa <- cluster_profile_pivoted |>
  tidytable::filter(n >= MIN_PANELISTS) |>
  tidytable::filter(descriptors > 0)

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
    group = rep(1, ncol(cluster_profile_pivoted_wide)),
    type = rep("s", ncol(cluster_profile_pivoted_wide)),
    ncp = Inf,
    axes = c(1, 2)
  )

res.MFA.2 <-
  FactoMineR::MFA(
    base = cluster_profile_pivoted_wide_2,
    group = rep(1, ncol(cluster_profile_pivoted_wide_2)),
    type = rep("s", ncol(cluster_profile_pivoted_wide_2)),
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
