#' Analyze napping
#'
#' @return NULL
#'
#' @examples NULL
analyze_napping <- function() {
  input_dir <- "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory"
  dictionary_generic_path <- "inst/extdata/dictionary_generic.tsv"

  dictionary_napping_path <- "inst/extdata/dictionary_napping.tsv"

  dictionary_specific_path <- "inst/extdata/dictionary_specific.tsv"
  sessions <- 6
  session_infos <- sessions |>
    furrr::future_map(.f = get_session_info)
  tables_words <- session_infos |>
    furrr::future_map(
      .f = load_session,
      input_dir = input_dir,
      tab = "napping_words"
    )

  tables_words_harmonized <- tables_words |>
    furrr::future_map(
      .f = harmonize_terms_df,
      dictionary_generic_path = dictionary_generic_path,
      dictionary_napping_path = dictionary_napping_path,
      dictionary_specific_path = dictionary_specific_path
    )

  tables_words_raw <- tables_words |>
    furrr::future_map(
      .f = function(x) {
        x |>
          tidytable::pivot_longer(2:ncol(x)) |>
          tidytable::filter(!is.na(value)) |>
          tidytable::separate_longer_delim(cols = "value", delim = " ") |>
          tidytable::filter(!is.na(value)) |>
          tidytable::filter(value != "")
      }
    )

  file_text_cleaned <- tables_words_harmonized |>
    tidytable::bind_rows()
  file_text_raw <- tables_words_raw |>
    tidytable::bind_rows()

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
  tables_coord <- session_infos |>
    furrr::future_map(
      .f = load_session,
      input_dir = input_dir,
      tab = "napping_coord"
    )

  df_coord <- tables_coord |>
    tidytable::bind_rows() |>
    data.frame()

  rownames(df_coord) <- df_coord$Produit

  df_coord <- df_coord |>
    dplyr::select(-Produit)

  # if (SESSION == 8) {
  #   df_coord <- df_coord |>
  #     tidytable::filter(rownames(df_coord) != 23 &
  #       rownames(df_coord) != 32) ## discarded
  #
  #   df_words_cleaned <- df_words_cleaned |>
  #     tidytable::filter(rownames(df_words_cleaned) != 23 &
  #       rownames(df_words_cleaned) != 32) ## discarded
  #
  #   df_words_raw <- df_words_raw |>
  #     tidytable::filter(rownames(df_words_raw) != 23 &
  #       rownames(df_words_raw) != 32) ## discarded
  # }

  df_coord <-
    df_coord[, colSums(is.na(df_coord)) < nrow(df_coord)]

  df_words_cleaned <-
    df_words_cleaned[, colSums(df_words_cleaned) != 0]

  df_words_raw <-
    df_words_raw[, colSums(df_words_raw) != 0]


  nap.tot <- cbind(df_coord, df_words_cleaned)

  ##### PMFA (Procuste Multiple Factor Analysis to obtain individual tableclothes  ####
  single.nap <- SensoMineR::pmfa(
    matrice = df_coord,
    matrice.illu = df_words_cleaned,
    graph.mfa = FALSE
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
  dev.off()
  res.nap <- FactoMineR::MFA(
    base = nap.tot,
    group = c(rep(2, ncol(df_coord) / 2), ncol(df_words_cleaned)),
    type = c(rep("c", ncol(df_coord) / 2), "s"),
    axes = c(1, 2)
  )

  ##### MFA with word frequencies   ####
  dev.off()
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
  # plot(res.nap, choix = "ind", title = "Napping - ...")
  #
  # plot(res.nap,
  #   choix = "var",
  #   invisible = "ind.sup",
  #   title = "Napping - ..."
  # )

  ####   Clustering analysis based on coordinates of products ####

  dev.off()
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
  dev.off()
  SensoMineR::nappeplot(
    donnee = df_coord,
    numr = 3,
    numc = 4
  )

  # SensoMineR::indscal(matrice = df_coord,
  #                     matrice.illu = df_words_raw,
  #                     maxit = 100)

  dev.off()
  res <- SensoMineR::indscal(
    matrice = df_coord,
    matrice.illu = df_words_cleaned,
    maxit = 100
  )

  dev.off()
  FactoMineR::prefpls(donnee = cbind.data.frame(res$points, df_coord), choix = "ind")

  dev.off()
  FactoMineR::prefpls(
    donnee = cbind.data.frame(res$points, df_words_cleaned),
    choix = "var"
  )
}
