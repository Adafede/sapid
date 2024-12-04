#' Plot napping
#'
#' @param input_coordinates Input coordinates
#' @param input_descriptors Input descriptors
#' @param session Session
#'
#' @return NULL
#'
#' @examples NULL
plot_napping <- function(input_coordinates = system.file("extdata", "napping_coordinates.tsv", package = "sapid"),
                         input_descriptors = system.file("extdata", "napping_descriptors.tsv", package = "sapid"),
                         sessions = 2) {
  df_coord <- input_coordinates |>
    tidytable::fread() |>
    tidytable::filter(session == paste0("session_", sessions |>
      stringi::stri_pad(pad = "0", width = 2))) |>
    data.frame()

  rownames(df_coord) <- df_coord$fraction

  df_coord <- df_coord |>
    dplyr::select(-fraction, -session)

  table_words_prepared <- input_descriptors |>
    tidytable::fread() |>
    tidytable::filter(session == paste0("session_", sessions |>
      stringi::stri_pad(pad = "0", width = 2)))

  file_text_cleaned <- table_words_prepared |>
    tidytable::distinct(fraction, session, jury, taste_intermediate)

  file_text_raw <- table_words_prepared |>
    tidytable::distinct(fraction, session, jury, taste_original)

  words_cleaned <- FactoMineR::textual(
    tab = file_text_cleaned,
    maj.in.min = TRUE,
    sep.word = " ",
    num.text = 4,
    contingence.by = 1
  )
  df_words_cleaned <- words_cleaned$cont.table |>
    data.frame()

  # words_raw <- FactoMineR::textual(
  #   tab = file_text_raw,
  #   maj.in.min = TRUE,
  #   sep.word = " ",
  #   num.text = 4,
  #   contingence.by = 1
  # )
  # df_words_raw <- words_raw$cont.table |>
  #   data.frame()

  # if (sessions == 8) {
  #   df_coord <- df_coord |>
  #     tidytable::filter(rownames(df_coord) != 23 &
  #       rownames(df_coord) != 32) |>
  #     as.data.frame()
  #
  #   df_words_cleaned <- df_words_cleaned |>
  #     tidytable::filter(rownames(df_words_cleaned) != 23 &
  #       rownames(df_words_cleaned) != 32) |>
  #     data.frame()
  #
  #   df_words_raw <- df_words_raw |>
  #     tidytable::filter(rownames(df_words_raw) != 23 &
  #       rownames(df_words_raw) != 32) |>
  #     data.frame()
  # }

  df_coord <-
    df_coord[, colSums(is.na(df_coord)) < nrow(df_coord)]

  df_words_cleaned <-
    df_words_cleaned[, colSums(df_words_cleaned) != 0]

  # df_words_raw <-
  #   df_words_raw[, colSums(df_words_raw) != 0]

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
