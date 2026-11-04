#' Plot napping sensory map and descriptor projections
#'
#' Perform multifactorial sensory map analysis on napping coordinates and
#' associated descriptor word contingencies for a given session using
#' Procrustes Multiple Factor Analysis ([SensoMineR::pmfa()]), Multiple Factor
#' Analysis ([FactoMineR::MFA()]), Hierarchical Clustering on Principal
#' Components ([FactoMineR::HCPC()]), tablecloth plotting
#' ([SensoMineR::nappeplot()]), Individual Differences Scaling
#' ([SensoMineR::indscal()]), and Preference PLS ([FactoMineR::prefpls()]).
#'
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:topic_2269 Statistics and probability
#' @concept EDAM:operation_3432 Clustering
#' @concept EDAM:operation_0337 Visualisation
#' @param input_coordinates Optional path or `data.frame` of napping coordinates
#'   (defaults to built-in [napping_coordinates]).
#' @param input_descriptors Optional path or `data.frame` of napping descriptors
#'   (defaults to built-in [napping_descriptors]).
#' @param sessions Integer session number to analyze (default `2`).
#'
#' @return Result of the final [FactoMineR::prefpls()] call (invisibly).
#' @export
#' @examples
#' \dontrun{
#' plot_napping(sessions = 2)
#' }
plot_napping <- function(
  input_coordinates = system.file(
    "extdata",
    "napping_coordinates.tsv",
    package = "sapid"
  ),
  input_descriptors = system.file(
    "extdata",
    "napping_descriptors.tsv",
    package = "sapid"
  ),
  sessions = 2
) {
  session_tag <- if (
    is.character(sessions) && grepl("^session_", sessions[1L])
  ) {
    sessions[1L]
  } else if (identical(sessions, "All")) {
    "session_All"
  } else {
    paste0("session_", sprintf("%02d", as.integer(sessions[1L])))
  }
  base_dev <- grDevices::dev.cur()
  close_extra_devs <- function() {
    while (grDevices::dev.cur() > max(1L, base_dev)) {
      grDevices::dev.off()
    }
  }

  raw_coord <- load_dataset_or_file(input_coordinates, "napping_coordinates")
  df_coord <- as.data.frame(
    raw_coord[raw_coord$session == session_tag, , drop = FALSE],
    stringsAsFactors = FALSE
  )
  rownames(df_coord) <- gsub(
    "fraction_",
    "",
    as.character(df_coord$fraction),
    fixed = TRUE
  )
  df_coord$fraction <- NULL
  df_coord$session <- NULL

  raw_words <- load_dataset_or_file(input_descriptors, "napping_descriptors")
  table_words_prepared <- raw_words[
    raw_words$session == session_tag,
    ,
    drop = FALSE
  ]

  file_text_cleaned <- unique(
    as.data.frame(
      table_words_prepared[,
        c("fraction", "session", "jury", "taste_intermediate"),
        drop = FALSE
      ],
      stringsAsFactors = FALSE
    )
  )
  file_text_cleaned$fraction <- gsub(
    "fraction_",
    "",
    as.character(file_text_cleaned$fraction),
    fixed = TRUE
  )

  # file_text_raw <- table_words_prepared |>
  #   tidytable::distinct(fraction, session, jury, taste_original)

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

  df_coord <- df_coord[,
    colSums(is.na(df_coord)) < nrow(df_coord),
    drop = FALSE
  ]

  df_words_cleaned <- df_words_cleaned[,
    colSums(df_words_cleaned) != 0,
    drop = FALSE
  ]

  # df_words_raw <-
  #   df_words_raw[, colSums(df_words_raw) != 0]

  nap_tot <- cbind(df_coord, df_words_cleaned)

  ##### PMFA (Procuste Multiple Factor Analysis to obtain individual tableclothes  ####
  SensoMineR::pmfa(
    matrice = df_coord,
    matrice.illu = df_words_cleaned,
    graph.mfa = FALSE
  )

  ###
  ## decide to use this part or not
  # single.nap_high <- single.nap |>
  #   data.frame() |>
  #   tidytable::filter(RV.coeff >= min_rv_coeff)
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
  close_extra_devs()
  res_nap <- FactoMineR::MFA(
    base = nap_tot,
    group = c(rep(2, ncol(df_coord) / 2), ncol(df_words_cleaned)),
    type = c(rep("c", ncol(df_coord) / 2), "s"),
    axes = c(1, 2)
  )

  ##### MFA with word frequencies   ####
  close_extra_devs()
  FactoMineR::MFA(
    base = nap_tot,
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

  close_extra_devs()
  FactoMineR::HCPC(
    res = res_nap,
    nb.clust = -1,
    consol = TRUE,
    iter.max = 100,
    metric = "euclidean",
    method = "centroid",
    proba = 0.05
  )

  ## later
  # write.infile(res.HCPC, file.path(exportDir, "res_hcpc.csv"),  sep = ";")
  close_extra_devs()
  SensoMineR::nappeplot(
    donnee = df_coord,
    numr = 3,
    numc = 4
  )

  # SensoMineR::indscal(matrice = df_coord,
  #                     matrice.illu = df_words_raw,
  #                     maxit = 100)

  close_extra_devs()
  res <- SensoMineR::indscal(
    matrice = df_coord,
    matrice.illu = df_words_cleaned,
    maxit = 100
  )

  close_extra_devs()
  FactoMineR::prefpls(
    donnee = cbind.data.frame(res$points, df_coord),
    choix = "ind"
  )

  close_extra_devs()
  out <- FactoMineR::prefpls(
    donnee = cbind.data.frame(res$points, df_words_cleaned),
    choix = "var"
  )
  close_extra_devs()
  invisible(out)
}
