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
#' @details
#' Following standard Napping + Ultra-Flash Profiling methodology (Pagès, 2005),
#' the 2D tablecloth coordinates of each panelist are analyzed as active
#' unstandardized continuous blocks (`type = "c"`), while the descriptor word
#' contingency table is projected as a supplementary standardized block
#' (`type = "s"`, `num.group.sup = n_coord_groups + 1L`). Hierarchical
#' clustering on principal components ([FactoMineR::HCPC()]) uses Ward's
#' minimum-variance criterion (`method = "ward"`) to avoid centroid reversals.
#'
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:topic_2269 Statistics and probability
#' @concept EDAM:operation_3432 Clustering
#' @concept EDAM:operation_0337 Visualisation
#' @encoding UTF-8
#' @param input_coordinates Optional path or `data.frame` of napping coordinates
#'   (defaults to built-in [napping_coordinates]).
#' @param input_descriptors Optional path or `data.frame` of napping descriptors
#'   (defaults to built-in [napping_descriptors]).
#' @param sessions Integer session number to analyze (default `2`).
#' @param output Optional PDF file path to capture all diagnostic plots. When
#'   `NULL` in non-interactive sessions, plots are rendered to a temporary PDF
#'   device that is removed on exit so no stray `Rplots*.pdf` files are left
#'   in the working directory.
#'
#' @return Named list with elements `pmfa`, `mfa`, `hcpc`, and `indscal`
#'   (invisibly).
#' @export
#' @examples
#' \dontrun{
#' plot_napping(sessions = 2)
#' }
plot_napping <- function(
  input_coordinates = NULL,
  input_descriptors = NULL,
  sessions = 2,
  output = NULL
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
  devs_before <- grDevices::dev.list()
  tmp_pdf <- NULL
  use_sink_pdf <- !is.null(output) || !interactive()
  if (use_sink_pdf) {
    old_dev_opt <- options(device = function(...) invisible(NULL))
    on.exit(options(old_dev_opt), add = TRUE)
  }
  on.exit(
    {
      devs_now <- grDevices::dev.list()
      extra_devs <- rev(setdiff(devs_now, devs_before))
      for (d in extra_devs) {
        try(grDevices::dev.off(d), silent = TRUE)
      }
      if (!is.null(tmp_pdf) && file.exists(tmp_pdf)) {
        unlink(tmp_pdf)
      }
    },
    add = TRUE
  )
  if (!is.null(output)) {
    .cascade_check_export_dir(dirname(output))
    grDevices::pdf(file = output, width = 10, height = 8)
  } else if (!interactive()) {
    tmp_pdf <- tempfile(fileext = ".pdf")
    grDevices::pdf(file = tmp_pdf, width = 10, height = 8)
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

  # Clean descriptor contingency block

  words_cleaned <- FactoMineR::textual(
    tab = file_text_cleaned,
    maj.in.min = TRUE,
    sep.word = " ",
    num.text = 4,
    contingence.by = 1
  )
  df_words_cleaned <- words_cleaned$cont.table |>
    data.frame()

  df_coord <- df_coord[,
    colSums(is.na(df_coord)) < nrow(df_coord),
    drop = FALSE
  ]

  df_words_cleaned <- df_words_cleaned[,
    colSums(df_words_cleaned) != 0,
    drop = FALSE
  ]

  nap_tot <- cbind(df_coord, df_words_cleaned)

  # PMFA
  res_pmfa <- SensoMineR::pmfa(
    matrice = df_coord,
    matrice.illu = df_words_cleaned,
    graph.mfa = FALSE
  )

  # MFA with supplementary descriptor frequencies
  n_coord_groups <- ncol(df_coord) / 2
  res_nap <- FactoMineR::MFA(
    base = nap_tot,
    group = c(rep(2, n_coord_groups), ncol(df_words_cleaned)),
    type = c(rep("c", n_coord_groups), "s"),
    num.group.sup = n_coord_groups + 1L,
    ncp = Inf,
    axes = c(1, 2)
  )

  # HCPC on product coordinates
  res_hcpc <- FactoMineR::HCPC(
    res = res_nap,
    nb.clust = -1,
    consol = TRUE,
    iter.max = 100,
    metric = "euclidean",
    method = "ward",
    proba = 0.05
  )

  SensoMineR::nappeplot(
    donnee = df_coord,
    numr = 3,
    numc = 4
  )

  res_indscal <- SensoMineR::indscal(
    matrice = df_coord,
    matrice.illu = df_words_cleaned,
    maxit = 100
  )

  FactoMineR::prefpls(
    donnee = cbind.data.frame(res_indscal$points, df_coord),
    choix = "ind"
  )

  FactoMineR::prefpls(
    donnee = cbind.data.frame(res_indscal$points, df_words_cleaned),
    choix = "var"
  )
  invisible(list(
    pmfa = res_pmfa,
    mfa = res_nap,
    hcpc = res_hcpc,
    indscal = res_indscal
  ))
}
