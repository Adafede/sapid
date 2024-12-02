#' Prepare chasselas
#'
#' @param input_dir Input dir
#' @param output_dir output_dir
#' @param sessions Sessions
#'
#' @return NULL
#'
#' @examples NULL
prepare_chasselas <-
  function(input_dir = "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory",
           output_dir = "~/switchdrive/SAPERE/03_analysis/04_fractions-sensory/03_output",
           sessions = seq(1, 7)) {
    sessions_infos <- sessions |>
      furrr::future_map(.f = get_session_info)

    chasselas <- sessions_infos |>
      furrr::future_map(
        .f = function(session_info) {
          list.files(
            path =
              file.path(
                input_dir,
                paste0(session_info$date, "_cluster", session_info$cluster),
                "03_files"
              ),
            pattern = ".xlsx",
            full.names = TRUE
          ) |>
            readxl::read_xlsx(sheet = 1) |>
            tidytable::mutate(
              ProductName = tidytable::if_else(
                condition = ProductName == session_info$product_name,
                true = paste("1_avant_cluster", session_info$cluster, sep = "_"),
                false = paste("2_après_cluster", session_info$cluster, sep = "_")
              )
            ) |>
            data.frame()
        }
      ) |>
      tidytable::bind_rows()

    chasselas_pivoted <- chasselas |>
      tidytable::group_by(CJ, ProductName) |>
      tidytable::pivot_longer(tidytable::where(is.numeric)) |>
      tidytable::ungroup() |>
      tidytable::group_by(CJ) |>
      tidytable::mutate(CJ = paste0(
        "jury_",
        tidytable::cur_group_id() |>
          stringi::stri_pad(pad = "0", width = 2)
      )) |>
      data.frame()

    chasselas_wide <- chasselas_pivoted |>
      tidytable::group_by(CJ, name) |>
      tidytable::pivot_wider(names_from = ProductName, values_from = value)

    before <- tidytable::coalesce(chasselas_wide[, 4:(4 + length(sessions))])
    after <- tidytable::coalesce(chasselas_wide[, (4 + length(sessions)):(4 +
      2 * length(sessions) - 1)])

    colnames_after <- colnames(chasselas_wide)[(4 + length(sessions)):(4 +
      2 * length(sessions) - 1)]

    chasselas_deltas <- chasselas_wide
    chasselas_deltas[, "delta"] <- after - before

    chasselas_deltas <- chasselas_deltas |>
      tidytable::filter(!is.na(delta)) |>
      tidytable::distinct(Date, CJ, name, delta)

    results <- list(chasselas_pivoted, chasselas_deltas)

    tidytable::fwrite(
      x = results[[1]],
      file = file.path(output_dir, "chasselas_prepared.tsv"),
      sep = "\t"
    )
    tidytable::fwrite(
      x = results[[2]],
      file = file.path(output_dir, "deltas_prepared.tsv"),
      sep = "\t"
    )
  }
