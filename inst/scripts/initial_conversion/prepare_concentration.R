start <- Sys.time()

pkgload::load_all()

message("This program prepares working concentration determination.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

#' Prepare concentration
#'
#' @param input_xlsx Input xlsx
#' @param output Output
#'
#' @return NULL
#'
#' @examples NULL
prepare_concentration <- function(
  input_xlsx = "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory/20210329_raw-extract/03_files/20210329_raw-extract.xlsx",
  output = "inst/extdata/concentration_afc.tsv"
) {
  pipol <- input_xlsx |>
    readxl::read_xlsx(sheet = 4) |>
    tidytable::mutate(
      concentration = concentration |>
        round(digits = 2) |>
        format(nsmall = 2) |>
        factor()
    ) |>
    data.frame()

  pipol_afc <- input_xlsx |>
    readxl::read_xlsx(sheet = 3) |>
    tidytable::mutate(
      concentration = concentration |>
        round(digits = 2) |>
        format(nsmall = 2)
    ) |>
    data.frame()

  cleaned <- input_xlsx |>
    readxl::read_xlsx(sheet = 5) |>
    tidytable::mutate(
      concentration = concentration |>
        round(digits = 2) |>
        format(nsmall = 2)
    ) |>
    data.frame()

  joined <- pipol |>
    tidytable::left_join(pipol_afc) |>
    tidytable::distinct(
      judge,
      concentration,
      intensity,
      correct.responses,
      Total.responses
    )

  cleaned |>
    tidytable::pivot_longer(cols = tidytable::contains("attribut")) |>
    tidytable::mutate(
      value = gsub(
        pattern = "_.*$",
        replacement = "",
        x = value
      )
    ) |>
    tidytable::filter(!is.na(value)) |>
    tidytable::inner_join(joined) |>
    tidytable::select(
      concentration,
      jury = judge,
      taste = value,
      value = intensity,
      afc_correct = correct.responses,
      afc_total = Total.responses
    ) |>
    tidytable::arrange(jury) |>
    tidytable::group_by(jury) |>
    tidytable::mutate(
      jury = paste0(
        "jury_",
        tidytable::cur_group_id() |>
          stringi::stri_pad(pad = "0", width = 2)
      )
    ) |>
    tidytable::fwrite(file = output, sep = "\t")

  return(output)
}

prepare_concentration()

end <- Sys.time()

message("Script finished in ", format(end - start))
