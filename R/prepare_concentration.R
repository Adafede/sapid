#' Prepare concentration
#'
#' @param input_xlsx Input xlsx
#' @param output Output
#'
#' @return NULL
#'
#' @examples NULL
prepare_concentration <- function(input_xlsx = "~/switchdrive/SAPERE/02_raw-data/inhouse/02_sensory/20210329_raw-extract/03_files/20210329_raw-extract.xlsx",
                                  output = "~/git/sapid/inst/extdata/concentration_afc.tsv") {
  message("Loading original files...\n")
  Pipol <- input_xlsx |>
    readxl::read_xlsx(sheet = 4) |>
    tidytable::mutate(concentration = concentration |>
      round(digits = 2) |>
      format(nsmall = 2) |>
      factor()) |>
    data.frame()
  AFC_Pipol <- input_xlsx |>
    readxl::read_xlsx(sheet = 3) |>
    tidytable::mutate(concentration = concentration |>
      round(digits = 2) |>
      format(nsmall = 2)) |>
    data.frame()

  message("Loading cleaned files...\n")
  cleaned <- input_xlsx |>
    readxl::read_xlsx(sheet = 5) |>
    tidytable::mutate(concentration = concentration |>
      round(digits = 2) |>
      format(nsmall = 2)) |>
    data.frame()

  message("Joining data together \n")
  joined <- Pipol |>
    tidytable::left_join(AFC_Pipol) |>
    tidytable::distinct(
      judge,
      concentration,
      intensity,
      correct.responses,
      Total.responses
    )

  message("Counting terms \n")
  prepared <- cleaned |>
    tidytable::pivot_longer(cols = tidytable::contains("attribut")) |>
    tidytable::mutate(value = gsub(
      pattern = "_.*$",
      replacement = "",
      x = value
    )) |>
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
    tidytable::mutate(jury = paste0(
      "jury_",
      tidytable::cur_group_id() |>
        stringi::stri_pad(pad = "0", width = 2)
    ))

  prepared |>
    tidytable::fwrite(file = output, sep = "\t")
  return(output)
}
