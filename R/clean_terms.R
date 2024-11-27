#' Clean terms
#'
#' @param dictionary Dictionary
#' @param x X
#'
#' @return NULL
#' @export
#'
#' @examples NULL
clean_terms <-
  function(dictionary = dictionary_path, x) {
    dictionary <- dictionary |>
      tidytable::fread() |>
      tidytable::mutate(n = original |>
        stringi::stri_length()) |>
      tidytable::arrange(tidytable::desc(n))

    string <- toupper(x)
    pattern <- paste0("\\b", dictionary$original, "\\b")
    replacement <- dictionary$translated
    if (!is.null(dictionary$translated_simple)) {
      replacement <- dictionary$translated_simple
    }

    replaced <- stringi::stri_replace_all_regex(
      str = string,
      pattern = pattern,
      replacement = replacement,
      case_insensitive = FALSE,
      vectorize_all = FALSE
    )

    return(replaced)
  }

#' Clean terms 2
#'
#' @param dictionary Dictionary
#' @param x X
#'
#' @return NULL
#' @export
#'
#' @examples NULL
clean_terms_2 <-
  function(dictionary = dictionary_path, x) {
    dictionary <- dictionary |>
      tidytable::fread() |>
      tidytable::mutate(n = original |>
        stringi::stri_length()) |>
      tidytable::arrange(tidytable::desc(n))

    string <- toupper(x)
    pattern <- dictionary$original
    replacement <- dictionary$translated
    if (!is.null(dictionary$translated_simple)) {
      replacement <- dictionary$translated_simple
    }

    replaced_NA <- stringi::stri_replace_all_regex(
      str = string,
      pattern = pattern,
      replacement = replacement,
      case_insensitive = FALSE,
      vectorize_all = FALSE
    )

    replaced_final <- cbind(string, replaced_NA) |>
      data.frame() |>
      tidytable::mutate(final = tidytable::if_else(
        condition = !is.na(replaced_NA),
        true = replaced_NA,
        false = string
      ))

    return(replaced_final$final)
  }
