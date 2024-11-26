library(stringi)

#' Title
#'
#' @param dictionary
#' @param x
#'
#' @return
#' @export
#'
#' @examples
clean_terms <-
  function(dictionary = dictionary_path, x) {
    dictionary <- read_tsv(file = dictionary) %>%
      mutate(n = str_count(original)) %>%
      arrange(desc(n))

    string <- toupper(x)
    pattern <- paste0("\\b", dictionary$original, "\\b")
    replacement <- dictionary$translated
    if (!is.null(dictionary$translated_simple)) {
      replacement <- dictionary$translated_simple
    }

    replaced <- stri_replace_all_regex(
      str = string,
      pattern = pattern,
      replacement = replacement,
      case_insensitive = FALSE,
      vectorize_all = FALSE
    )

    return(replaced)
  }

#' Title
#'
#' @param dictionary
#' @param x
#'
#' @return
#' @export
#'
#' @examples
clean_terms_2 <-
  function(dictionary = dictionary_path, x) {
    dictionary <- read_tsv(file = dictionary) %>%
      mutate(n = str_count(original)) %>%
      arrange(desc(n))

    string <- toupper(x)
    pattern <- dictionary$original
    replacement <- dictionary$translated
    if (!is.null(dictionary$translated_simple)) {
      replacement <- dictionary$translated_simple
    }

    replaced_NA <- stri_replace_all_regex(
      str = string,
      pattern = pattern,
      replacement = replacement,
      case_insensitive = FALSE,
      vectorize_all = FALSE
    )

    replaced_final <- cbind(string, replaced_NA) %>%
      data.frame() %>%
      mutate(final = if_else(
        condition = !is.na(replaced_NA),
        true = replaced_NA,
        false = string
      ))

    return(replaced_final$final)
  }
