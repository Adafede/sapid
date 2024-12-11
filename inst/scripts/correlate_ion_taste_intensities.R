start <- Sys.time()

pkgload::load_all()

message("This program calculates ion/taste intensities correlations.")
message("Authors: \n", "AR")
message("Contributors: \n", "...")

correlate_ion_taste_intensities <- function(input_ions = "~/Documents/papers/sapid/sapere_tmp/fractions_mzmine/fractions.csv",
                                            input_tastes = system.file("extdata", "profiles.tsv", package = "sapid"),
                                            output = "inst/extdata/correlations.tsv",
                                            min_jury = 2L,
                                            imputation_factor = 0.5,
                                            widths = 5:9) {
  df_ion_intensities <- input_ions |>
    tidytable::fread() |>
    tidytable::distinct(id, rt, mz, contains(":area")) |>
    tidytable::pivot_longer(contains(":area")) |>
    tidytable::select(-mz, -rt) |>
    tidytable::mutate(
      name = gsub(
        pattern = "datafile:210619_AR_[0-9]{2}_M_",
        replacement = "fraction_",
        x = gsub(
          pattern = "_01.mzML:area",
          replacement = "",
          x = name,
          fixed = TRUE
        )
      )
    ) |>
    tidytable::mutate(value = value |>
      tidytable::replace_na(imputation_factor * min(value, na.rm = TRUE))) |>
    tidytable::select(
      fraction = name,
      id_ion = id,
      intensity_ion = value
    ) |>
    tidytable::distinct() |>
    tidytable::mutate(fraction = fraction |>
      gsub(
        pattern = "fraction_",
        replacement = "",
        fixed = TRUE
      ) |>
      as.integer()) |>
    tidytable::arrange(fraction)

  df_taste_intensities <- input_tastes |>
    load_consistent_profiles(min_jury = min_jury) |>
    tidytable::mutate(tidytable::across(
      tidytable::everything(),
      .fns = function(x) {
        tidytable::if_else(condition = x == 0,
          true = NA,
          false = x
        )
      }
    )) |>
    tidytable::mutate(value = value |>
      tidytable::replace_na(imputation_factor * min(value, na.rm = TRUE))) |>
    tidytable::group_by(fraction, taste) |>
    tidytable::mutate(median = value |>
      median()) |>
    tidytable::ungroup() |>
    tidytable::select(fraction, id_taste = taste, intensity_taste = median) |>
    tidytable::distinct() |>
    tidytable::mutate(fraction = fraction |>
      gsub(
        pattern = "fraction_",
        replacement = "",
        fixed = TRUE
      ) |>
      as.integer()) |>
    tidytable::arrange(fraction)

  fractions <- df_ion_intensities$fraction |>
    unique()
  tastes <- df_taste_intensities$id_taste |>
    unique()

  generate_rolling_windows <- function(fractions, window_size) {
    seq_len(length(fractions) - window_size + 1) |>
      furrr::future_map(
        .f = function(fractions, window_size, i) {
          fractions[i:(i + window_size - 1)]
        },
        fractions = fractions,
        window_size = window_size
      )
  }

  auto_correlate <- function(x, y) {
    if (!is.numeric(x) || !is.numeric(y)) {
      stop("Both x and y need to be numeric vectors.")
    }

    if (sum(is.finite(x)) < 2 || sum(is.finite(y)) < 2) {
      warning("Not enough finite observations for correlation. Returning NA.")
      return(list(
        method = NA,
        correlation = NA,
        p.value = NA
      ))
    }

    if (length(unique(x[is.finite(x)])) <= 1 ||
      length(unique(y[is.finite(y)])) <= 1) {
      warning("All values in x or y are identical. Defaulting to Kendall correlation.")
      method <- "kendall"
    } else if (length(x) < 3 || length(y) < 3) {
      warning("Sample size too small for Shapiro-Wilk test. Defaulting to Kendall correlation.")
      method <- "kendall"
    } else if (length(x) > 5000 || length(y) > 5000) {
      warning("Sample size too large for Shapiro-Wilk test. Defaulting to Kendall correlation.")
      method <- "kendall"
    } else {
      if (stats::shapiro.test(x)$p.value > 0.05 &&
        stats::shapiro.test(y)$p.value > 0.05) {
        method <- "pearson"
      } else {
        method <- "kendall"
      }
    }

    result <- stats::cor.test(x = x, y = y, method = method)

    return(list(
      method = method,
      correlation = result$estimate,
      p.value = result$p.value
    ))
  }

  correlate_intensities <- function(taste,
                                    fractions_lists,
                                    df_ion_intensities,
                                    df_taste_intensities) {
    results_list <- fractions_lists |>
      furrr::future_map(
        function(taste,
                 fractions_list,
                 df_taste_intensities,
                 df_ion_intensities) {
          df <- df_taste_intensities |>
            tidytable::filter(id_taste == taste) |>
            tidytable::inner_join(df_ion_intensities |>
              tidytable::filter(fraction %in% fractions_list)) |>
            tidytable::select(-id_taste)

          if (nrow(df) == 0) {
            warning(paste(
              "No data found for fractions_list:",
              paste(fractions_list, collapse = ", ")
            ))
            return(NULL)
          }

          df <- df |>
            tidytable::pivot_wider(names_from = id_ion, values_from = intensity_ion) |>
            tidytable::select(-1) |>
            data.frame()

          if (nrow(df) == 0) {
            warning(paste(
              "Data frame is empty after pivot for fractions_list:",
              paste(fractions_list, collapse = ", ")
            ))
            return(NULL)
          }

          correlations <- df[2:ncol(df)] |>
            as.list() |>
            furrr::future_map(
              .f = function(ion_intensity,
                            taste_intensity = df$intensity_taste) {
                auto_correlate(x = ion_intensity, y = taste_intensity)
              }
            )

          cor_summary <- data.frame(
            correlation = correlations |>
              furrr::future_map_dbl(function(x) {
                x$correlation
              }),
            p_value = correlations |>
              furrr::future_map_dbl(function(x) {
                x$p.value
              }),
            method = correlations |>
              furrr::future_map_chr(function(x) {
                x$method
              })
          )

          cor_summary$p_adjusted <- cor_summary$p_value |>
            stats::p.adjust(method = "BH")
          cor_summary$fractions <- fractions_list |>
            paste(collapse = " ")
          cor_summary$id_ion <- rownames(cor_summary) |>
            gsub(
              pattern = "X",
              replacement = "",
              fixed = TRUE
            ) |>
            as.integer()

          return(cor_summary)
        },
        df_ion_intensities = df_ion_intensities,
        df_taste_intensities = df_taste_intensities,
        taste = taste
      )

    empty_results <- tidytable::tidytable(
      correlation = NA_real_,
      p_value = NA_real_,
      method = NA_character_,
      p_adjusted = NA_real_,
      fractions = NA_character_,
      id_ion = NA_integer_
    )
    filtered_results <- results_list[results_list |>
      furrr::future_map_lgl(
        .f = function(x) {
          !is.null(x)
        }
      )]

    if (length(filtered_results) == 0) {
      filtered_results <- empty_results
    }
    return(filtered_results |>
      tidytable::bind_rows())
  }

  fractions_lists <- widths |>
    furrr::future_map(
      .f = function(fractions, size) {
        generate_rolling_windows(fractions, size)
      },
      fractions = fractions
    ) |>
    unlist(recursive = FALSE)

  # fractions_lists <- list(seq(32, 39))
  # fractions_lists <- fractions_lists[160:170]
  # tastes <- c("BITTER", "VOLUME", "FRESH")

  results <- tastes |>
    furrr::future_map(
      .f = function(taste,
                    fractions_lists,
                    df_ion_intensities,
                    df_taste_intensities) {
        correlate_intensities(
          taste = taste,
          fractions_lists = fractions_lists,
          df_ion_intensities = df_ion_intensities,
          df_taste_intensities = df_taste_intensities
        ) |>
          tidytable::mutate(id_taste = taste)
      },
      fractions_lists = fractions_lists,
      df_ion_intensities = df_ion_intensities,
      df_taste_intensities = df_taste_intensities
    ) |>
    tidytable::bind_rows() |>
    tidytable::filter(!is.na(p_adjusted)) |>
    tidytable::select(
      id_ion,
      id_taste,
      fractions,
      correlation,
      p_value,
      p_adjusted,
      method
    ) |>
    tidytable::distinct()

  results |>
    tidytable::fwrite(file = output, sep = "\t")

  return(output)
}
correlate_ion_taste_intensities()

end <- Sys.time()

message("Script finished in ", format(end - start))
