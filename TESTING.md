# Testing & Quality Assurance Guide for `sapid`

This document describes the testing conventions, coverage verification, static
analysis, and mutation testing workflows for the `sapid` R package.

## 1. Test Architecture

`sapid` uses [`tinytest`](https://cran.r-project.org/package=tinytest) for
lightweight, zero-overhead unit and integration testing.

- **Runner**: `tests/tinytest.R` invokes `tinytest::test_package("sapid")`.
- **Test files**: Located in `inst/tinytest/test_*.R`:
  - `test_datasets.R`: Schema, dimension, column-type, and domain-invariant
    assertions for all 10 exported datasets.
  - `test_core_functions.R`: Unit and boundary tests for `get_session_info()`,
    `harmonize_terms()` (including two-pass sentinel placeholder protection),
    and `load_consistent_profiles()` (including multiplicative RMS panelist
    normalization).
  - `test_globals_and_colors.R`: Unit tests for internal helpers in
    `R/globals.R`, `R/colors.R`, and `R/geom_sigmoid.R` (`fct_rev_base()`,
    `fct_reorder_base()`, `sign_test_pvalue()`, `pratt_wilcox_pvalue()`,
    `fit_afc_psychometric()`, `unescape_xml()`, `read_xlsx_base()`,
    `load_dataset_or_file()`, `prepare_ion_intensities_df()`,
    `prepare_taste_intensities_df()`, `.cascade_keep_best_candidates()`,
    `scale_fill_batlow()`, `scale_color_batlow()`, `label_si_number()`,
    `label_short_scale()`, `sigmoid()`).
  - `test_plot_harmonize_session.R`: Unit and boundary tests for
    `geom_sigmoid()` / `StatSigmoid`, `harmonize_terms_df()`, and
    `load_session()`.
  - `test_prepare_and_napping_paths.R`: End-to-end tests with synthetic `.xlsx`
    fixtures for `prepare_chasselas()`, `prepare_profiles()`,
    `prepare_napping()`, and `plot_napping()`.
  - `test_advanced_coverage.R`: Mathematical and end-to-end verification of
    `prepare_concentration()`, `correlate_ion_taste_intensities()` (verified
    against `stats::cor.test()` for Pearson, exact Kendall LUT, tied Kendall
    normal approximation, Greiner's Pearson-equivalent transformation, and
    window/taste/global BH FDR scopes), `plot_correlations()`,
    `plot_profiles()`, and `prepare_chemical_profiles()`.
  - `test_plots_and_correlations.R`: End-to-end PDF generation and correlation
    pipeline tests (`plot_chasselas_modulation()`, `plot_chasselas_variation()`,
    `plot_concentration()`, `plot_descriptors_network()`,
    `plot_informed_tasting()`, `plot_matrices()`).
  - `test_low_coverage_error_paths.R`: Error-path and synthetic `.mzML` / Bruker
    `1r` NMR integration tests for `plot_chromatograms()` and `cluster_nmr()`.

## 2. Test Design Conventions

1. **No coverage-only tests**: Every test must assert exact expected values,
   structures, or error/warning messages. Executing a function solely to
   increase line coverage without asserting its outcome is forbidden.
2. **Independent expected values**: Compute expected numerical or tabular
   outputs independently (by hand or against reference implementations like
   `stats::cor.test()` or `stats::binom.test()`), never by re-running the
   function under test.
3. **Mutation resistance**:
   - Explicitly test boundary values (`0`, `1`, `n - 1`, `n`, `n + 1`, `10` vs.
     `> 10`, `1e3` / `1e6` / `1e9`).
   - Test both branches of conditionals with inputs that produce distinct
     outputs.
   - Verify exact column names, types, row counts, sorting order, and
     error/warning message patterns.
4. **Filesystem & device isolation**:
   - Write all temporary fixtures (`.xlsx`, `.mzML`, Bruker NMR directories,
     `.tsv`, `.pdf`) inside `tempfile()` directories and clean them up with
     `unlink(..., recursive = TRUE)` or `on.exit(..., add = TRUE)`.
   - Close any graphics devices opened during tests before exiting.

## 3. Running Tests, Coverage, and Checks Locally

### Run the Test Suite

```bash
# Install current source and run all tinytest suites
R CMD INSTALL --no-multiarch --with-keep.source .
Rscript -e 'res <- tinytest::test_all("."); print(summary(res)); stopifnot(all(as.logical(res)))'
```

### Run Line Coverage (`covr`)

```bash
Rscript -e '
cov <- covr::package_coverage()
print(cov)
cat(sprintf("Total coverage: %.2f%%\n", covr::percent_coverage(cov)))
'
```

### Run Formatting and Linting

```bash
# Formatter (Air) and fast AST linter (Jarl)
air format . --check
jarl check .

# R lintr package
Rscript -e 'print(lintr::lint_package("."))'
```

### Run Full `R CMD check`

```bash
Rscript -e 'rcmdcheck::rcmdcheck(".", args = c("--no-manual"), error_on = "warning")'
```

## 4. Mutation Testing

To verify that tests actively detect logic regressions (boundary flips, operator
inversions, dropped branches, altered constants), apply single-site mutations to
functions in `asNamespace("sapid")` and run the corresponding `tinytest` file:

```r
pkgload::load_all(".")
ns <- asNamespace("sapid")

# Example: mutate >= min_jury to > min_jury in load_consistent_profiles()
orig_fn <- get("load_consistent_profiles", envir = ns)
src <- paste(readLines("R/load_consistent_profiles.R"), collapse = "\n")
mut_src <- sub("taste_counts >= min_jury", "taste_counts > min_jury", src, fixed = TRUE)

env <- new.env(parent = ns)
eval(parse(text = mut_src), envir = env)
unlockBinding("load_consistent_profiles", ns)
assign("load_consistent_profiles", env$load_consistent_profiles, envir = ns)
lockBinding("load_consistent_profiles", ns)

res <- tinytest::run_test_file("inst/tinytest/test_core_functions.R", verbose = 0)
stopifnot(!all(as.logical(res))) # Mutant must be killed!

# Restore original function
unlockBinding("load_consistent_profiles", ns)
assign("load_consistent_profiles", orig_fn, envir = ns)
lockBinding("load_consistent_profiles", ns)
```
