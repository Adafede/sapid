# Contributing to sapid

## Prerequisites

- R `>= 4.5.0`
- [Quarto CLI](https://quarto.org/) (required to render vignettes and `README.md`)
- [air](https://posit-dev.github.io/air/) (R formatter, configured via `.air.toml`)
- [jarl](https://jarl.etiennebacher.com/) (R linter)
- Optionally [prek](https://prek.j178.dev/) to run the local hook suite in `prek.toml`

Install package dependencies (CRAN, Bioconductor `AlpsNMR`, and GitHub remotes `adafede/cascade` and `thomasp85/ggraph`) from `r-universe` or via `pak`:

```r
install.packages(
  c("tinytest", "lintr", "spelling", "roxygen2", "quarto", "grateful"),
  repos = "https://cloud.r-project.org"
)

options(repos = c(
  adafede = "https://adafede.r-universe.dev",
  cran = "https://cran.r-universe.dev",
  bioc = "https://bioc.r-universe.dev"
))
```

Or with `pak` from a source tree containing `DESCRIPTION`:

```r
pak::local_install_deps(dependencies = TRUE)
```

## Building and Running Tests Locally

`sapid` uses `LazyData: TRUE` in `DESCRIPTION`. If your working copy contains large research files under `data/` (for example, raw NMR directories, `.mzML` extracts, or multi-hundred-MB `correlations.tsv` files ignored by Git), running `R CMD INSTALL .` or `pkgload::load_all(".")` directly in the repository root will stall while scanning those files.

Always build a source tarball first (which applies `.Rbuildignore` exclusions) and install into a temporary library directory:

```sh
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

R CMD build --no-build-vignettes --no-manual .
mv sapid_*.tar.gz "$TMP/"
mkdir -p "$TMP/lib"
R CMD INSTALL --no-multiarch -l "$TMP/lib" "$TMP"/sapid_*.tar.gz

R_LIBS_USER="$TMP/lib:$(Rscript -e 'cat(paste(.libPaths(), collapse = ":"))')" \
  Rscript -e 'tinytest::test_package("sapid")'
```

All tests in `inst/tinytest/` should pass without creating `Rplots.pdf` files in the working directory.

## Formatting, Linting, and Static Checks

Run the formatter and linter before committing:

```sh
air format . --check
jarl check .
```

To run `lintr` accurately, `sapid` must first be installed in `R_LIBS_USER` so `object_usage_linter` resolves internal package helpers and datasets instead of reporting false positives. Unpack the built tarball into the temporary directory alongside `.lintr`:

```sh
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

R CMD build --no-build-vignettes --no-manual .
mv sapid_*.tar.gz "$TMP/"
mkdir -p "$TMP/lib" "$TMP/pkg"
R CMD INSTALL --no-multiarch -l "$TMP/lib" "$TMP"/sapid_*.tar.gz
tar -xzf "$TMP"/sapid_*.tar.gz -C "$TMP/pkg"
cp .lintr "$TMP/pkg/sapid/.lintr"

EXISTING_LIBS=$(Rscript -e 'cat(paste(.libPaths(), collapse = ":"))')
R_LIBS_USER="$TMP/lib:$EXISTING_LIBS" Rscript -e '
  lints <- lintr::lint_package("'"$TMP"'/pkg/sapid")
  print(lints)
  quit(status = length(lints) > 0L)
'
```

Check static code usage against `NAMESPACE` and `R/globals.R`:

```sh
Rscript -e '
  out <- tools:::.check_code_usage_in_package("sapid", lib.loc = "'"$TMP"'/lib")
  if (length(out) > 0L) { writeLines(out); quit(status = 1L) }
'
```

## Documentation and Spellcheck

When `data/` contains large untracked research directories, run `roxygen2::roxygenise()` on a temporary copy that includes only tracked package sources (`DESCRIPTION`, `NAMESPACE`, `R/`, `man/`, and `.rda` files in `data/`), then sync updated `NAMESPACE` and `man/*.Rd` files back.

Keep `README.md` synchronized with `README.qmd`:

```sh
EXISTING_LIBS=$(Rscript -e 'cat(paste(.libPaths(), collapse = ":"))')
R_LIBS_USER="$TMP/lib:$EXISTING_LIBS" quarto render README.qmd --to gfm
```

Verify spelling (updating `inst/WORDLIST` only for valid domain terminology, author names, and identifiers) and run full `R CMD check`:

```sh
Rscript -e '
  res <- spelling::spell_check_package(".")
  print(res)
  quit(status = nrow(res) > 0L)
'

R CMD build --no-manual .
R CMD check --no-manual sapid_*.tar.gz
```

## Pull Request Checklist

- Add or update regression tests under `inst/tinytest/` for behavior changes or bug fixes.
- Ensure `air format . --check`, `jarl check .`, `lintr::lint_package()`, `tinytest::test_package("sapid")`, and `spelling::spell_check_package(".")` report zero issues.
- Re-render `README.md` if `README.qmd` changed and regenerate `man/*.Rd` / `NAMESPACE` if roxygen comments changed.
- Verify `R CMD check --no-manual` finishes with zero errors, warnings, or notes.
