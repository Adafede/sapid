# Agent Guidelines for `sapid`

R package (`0.0.0.9002`, R `>= 4.5.0`) integrating sensory evaluation with LC-MS, CAD, TIMA, and 1D NMR datasets.

## Hard Rules

- **Never run `R CMD INSTALL .`, `pkgload::load_all(".")`, `covr::package_coverage(".")`, or direct `roxygen2::roxygenise()` in the repo root** when gitignored research data exists under `data/`. `LazyData: TRUE` forces scanning multi-hundred-MB raw/processed directories and stalls. Always build a source tarball first (which applies `.Rbuildignore`) or copy tracked sources + `data/*.rda` to `mktemp -d`.
- **Keep documentation surfaces strictly separated**: never mention `AGENTS.md` or `.agents/` anywhere in `README*`, `CONTRIBUTING.md`, `NEWS.md`, `DESCRIPTION`, `vignettes/`, `man/`, or `R/`.
- **Leave zero plot artifacts**: ensure tests and scripts never leave `Rplots*.pdf` in the repo root (`find . -maxdepth 3 -name 'Rplots*'`).

## Repository Map

- `R/` (27 files, 24 exports in `NAMESPACE`): core package code, datasets (`R/data.R`), package hooks (`R/sapid-package.R`), internal helpers and 35 NSE symbols (`R/globals.R`), 7 color/axis objects (`R/colors.R`).
- `data/`: 10 tracked `.rda` datasets (`chasselas`, `concentration_afc`, `dictionary_generic`, `dictionary_napping`, `dictionary_specific`, `fractions`, `groups`, `napping_coordinates`, `napping_descriptors`, `profiles`) plus optional gitignored local research subdirs.
- `inst/tinytest/` (8 `test_*.R` suites + `fixtures/`), invoked by `tests/tinytest.R`.
- `inst/scripts/` (17 analysis scripts), `vignettes/` (1 vignette + 2 articles), `man/` (34 `.Rd` files).
- Tooling configs: `.air.toml` (`line-width = 80`), `.lintr` (`line_length_linter(120L)`), `prek.toml`, `inst/WORDLIST`.

## Verification Commands

Build and run tests cleanly in a temp library:

```sh
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
R CMD build --no-build-vignettes --no-manual .
mv sapid_*.tar.gz "$TMP/"
mkdir -p "$TMP/lib" "$TMP/pkg"
R CMD INSTALL --no-multiarch -l "$TMP/lib" "$TMP"/sapid_*.tar.gz
EXISTING_LIBS=$(Rscript -e 'cat(paste(.libPaths(), collapse = ":"))')
R_LIBS_USER="$TMP/lib:$EXISTING_LIBS" Rscript -e 'tinytest::test_package("sapid")'
```

Run static checks, `lintr` (requires installed `sapid` on `R_LIBS_USER` + copied `.lintr`), usage checks, spelling, and full `R CMD check`:

```sh
air format . --check
jarl check .
tar -xzf "$TMP"/sapid_*.tar.gz -C "$TMP/pkg" && cp .lintr "$TMP/pkg/sapid/.lintr"
R_LIBS_USER="$TMP/lib:$EXISTING_LIBS" Rscript -e 'stopifnot(length(lintr::lint_package("'"$TMP"'/pkg/sapid")) == 0L)'
Rscript -e 'stopifnot(length(tools:::.check_code_usage_in_package("sapid", lib.loc = "'"$TMP"'/lib")) == 0L)'
Rscript -e 'stopifnot(nrow(spelling::spell_check_package(".")) == 0L)'
```

## Cheapest-First Workflow

1. Edit `R/` -> run `air format . && jarl check .`.
2. Re-sync `man/*.Rd` and `NAMESPACE` via `roxygen2::roxygenise()` on a clean temp copy (tracked files + `data/*.rda`), then copy `man/` and `NAMESPACE` back.
3. Build tarball -> install to `mktemp -d` -> run target `tinytest::run_test_file(...)` before running full `tinytest::test_package("sapid")`.
4. Before finalizing, run `R CMD build --no-manual` + `R CMD check --no-manual` -> require `Status: OK` and `0` `Rplots*` files.

## Deeper Guides

- [.agents/architecture.md](.agents/architecture.md): module boundaries, internal helpers, and runtime invariants.
- [.agents/testing-and-pitfalls.md](.agents/testing-and-pitfalls.md): known audit traps (`LazyData`, `lintr`, `codetools`, zsh globs, encoding) and commit rules.
- Human developer setup and CI prerequisites: [CONTRIBUTING.md](CONTRIBUTING.md).
