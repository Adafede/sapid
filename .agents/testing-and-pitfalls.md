# Testing, Linting, and Known Pitfalls

## Critical Pitfalls Discovered in Audit

1. **`LazyData: TRUE` + large gitignored `data/`**:
   - Developers may have ~1.24 GB of untracked lab artifacts under `data/` (`correlations.tsv`, `extract_mzmine/`, `fractions_mzmine/`, `figures/`, `interim/`, `processed/`).
   - `R CMD INSTALL .`, `pkgload::load_all(".")`, `lintr::lint_package(".")`, and `covr::package_coverage(".")` scan `data/` directly and hang.
   - Fix: run `R CMD build` first (`.Rbuildignore` drops untracked `data/` subdirs) or copy tracked package sources + `data/*.rda` into `mktemp -d` before running `INSTALL`, `roxygen2::roxygenise()`, `lintr`, or `covr`.

2. **`lintr::lint_package()` needs an installed `sapid` namespace**:
   - Running `lintr::lint_package()` when `sapid` is not importable in `R_LIBS_USER` produces ~209 false-positive `object_usage_linter` warnings (`paired`, `harmonize_terms`, `.cascade_*`).
   - Fix: install the built tarball into `$TMP/lib`, prepend `$TMP/lib` to `.libPaths()` via `R_LIBS_USER`, unpack the tarball to `$TMP/pkg/sapid`, copy `.lintr` into `$TMP/pkg/sapid/.lintr`, and lint `$TMP/pkg/sapid`.

3. **Static global binding check (`codetools` vs `R CMD check`)**:
   - Raw `codetools::checkUsagePackage("sapid")` inspects attached `stats`/`graphics` (`median`, `stars`, `time`) and ignores `utils::globalVariables()` unless explicitly passed.
   - Use `tools:::.check_code_usage_in_package("sapid", lib.loc = "$TMP/lib")`, which matches `R CMD check` (clean namespace without attached `graphics`/`stats`).

4. **macOS `zsh` glob safety**:
   - Never run unguarded `rm -f Rplots*` in `zsh`: `nomatch` aborts the whole chained command when zero files match. Use `find . -maxdepth 3 -name 'Rplots*' -delete`.

5. **Non-ASCII / mojibake checks on `.rda` datasets**:
   - `inst/tinytest/test_datasets.R` validates encoding invariants across all 10 `data/*.rda` files (`dictionary_*`, `profiles`, etc.), including checking for replacement characters (`\ufffd`) and half-width katakana mojibake (`\uff61-\uff9f`). Keep `.rda` objects pure UTF-8 (see `data-raw/`).

6. **`spelling::spell_check_package()` tokens**:
   - Quarto inline partial-word bolding (e.g. `**S**trategy`) splits words into fragments in GFM/Quarto outputs and pollutes `inst/WORDLIST`. Write whole words plainly.

7. **Git pre-commit checks (`prek.toml`)**:
   - Always pass an explicit config (`prek run <hook> --files ...` or `prek validate-config prek.toml`). Without a path, `prek validate-config` emits a warning.
   - `.Rbuildignore` must remain sorted according to `file-contents-sorter`.

## Commit and Definition-of-Done Checklist

- Never commit `.agent-work/`, `data/` large artifacts, `Rplots*.pdf`, or `.Rhistory`/`.RData`/`.rds` files.
- Commit agent-facing updates (`AGENTS.md`, `.agents/*`) separately from runtime code and human documentation; never add AI trailers to commit messages.
- Before claiming done after code or test edits:
  - `air format . --check` exits `0`
  - `jarl check .` passes
  - Tarball `lintr::lint_package()` returns `0` lints
  - `tools:::.check_code_usage_in_package()` returns `0` lines
  - `tinytest::test_package("sapid")` passes `100%` with `0` leaked `Rplots*` files
  - `spelling::spell_check_package(".")` returns `0` rows
  - `R CMD build --no-manual` + `R CMD check --no-manual` reports `Status: OK`
