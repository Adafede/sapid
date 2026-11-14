# Architecture and Invariants

## Module Layout

| Domain | Files in `R/` | Notes |
| --- | --- | --- |
| Package & docs | `sapid-package.R`, `data.R`, `colors.R`, `globals.R` | Attach hook, roxygen dataset docs, 7 color/label objects, internal helpers + 35 NSE globals |
| Dictionaries & session I/O | `harmonize_terms.R`, `harmonize_terms_df.R`, `get_session_info.R`, `load_session.R` | Word-boundary harmonization, FIZZ/napping metadata lookup, base `.xlsx` XML parser |
| Sensory prep & filtering | `prepare_profiles.R`, `prepare_napping.R`, `prepare_chasselas.R`, `prepare_concentration.R`, `load_consistent_profiles.R` | Raw `.xlsx` -> clean TSV tables; jury consistency filtering |
| Sensory & psychometric plots | `plot_profiles.R`, `plot_napping.R`, `plot_chasselas_variation.R`, `plot_chasselas_modulation.R`, `plot_concentration.R`, `plot_descriptors_network.R`, `plot_informed_tasting.R`, `plot_matrices.R`, `geom_sigmoid.R` | `ggplot2` visualizations, FactoMineR/SensoMineR napping plots, 3-AFC log-logistic psychometric curves |
| LC-MS / CAD / NMR | `prepare_chemical_profiles.R`, `correlate_ion_taste_intensities.R`, `plot_correlations.R`, `cluster_nmr.R`, `plot_chromatograms.R` | MZmine/CAD/TIMA prep, ion-taste correlation windows, Bruker NMR Hellinger clustering |

## Internal Helpers (`R/globals.R`)

- `utils::globalVariables()`: 35 NSE symbols. Keep `"median"`, `"stars"`, and `"time"` registered alongside data-table/ggplot column names so clean-session `R CMD check` emits no binding notes.
- `.cascade_*` wrappers (`11` functions): resolved lazily via `utils::getFromNamespace(..., "cascade")`. Do not inline broken fallbacks or drop tests mocking them unless `cascade` API changes.
- Base `.xlsx` reader: `read_xlsx_base()`, `resolve_xlsx_sheet_file()`, `parse_xlsx_date_styles()`, `format_excel_serial_date()`, `unescape_xml()`.
- Statistical & plotting helpers: `fit_afc_psychometric()`, `sign_test_pvalue()`, `pratt_wilcox_pvalue()`, `order_numeric_labels()`, `fct_rev_base()`, `fct_reorder_base()`, `load_dataset_or_file()`, `prepare_ion_intensities_df()`, `prepare_taste_intensities_df()`.

## Data & Behavior Invariants (Do Not Break)

1. **Unicode word harmonization (`R/harmonize_terms.R`)**:
   - Dictionaries use UTF-8 (`dictionary_generic` / `dictionary_specific`: columns `original`, `translated`, `translated_simple`; `dictionary_napping`: `original`, `translated`).
   - `mode = "word"` must sort patterns by decreasing `nchar`, filter empty/`NA` originals, escape PCRE metacharacters, and match with `(*UCP)` delimiter lookarounds `(?<=^|[^\\p{L}\\p{N}_])...(?=$|[^\\p{L}\\p{N}_])` plus `\x01...\x02` two-pass placeholders. Plain `\b` breaks accented French terms (`ACIDITÉ`, `BOISÉ`, `FRUITÉ`, `SALÉ`) and leading hyphens.
2. **Custom library package attach (`R/sapid-package.R`)**:
   - `.onAttach()` must pass `lib.loc = libname` to `utils::citation(pkgname, lib.loc = libname)` wrapped in `tryCatch()`. Without `lib.loc`, loading from `mktemp -d` or custom libraries fails.
3. **Spreadsheet parsing & ID protection (`R/globals.R`, `R/load_session.R`, `R/prepare_chasselas.R`)**:
   - `read_xlsx_base()` resolves sheets through `xl/_rels/workbook.xml.rels`, formats date cells via `styles.xml`, strips `<rPh>` phonetic runs, and tracks column letters even on sparse cells or empty `<row/>` nodes.
   - `load_session()` and `prepare_chasselas()` must exclude metadata columns (`CJ`, `Date`, `Code`, `Session`) from numeric descriptor pivots.
4. **FIZZ scaling & deduplication (`R/prepare_profiles.R`)**:
   - Out-of-range FIZZ values (`value > 10`) are rescaled (`value / 10` or median of valid `<= 10` scores) before deduplication.
   - `dedup_key` uses `RAW:<name>` when `newName` is `NA` and `HARM:<newName>` otherwise so distinct unmapped descriptors never collapse together.
5. **Greiner correlation idempotence (`R/correlate_ion_taste_intensities.R`, `R/plot_correlations.R`)**:
   - When `greiner_kendall = TRUE` transforms Kendall's $\tau$ via $\sin(\frac{\pi}{2}\tau)$, `correlate_ion_taste_intensities()` records `"kendall_greiner"` in the `method` column. `plot_correlations()` must inspect `method` and skip transforming rows already tagged `"kendall_greiner"`.
6. **Napping graphics device isolation (`R/plot_napping.R`)**:
   - Always open an explicit output or temporary PDF device and override `options(device = function(...) invisible(NULL))` inside `on.exit()` cleanup when non-interactive (or saving to `output`). `FactoMineR`/`SensoMineR` otherwise call `dev.new()` and leak `Rplots*.pdf`.
7. **Numeric fraction ordering (`R/globals.R:order_numeric_labels`, `R/plot_profiles.R`, `R/plot_correlations.R`)**:
   - Order `"fraction_2"` before `"fraction_10"` numerically via `order_numeric_labels()`, not lexicographically.
8. **Backward-compatible unused parameters**:
   - `plot_profiles()` keeps unused `*_extract` parameters and `prepare_ion_intensities_df()` keeps `imputation_factor = NULL` to avoid breaking callers; do not remove them without a deprecation cycle, and document them as ignored.
