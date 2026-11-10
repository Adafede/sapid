# sapid

# sapid 0.0.0.9002

## Mathematical & Algorithmic Improvements

- **3-AFC psychometric MLE curve (`fit_afc_psychometric()`,
  `plot_concentration()`)**: Replaced the decorative 2-point sigmoid and
  hardcoded `0.12` mg/L threshold with a guessing-corrected binomial
  log-logistic MLE ($\gamma = 1/3$, Firth log-slope regularization
  $\lambda = 0.05$), delta-method 95% confidence ribbons, and analytical
  $\text{EC}_{50}$ threshold ($0.525\text{ mg/L}$ at $P = 2/3$). Preserved
  individual juror ratings in Panel A violins instead of per-taste means.
- **Pratt's zero-preserving signed-rank test (`pratt_wilcox_pvalue()`,
  `plot_chasselas_modulation()`)**: Replaced the binomial sign test with Pratt's (1959)
  zero-preserving Wilcoxon signed-rank test (`test_method = "pratt"`), retaining
  zero differences during ranking with exact zero- and tie-variance corrections.
- **Greiner's Kendall-to-Pearson transformation, local RT isomer deduplication,
  and Fisher-$z$ desirability scoring (`correlate_ion_taste_intensities()`,
  `plot_correlations()`, `prepare_taste_intensities_df()`)**:
  - Mapped Kendall's $\tau$ to the Pearson-equivalent scale via Greiner's
    relation $r_{\text{eq}} = \sin(\frac{\pi}{2}\tau)$
    (`greiner_kendall = TRUE`).
  - Deduplicated 2D InChIKeys within local retention-time windows
    (`rt_window = 0.5` min) rather than globally across the chromatographic run,
    preserving resolved isomers such as Sweroside (`id_ion = 3640`).
  - Ranked feature-taste candidates by sample-size-adjusted Fisher-$z$
    desirability
    $\text{sign}(r)\tanh(\frac{1}{3}\text{atanh}(|r|)\sqrt{\max(1, n - 3)}) S_{\text{TIMA}}$.
  - Completed the `fraction x taste` grid (`complete_grid = TRUE`) with baseline
    imputation (`imputation_factor * min_v`) and added configurable BH-FDR scope
    (`fdr_scope = c("window", "taste", "global")`).
- **Harmonized consistency filtering and multiplicative RMS panelist scaling
  (`load_consistent_profiles()`, `plot_informed_tasting()`)**: Filtered
  descriptors on harmonized `taste_harmonized` instead of raw `taste_original`
  (recovering `METALLIC` and `CARTON`) and added multiplicative RMS juror
  scaling plus session panel-size weighting (`normalize_panelists = TRUE`).
- **Two-pass sentinel harmonization and synonym aggregation
  (`harmonize_terms()`, `prepare_profiles()`)**: Protected matched dictionary
  spans with sentinel tokens (`\x01<idx>\x02`) to prevent shorter patterns from
  overwriting already-translated substrings, rescaled FIZZ `> 10` export
  artifacts before deduplication, and aggregated synonymous ratings per
  `(ProductName, CJ, newName)` via `max(value, na.rm = TRUE)`.
- **Napping MFA supplementary word block and Ward HCPC (`plot_napping()`)**:
  Projected the descriptor contingency table as a supplementary group
  (`num.group.sup = n_coord_groups + 1L`, `type = "s"`) in `FactoMineR::MFA()`
  and switched `FactoMineR::HCPC()` from `"centroid"` to `"ward"`.
- **Weighted simple network graph (`plot_descriptors_network()`)**: Collapsed
  parallel duplicate edges into edge weights (`weight`) before layout
  computation while preserving total vertex in-strength (`size`).
- **1D proton NMR chemometrics (`cluster_nmr()`)**: Excluded
  `range_exclude_area` prior to total-area normalization, merged detected peak
  positions closer than `peak_width_ppm / 2`, preserved sub-threshold areas as
  `0` for Hellinger + `ward.D2` clustering (`dist_method = "euclidean"`), and
  aligned sorted dendrogram leaf colors to fraction labels via `labels(dend)`.

# sapid 0.0.9001

- Adapt to `tima` updates
- Switched documentation from `pkgdown` to `altdoc`
- Updated minimal R version to `4.4.0` (and related Bioconductor dependencies)

# sapid 0.0.9000

- Initial version.
