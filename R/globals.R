utils::globalVariables(
  c(
    "best_candidate_3",
    "chasselas",
    "color",
    "concentration",
    "concentration_afc",
    "consensus_3",
    "consistency_3",
    "correct.responses",
    "correct_percent",
    "correlation",
    "count",
    "CJ",
    "data",
    "Date",
    "delta",
    "feature_id",
    "fraction",
    "fractions",
    "fractions.x",
    "fractions.y",
    "fractions_list",
    "from",
    "group",
    "groups",
    "id",
    "id_ion",
    "id_taste",
    "ids",
    "inchikey_2D",
    "intensity",
    "intensity_ion",
    "intensity_new",
    "intensity_taste",
    "judge",
    "jury",
    "label",
    "m",
    "mass",
    "median",
    "method",
    "mz",
    "n",
    "name",
    "name_21",
    "napping_descriptors",
    "newName",
    "newValue",
    "non_na_count",
    "NMRExperiment",
    "original",
    "output",
    "output_session",
    "p_adjusted",
    "p_value",
    "product",
    "product_1before",
    "product_2after",
    "ProductName",
    "profiles",
    "Produit",
    "rowname",
    "rt",
    "score_final",
    "score_mixed",
    "session",
    "size",
    "smiles_2D",
    "stars",
    "sum_name",
    "sum_taste",
    "taste",
    "taste_harmonized",
    "taste_intermediate",
    "taste_original",
    "time",
    "to",
    "Total.responses",
    "afc_correct",
    "afc_total",
    "value",
    "value_2",
    "value_31",
    "value_32",
    "value_4",
    "value_5",
    "values"
  )
)

.cascade_check_export_dir <- utils::getFromNamespace(
  "check_export_dir",
  "cascade"
)
.cascade_keep_best_candidates <- utils::getFromNamespace(
  "keep_best_candidates",
  "cascade"
)
.cascade_make_confident <- utils::getFromNamespace("make_confident", "cascade")
.cascade_prepare_comparison <- utils::getFromNamespace(
  "prepare_comparison",
  "cascade"
)
.cascade_make_other <- utils::getFromNamespace("make_other", "cascade")
.cascade_no_other <- utils::getFromNamespace("no_other", "cascade")
.cascade_prepare_hierarchy <- utils::getFromNamespace(
  "prepare_hierarchy",
  "cascade"
)
.cascade_prepare_plot <- utils::getFromNamespace("prepare_plot", "cascade")
.cascade_preprocess_chromatograms <- utils::getFromNamespace(
  "preprocess_chromatograms",
  "cascade"
)
.cascade_load_chromatograms <- utils::getFromNamespace(
  "load_chromatograms",
  "cascade"
)
.cascade_load_name <- utils::getFromNamespace("load_name", "cascade")
