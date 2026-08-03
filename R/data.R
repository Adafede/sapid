#' Sapid Package Datasets
#'
#' This page documents the built-in datasets in the `sapid` package,
#' which provide sensory panel data, sample metadata, and reference
#' dictionaries for taste-characterization workflows.
#' All datasets originate from or are derived from the Swertia chirayita
#' case study published in \doi{10.1016/j.crfs.2025.101043},
#' with raw data archived at https://zenodo.org/records/14616396.
#'
#' @format NULL
#' @name sapid-data
NULL

#' Chasselas Sensory Panel Taste Ratings
#'
#' A dataset of sensory panel ratings for Chasselas wine samples,
#' collected during multiple sessions with multiple jurors.
#'
#' @format A data frame with 1,314 rows and 6 columns:
#' \describe{
#'   \item{date}{ISO 8601 timestamp of the rating session}
#'   \item{jury}{Juror identifier (e.g., "jury_01")}
#'   \item{product}{Product identifier ("product_1before", "product_2after")}
#'   \item{session}{Session identifier (e.g., "session_06")}
#'   \item{taste}{Taste attribute or descriptor (e.g., "sourness", "bitterness")}
#'   \item{value}{Numerical rating value for the attribute}
#' }
#'
#' @source \doi{10.1016/j.crfs.2025.101043}; Zenodo \url{https://zenodo.org/records/14616396}
#'
#' @examples
#' data(chasselas)
#' head(chasselas)
#' table(chasselas$taste)
"chasselas"

#' Concentration AFC (Ascending Forced Choice) Test Results
#'
#' Results from an Ascending Forced Choice (AFC) sensory test
#' with concentration-dependent taste evaluation.
#' Includes AFC accuracy metrics (number correct out of total trials).
#'
#' @format A data frame with 76 rows and 6 columns:
#' \describe{
#'   \item{concentration}{Concentration level of the tested compound}
#'   \item{jury}{Juror identifier (e.g., "jury_01")}
#'   \item{taste}{Taste attribute tested (e.g., "amer", "acide")}
#'   \item{value}{Numerical rating or intensity value}
#'   \item{afc_correct}{Number of correct responses in the AFC trial}
#'   \item{afc_total}{Total number of trials in the AFC session}
#' }
#'
#' @source \doi{10.1016/j.crfs.2025.101043}; Zenodo \url{https://zenodo.org/records/14616396}
#'
#' @examples
#' data(concentration_afc)
#' head(concentration_afc)
#' summary(concentration_afc)
"concentration_afc"

#' Sensory Profiles with Harmonized Taste Descriptors
#'
#' The primary sensory panel dataset containing taste profile ratings
#' for Swertia chirayita sample fractions. Includes original (as entered
#' by panelists) and harmonized (standardized) taste descriptor terms.
#' This is the main table for sensory-MS correlation analyses.
#'
#' @format A data frame with 1,665 rows and 6 columns:
#' \describe{
#'   \item{fraction}{Fraction identifier (e.g., "fraction_17")}
#'   \item{session}{Session identifier (e.g., "session_01")}
#'   \item{jury}{Juror identifier (e.g., "jury_03")}
#'   \item{taste_original}{Original taste descriptor as entered by panelist}
#'   \item{taste_harmonized}{Standardized/harmonized taste descriptor}
#'   \item{value}{Numerical rating or intensity score}
#' }
#'
#' @details
#' Missing or empty harmonized values indicate descriptors that could not
#' be consistently mapped across panelists or were deemed non-informative
#' (e.g., "RIEN" meaning "nothing").
#' Use \code{!is.na(taste_harmonized) & taste_harmonized != ""} to filter
#' for valid descriptors in analyses.
#'
#' @source \doi{10.1016/j.crfs.2025.101043}; Zenodo \url{https://zenodo.org/records/14616396}
#'
#' @examples
#' data(profiles)
#' head(profiles)
#' table(profiles$taste_harmonized)[table(profiles$taste_harmonized) > 10]
"profiles"

#' Napping Session Coordinates
#'
#' Coordinates assigned to sample fractions during napping sessions,
#' recorded by individual jurors (panelists). Each juror positioned samples
#' on a 2D plane; coordinates represent their sensory perception of similarity.
#'
#' @format A data frame with 64 rows and multiple columns:
#' \describe{
#'   \item{fraction}{Fraction identifier (e.g., "fraction_17")}
#'   \item{session}{Session identifier (e.g., "session_01")}
#'   \item{J01X, J01Y, J02X, J02Y, ...}{Napping coordinates for each juror;
#'     column pairs (JxxX, JxxY) represent X and Y positions assigned by
#'     juror Jxx}
#' }
#'
#' @details
#' Napping is a projective sensory method where panelists position samples
#' on a 2D plane based on perceived similarity. This produces a sensory map
#' that can be analyzed via correspondence analysis or PCA.
#'
#' @source \doi{10.1016/j.crfs.2025.101043}; Zenodo \url{https://zenodo.org/records/14616396}
#'
#' @examples
#' data(napping_coordinates)
#' head(napping_coordinates)
#' colnames(napping_coordinates)
"napping_coordinates"

#' Napping Session Descriptors
#'
#' Taste descriptors and attributes assigned to sample fractions
#' during napping sessions. Each descriptor was provided by a juror
#' for a specific fraction, with original, intermediate, and harmonized forms.
#'
#' @format A data frame with 1,146 rows and 6 columns:
#' \describe{
#'   \item{fraction}{Fraction identifier (e.g., "fraction_17")}
#'   \item{session}{Session identifier (e.g., "session_01")}
#'   \item{jury}{Juror identifier (e.g., "J1")}
#'   \item{taste_original}{Original descriptor as written by juror}
#'   \item{taste_intermediate}{Intermediate standardization step}
#'   \item{taste_harmonized}{Final harmonized descriptor}
#' }
#'
#' @details
#' Napping descriptors complement napping coordinates. While coordinates
#' represent spatial positioning of similarities, descriptors provide
#' explicit sensory attribute labels that jurors associate with each sample.
#'
#' @source \doi{10.1016/j.crfs.2025.101043}; Zenodo \url{https://zenodo.org/records/14616396}
#'
#' @examples
#' data(napping_descriptors)
#' head(napping_descriptors)
#' unique(napping_descriptors$taste_harmonized)
"napping_descriptors"

#' Swertia chirayita Sample Fractionation Hierarchy
#'
#' Hierarchical record of sample preparation and fractionation of
#' Swertia chirayita, documenting parent-child relationships and mass
#' (in milligrams) at each step. Useful for tracking sample lineage
#' and mass balance through extraction and purification.
#'
#' @format A data frame with 94 rows and 3 columns:
#' \describe{
#'   \item{parent}{Parent fraction identifier (NA for root material)}
#'   \item{label}{Child fraction or processing step identifier}
#'   \item{mass}{Mass in milligrams at this stage}
#' }
#'
#' @details
#' The hierarchy starts with raw plant material and initial solvents (EtOH, water)
#' and traces through extraction, decanting, drying, and sub-fractionation steps
#' leading to final analytic samples (V_01, V_02, etc.).
#'
#' @source \doi{10.1016/j.crfs.2025.101043}; Zenodo \url{https://zenodo.org/records/14616396}
#'
#' @examples
#' data(fractions)
#' head(fractions)
#' # View the hierarchy structure
#' subset(fractions, !is.na(parent))
"fractions"

#' Sample Grouping and Cluster Assignments
#'
#' Cluster or group assignments for sample fractions, including
#' group identifiers (typically color codes used in visualizations)
#' and corresponding sample row identifiers.
#'
#' @format A data frame with 54 rows and 2 columns:
#' \describe{
#'   \item{group}{Group identifier, often a color code (e.g., "#e31a1c")}
#'   \item{rowname}{Sample or fraction row identifier (e.g., "17")}
#' }
#'
#' @details
#' Used for coloring plots and visualizations based on clustering
#' or hierarchical analysis results. Typically derived from dendrogram
#' or other unsupervised classification of sample sensory profiles.
#'
#' @source \doi{10.1016/j.crfs.2025.101043}; Zenodo \url{https://zenodo.org/records/14616396}
#'
#' @examples
#' data(groups)
#' head(groups)
#' unique(groups$group)
"groups"

#' Generic Taste Terms Dictionary
#'
#' A reference dictionary for mapping taste descriptors to standardized
#' terms. Includes original, translated, and simplified versions.
#' Designed for generic/widely-used sensory attributes.
#'
#' @format A data frame with 64 rows and 3 columns:
#' \describe{
#'   \item{original}{Original or raw descriptor (often in French)}
#'   \item{translated}{English translation}
#'   \item{translated_simple}{Simplified/harmonized English form}
#' }
#'
#' @details
#' Used by data harmonization functions (e.g., \code{harmonize_terms()})
#' to standardize taste terminology across panelists and sessions.
#'
#' @source \doi{10.1016/j.crfs.2025.101043}; Zenodo \url{https://zenodo.org/records/14616396}
#'
#' @examples
#' data(dictionary_generic)
#' head(dictionary_generic)
#' # Example lookup
#' subset(dictionary_generic, original == "BOISÉ")
"dictionary_generic"

#' Napping-Specific Taste Terms Dictionary
#'
#' A reference dictionary specifically for napping session terminology,
#' mapping napping-specific descriptors to standardized terms.
#'
#' @format A data frame with 45 rows and 2 columns:
#' \describe{
#'   \item{original}{Original napping descriptor}
#'   \item{translated}{Standardized descriptor}
#' }
#'
#' @details
#' Napping methods often generate unique terminology or abbreviations
#' (e.g., "_LEGER" for "light"). This dictionary maps these to
#' harmonized forms used in broader analyses.
#'
#' @source \doi{10.1016/j.crfs.2025.101043}; Zenodo \url{https://zenodo.org/records/14616396}
#'
#' @examples
#' data(dictionary_napping)
#' head(dictionary_napping)
#' # Find mappings for "LEGER"
#' subset(dictionary_napping, grepl("LEGER", original))
"dictionary_napping"

#' Specific Taste Terms Dictionary
#'
#' A reference dictionary for specific or less-common taste descriptors,
#' including mappings to English translations and simplified forms.
#'
#' @format A data frame with 196 rows and 3 columns:
#' \describe{
#'   \item{original}{Original specific descriptor (often in French)}
#'   \item{translated}{English translation}
#'   \item{translated_simple}{Simplified/harmonized English form}
#' }
#'
#' @details
#' Larger and more specialized than \code{dictionary_generic}.
#' Used for comprehensive harmonization of all descriptors in the dataset.
#'
#' @source \doi{10.1016/j.crfs.2025.101043}; Zenodo \url{https://zenodo.org/records/14616396}
#'
#' @examples
#' data(dictionary_specific)
#' head(dictionary_specific)
#' nrow(dictionary_specific)
"dictionary_specific"
