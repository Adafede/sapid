

<!-- README.md is generated from README.qmd. Please edit that file -->

# SAPID <img src='https://raw.githubusercontent.com/adafede/sapid/main/man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/adafede/sapid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adafede/sapid/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/adafede/sapid/graph/badge.svg)](https://app.codecov.io/gh/adafede/sapid)
[![r-universe
badge](https://adafede.r-universe.dev/sapid/badges/version?&color=blue&style=classic.png)](https://adafede.r-universe.dev/sapid)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14616395.svg)](https://doi.org/10.5281/zenodo.14616395)
<!-- badges: end -->

A **S**trategy to **A**nalyze **P**lant Extracts Taste **I**n **D**epth.

⚠️ This repository is not maintained and will not be except for extreme
interest. It has just been opened for the sake of transparency.

## Installation

As the package is not (yet) available on CRAN, you will need to install
with:

``` r
install.packages(
  "sapid",
  repos = c(
    "https://adafede.r-universe.dev",
    "https://bioc.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

## Main Citations

### SAPID

Article: <https://doi.org/10.1016/j.crfs.2025.101043>

According to which steps you used, please give credit to the authors of
the tools/resources used.

### mzmine

General: <https://doi.org/10.1038/s41587-023-01690-2>

### SIRIUS

General: <https://doi.org/10.1038/s41592-019-0344-8>

- *CSI:FingerId*: <https://doi.org/10.1073/pnas.1509788112>
- *ZODIAC*: <https://doi.org/10.1038/s42256-020-00234-6>
- *CANOPUS*: <https://doi.org/10.1038/s41587-020-0740-8>
- *COSMIC*: <https://doi.org/10.1038/s41587-021-01045-9>

### LOTUS

General: <https://doi.org/10.7554/eLife.70780>

⚠️ Do not forget to cite which version you used:
<https://doi.org/10.5281/zenodo.5794106>

### ISDB

General: <https://doi.org/10.1021/acs.analchem.5b04804>

⚠️ Do not forget to cite which version you used:
<https://doi.org/10.5281/zenodo.5607185>

### TIMA

General: <https://doi.org/10.3389/fpls.2019.01329>

⚠️ Do not forget to cite which version you used:
<https://doi.org/10.5281/zenodo.5797920>

### Others

- NPClassifier: <https://doi.org/10.1021/acs.jnatprod.1c00399>

## Additional software credits

| Package | Version | Citation |
|:---|:---|:---|
| AlpsNMR | 4.11.0 | Madrid-Gambin et al. (2020) |
| base | 4.5.1 | R Core Team (2025) |
| BiocManager | 1.30.26 | Morgan and Ramos (2025) |
| BiocVersion | 3.22.0 | Morgan (2025) |
| BSDA | 1.2.2 | Arnholt and Evans (2023) |
| cascade | 0.0.0.9001 | Rutz and Wolfender (2023); Rutz (2025) |
| dendextend | 1.19.1 | Galili (2015) |
| FactoMineR | 2.12 | Lê, Josse, and Husson (2008) |
| ggbump | 0.1.0 | Sjoberg (2020) |
| ggpubr | 0.6.1 | Kassambara (2025) |
| ggraph | 2.2.1 | Pedersen (2024) |
| ggrepel | 0.9.6 | Slowikowski (2024) |
| igraph | 2.1.4 | Csardi and Nepusz (2006); Csárdi et al. (2025) |
| khroma | 1.16.0 | Frerebeau (2025) |
| knitr | 1.50 | Xie (2014); Xie (2015); Xie (2025) |
| NMRphasing | 1.0.7 | Jiang (2025) |
| pkgload | 1.4.0 | Wickham et al. (2024) |
| rmarkdown | 2.29 | Xie, Allaire, and Grolemund (2018); Xie, Dervieux, and Riederer (2020); Allaire et al. (2024) |
| scales | 1.4.0 | Wickham, Pedersen, and Seidel (2025) |
| SensoMineR | 1.28 | Husson, Le, and Cadoret (2025) |
| stringi | 1.8.7 | Gagolewski (2022) |
| testthat | 3.2.3 | Wickham (2011) |
| tidytable | 0.11.2 | Fairbanks (2024) |
| tidyverse | 2.0.0 | Wickham et al. (2019) |
| treemapify | 2.5.6 | Wilkins (2023) |

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-rmarkdown2024" class="csl-entry">

Allaire, JJ, Yihui Xie, Christophe Dervieux, Jonathan McPherson, Javier
Luraschi, Kevin Ushey, Aron Atkins, et al. 2024.
*<span class="nocase">rmarkdown</span>: Dynamic Documents for r*.
<https://github.com/rstudio/rmarkdown>.

</div>

<div id="ref-BSDA" class="csl-entry">

Arnholt, Alan T., and Ben Evans. 2023. *BSDA: Basic Statistics and Data
Analysis*. <https://doi.org/10.32614/CRAN.package.BSDA>.

</div>

<div id="ref-igraph2006" class="csl-entry">

Csardi, Gabor, and Tamas Nepusz. 2006. “The Igraph Software Package for
Complex Network Research.” *InterJournal* Complex Systems: 1695.
<https://igraph.org>.

</div>

<div id="ref-igraph2025" class="csl-entry">

Csárdi, Gábor, Tamás Nepusz, Vincent Traag, Szabolcs Horvát, Fabio
Zanini, Daniel Noom, and Kirill Müller. 2025.
*<span class="nocase">igraph</span>: Network Analysis and Visualization
in r*. <https://doi.org/10.5281/zenodo.7682609>.

</div>

<div id="ref-tidytable" class="csl-entry">

Fairbanks, Mark. 2024. *<span class="nocase">tidytable</span>: Tidy
Interface to “<span class="nocase">data.table</span>”*.
<https://doi.org/10.32614/CRAN.package.tidytable>.

</div>

<div id="ref-khroma" class="csl-entry">

Frerebeau, Nicolas. 2025. *<span class="nocase">khroma: Colour Schemes
for Scientific Data Visualization</span>*. Pessac, France: Université
Bordeaux Montaigne. <https://doi.org/10.5281/zenodo.1472077>.

</div>

<div id="ref-stringi" class="csl-entry">

Gagolewski, Marek. 2022. “<span class="nocase">stringi</span>: Fast and
Portable Character String Processing in R.” *Journal of Statistical
Software* 103 (2): 1–59. <https://doi.org/10.18637/jss.v103.i02>.

</div>

<div id="ref-dendextend" class="csl-entry">

Galili, Tal. 2015. “<span class="nocase">dendextend</span>: An r Package
for Visualizing, Adjusting, and Comparing Trees of Hierarchical
Clustering.” *Bioinformatics*.
<https://doi.org/10.1093/bioinformatics/btv428>.

</div>

<div id="ref-SensoMineR" class="csl-entry">

Husson, Francois, Sebastien Le, and Marine Cadoret. 2025. *SensoMineR:
Sensory Data Analysis*.
<https://doi.org/10.32614/CRAN.package.SensoMineR>.

</div>

<div id="ref-NMRphasing" class="csl-entry">

Jiang, Aixiang. 2025. *NMRphasing: Phase Error Correction and Baseline
Correction for One Dimensional (“1D”) “NMR” Data*.
<https://doi.org/10.32614/CRAN.package.NMRphasing>.

</div>

<div id="ref-ggpubr" class="csl-entry">

Kassambara, Alboukadel. 2025. *<span class="nocase">ggpubr</span>:
“<span class="nocase">ggplot2</span>” Based Publication Ready Plots*.
<https://doi.org/10.32614/CRAN.package.ggpubr>.

</div>

<div id="ref-FactoMineR" class="csl-entry">

Lê, Sébastien, Julie Josse, and François Husson. 2008. “FactoMineR: A
Package for Multivariate Analysis.” *Journal of Statistical Software* 25
(1): 1–18. <https://doi.org/10.18637/jss.v025.i01>.

</div>

<div id="ref-AlpsNMR" class="csl-entry">

Madrid-Gambin, Francisco, Oller-Moreno, Sergio, Fernandez, Luis,
Bartova, et al. 2020. “AlpsNMR: An r Package for Signal Processing of
Fully Untargeted NMR-Based Metabolomics.” *Bioinformatics*, January.
<https://doi.org/10.1093/bioinformatics/btaa022>.

</div>

<div id="ref-BiocVersion" class="csl-entry">

Morgan, Martin. 2025. *BiocVersion: Set the Appropriate Version of
Bioconductor Packages*. <https://doi.org/10.18129/B9.bioc.BiocVersion>.

</div>

<div id="ref-BiocManager" class="csl-entry">

Morgan, Martin, and Marcel Ramos. 2025. *BiocManager: Access the
Bioconductor Project Package Repository*.
<https://doi.org/10.32614/CRAN.package.BiocManager>.

</div>

<div id="ref-ggraph" class="csl-entry">

Pedersen, Thomas Lin. 2024. *<span class="nocase">ggraph</span>: An
Implementation of Grammar of Graphics for Graphs and Networks*.
<https://doi.org/10.32614/CRAN.package.ggraph>.

</div>

<div id="ref-base" class="csl-entry">

R Core Team. 2025. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-cascade2025" class="csl-entry">

Rutz, Adriano. 2025. *<span class="nocase">cascade</span>:
Contextualizing Untargeted Annotation with Semi-Quantitative Charged
Aerosol Detection for Pertinent Characterization of Natural Extracts*.

</div>

<div id="ref-cascade2023" class="csl-entry">

Rutz, Adriano, and Jean-Luc Wolfender. 2023. “Automated Composition
Assessment of Natural Extracts: Untargeted Mass Spectrometry-Based
Metabolite Profiling Integrating Semiquantitative Detection.” *Journal
of Agricultural and Food Chemistry* 71 (46).
<https://doi.org/10.1021/acs.jafc.3c03099>.

</div>

<div id="ref-ggbump" class="csl-entry">

Sjoberg, David. 2020. *<span class="nocase">ggbump</span>: Bump Chart
and Sigmoid Curves*. <https://doi.org/10.32614/CRAN.package.ggbump>.

</div>

<div id="ref-ggrepel" class="csl-entry">

Slowikowski, Kamil. 2024. *<span class="nocase">ggrepel</span>:
Automatically Position Non-Overlapping Text Labels with
“<span class="nocase">ggplot2</span>”*.
<https://doi.org/10.32614/CRAN.package.ggrepel>.

</div>

<div id="ref-testthat" class="csl-entry">

Wickham, Hadley. 2011. “<span class="nocase">testthat</span>: Get
Started with Testing.” *The R Journal* 3: 5–10.
<https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-pkgload" class="csl-entry">

Wickham, Hadley, Winston Chang, Jim Hester, and Lionel Henry. 2024.
*<span class="nocase">pkgload</span>: Simulate Package Installation and
Attach*. <https://doi.org/10.32614/CRAN.package.pkgload>.

</div>

<div id="ref-scales" class="csl-entry">

Wickham, Hadley, Thomas Lin Pedersen, and Dana Seidel. 2025.
*<span class="nocase">scales</span>: Scale Functions for Visualization*.
<https://doi.org/10.32614/CRAN.package.scales>.

</div>

<div id="ref-treemapify" class="csl-entry">

Wilkins, David. 2023. *<span class="nocase">treemapify</span>: Draw
Treemaps in “<span class="nocase">ggplot2</span>”*.
<https://doi.org/10.32614/CRAN.package.treemapify>.

</div>

<div id="ref-knitr2014" class="csl-entry">

Xie, Yihui. 2014. “<span class="nocase">knitr</span>: A Comprehensive
Tool for Reproducible Research in R.” In *Implementing Reproducible
Computational Research*, edited by Victoria Stodden, Friedrich Leisch,
and Roger D. Peng. Chapman; Hall/CRC.

</div>

<div id="ref-knitr2015" class="csl-entry">

———. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca Raton,
Florida: Chapman; Hall/CRC. <https://yihui.org/knitr/>.

</div>

<div id="ref-knitr2025" class="csl-entry">

———. 2025. *<span class="nocase">knitr</span>: A General-Purpose Package
for Dynamic Report Generation in R*. <https://yihui.org/knitr/>.

</div>

<div id="ref-rmarkdown2018" class="csl-entry">

Xie, Yihui, J. J. Allaire, and Garrett Grolemund. 2018. *R Markdown: The
Definitive Guide*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown>.

</div>

<div id="ref-rmarkdown2020" class="csl-entry">

Xie, Yihui, Christophe Dervieux, and Emily Riederer. 2020. *R Markdown
Cookbook*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown-cookbook>.

</div>

</div>
