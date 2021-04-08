
# p3k14c

<!-- badges: start -->
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/people3k/p3k14c/master?urlpath=rstudio)
<!-- badges: end -->

This repository contains the data and code for our paper:

> Darcy Bird [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-3466-6284),
Lux Miranda [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-3753-7405),
Marc Vander Linden [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-0120-7754),
Robinson, Erick [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-0789-3724),
R. Kyle Bocinsky [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-1862-3428),
Chris Nicholson [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-3212-2662),
José M. Capriles [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-6046-0939),
Judson Byrd Finley [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-0233-8630),
Eugenia M. Gayo [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-0746-0512),
Adolfo Gil [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-5718-8866),
Jade d’Alpoim Guedes [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-2627-7863),
Julie A. Hoggarth [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-8612-8846),
Andrea Kay [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-8285-1893),
Emma Loftus [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-4442-8511),
Umberto Lombardo [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-0001-4870),
Madeline Mackie [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-0769-2348),
Alessio Palmisano [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-0758-5032),
Steinar Solheim [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-8293-8147),
Jacob Freeman [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-7402-8450),
Robert L. Kelly[![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-9737-0152),
> ( **In review**). p3k14c: A synthetic global database of archaeological radiocarbon dates.

## Installation

You can install the released version of p3k14c from Github with:

``` r
# install.packages("remotes")
remotes::install_github("people3k/p3k14c")
```

## Executable research compendium

You can compile the research compendium with:

``` r
rmarkdown::render("vignettes/articles/p3k14c.Rmd", output_dir = "vignettes/articles")
```

### How to cite

Please cite this compendium as:

> Bocinsky, R. Kyle, Darcy Bird, and Erick Robinson, (2020). *Compendium
> of R code and data for Dendrochronological dates confirm a Late
> Prehistoric population decline in the American Southwest derived from
> radiocarbon dates*. Accessed \[TODAY'S DATE\].

## Contents

The [:file\_folder: analysis](/analysis) directory contains:

  - [:page\_facing\_up: p3k14c](/analysis/p3k14c): R
    Markdown source document for the manuscript. Includes code to
    reproduce the figures and tables generated by the analysis. It also
    has a rendered version, `p3k14c.pdf`, suitable for reading
    (the code is replaced by figures and tables in this file).
  - [:file\_folder: data](/analysis/data): Data used in the analysis.  
  - [:file\_folder: figures](/analysis/figures): Plots and other
    illustrations
  - [:file\_folder: tables](/analysis/tables): Tables included in 
    the paper

## How to run in your browser or download and run locally

The simplest way to explore the text, code and data is to click on
[binder](https://mybinder.org/v2/gh/people3k/p3k14c/master?urlpath=rstudio)
to open an instance of RStudio in your browser, which will have the
compendium files ready to work with. Binder uses
(rocker-project.org)\[rocker-project.org Docker\] images to ensure a
consistent and reproducible computational environment. These Docker
images can also be used locally.

You can download the compendium as a zip from from this URL:
[main.zip](/archive/main.zip). After unzipping, open the `Proj` file
in RStudio, and run `devtools::install()` to ensure you have the
packages this analysis depends on (also listed in the
[DESCRIPTION](/DESCRIPTION) file). Then open `p3k14c.Rmd` and knit
to produce the paper in PDF format.

Or, simply install straight from Github:

``` r
# install.packages("devtools")
devtools::install_github("people3k/p3k14c")
```

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** [GNU GPLv3](LICENSE.md)

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.

### Acknowledgements

This compendium was created using the [`rrtools` package by Ben
Marwick](https://github.com/benmarwick/rrtools), which is ✨ pure magic ✨
for doing reproducible research.


## Code of Conduct

Please note that the p3k14c project is released with a [Contributor Code of Conduct](https://people3k.github.io/p3k14c/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.