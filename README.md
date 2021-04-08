
# p3k14c

<!-- badges: start -->
<!-- badges: end -->

This is the submission release for the P3K14C data publication:

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