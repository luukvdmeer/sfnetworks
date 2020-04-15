# Tidy Geospatial Networks in R

<!-- badges: start -->

<!-- ![R-CMD-check](https://github.com/luukvdmeer/sfnetworks/workflows/R-CMD-check/badge.svg) -->

<!-- Plan: add badge showing documents build on pkgdown (RL) -->

![R-CMD-check-develop](https://github.com/luukvdmeer/sfnetworks/workflows/R-CMD-check/badge.svg?branch=develop)
<!-- badges: end -->

The goal of sfnetworks is to fill a missing block in R’s provision for spatial network analysis. Read more about the idea and the currently implemented functionalities in the [introductory vignette]().

## Installation

<!-- You can install the released version of sfnetworks from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("sfnetworks") -->

<!-- ``` -->

The `sfnetworks` package is still in very active development, Therefore, the package is also not on CRAN yet. Install it directly from GitHub using the `remotes` package in R.

Install the more stable master branch with:

```r
remotes::install_github("luukvdmeer/sfnetworks")
```

Install the `develop` branch, where most of the development takes place and where PRs should be directed, as follows:

```r
remotes::install_github("luukvdmeer/sfnetworks", ref = "develop")
```

Originally, `sfnetworks` was created as a homework assignment for an R course. Some people actually started using that version of the package in their work. Note that the new package is completely different! If you still want to install the original version, use:

```r
remotes::install_github("luukvdmeer/sfnetworks@v0.1")
```

## Contribution
We look very much forward to contributions to the package. This can be in the form of pull requests (please direct them to the `develop` branch), but also by opening issues in the [issue tracker](https://github.com/luukvdmeer/sfnetworks/issues). Such issues can be specific feature requests labeled with the `feature` label (e.g. "I would like to see a function `f(x)` that accepts an object `x` of class `foo` and returns < this >"). It can also be a more generic description of problems that you deal with when working with spatial networks in R (e.g. "I usually find it impossible/confusing/complicated to do < this >"). Please label such issues with the `hackathon` label, since we will be organising a spatial networks hackathon in which we hope to solve these kind of problems. For more information on that event, see [here](https://www.eventbrite.co.uk/e/erum2020-satellite-event-hackathon-on-spatial-networks-tickets-90976873277) (and join!).

We strive to follow the [tidyverse styleguide](https://style.tidyverse.org/) in the source code of the package. An exception to that is the assignment operator: we use `=` instead of `<-` (see [here](https://github.com/Robinlovelace/geocompr/issues/319) for some reasons why).

This project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.  
