# Tidy Geospatial Networks in R

<!-- badges: start -->

<!-- ![R-CMD-check](https://github.com/luukvdmeer/sfnetworks/workflows/R-CMD-check/badge.svg) -->

<!-- Plan: add badge showing documents build on pkgdown (RL) -->

![R-CMD-check-develop](https://github.com/luukvdmeer/sfnetworks/workflows/R-CMD-check/badge.svg?branch=develop)

<!-- badges: end -->

Thanks to active developer and user communities, R is becoming an increasingly popular language for interactive geospatial data analysis. A large step forward was the release of the [sf package](https://r-spatial.github.io/sf/), which provides classes and functions to represent and manipulate spatial vector data. Unlike its predecessors, `sf` is compatible with the popular data science oriented packages that form the [tidyverse](https://www.tidyverse.org/), most notably the data manipulation toolbox [dplyr](https://dplyr.tidyverse.org/), and more generally with the concept of [tidy data](https://vita.had.co.nz/papers/tidy-data.pdf).

R is also well-suited for network analysis, thanks to the R interface of the [igraph library](https://igraph.org/). The [tidygraph package](https://tidygraph.data-imaginist.com/index.html) extends `igraph` into the domain of the tidyverse, enabling compatibility with `dplyr` by treating a graph as a collection of two tidy data frames describing respectively the nodes and edges.

Given the strength of R in spatial *and* network analysis, one would expect it to be an ideal language for analysis of geospatial networks. Geospatial networks are simultaneously graph and spatial objects, with nodes and edges embedded in geographic space. Well known examples include transport networks, river basins, power grids, ecological networks and geolocated social networks. 

Although several R packages exist that address geospatial networks, they often focus on a specific application within the broader domain of geospatial network analysis, or complicate tidyverse compatibility. At present, no general purpose, tidyverse compatible classes exist for geospatial network data, representing a gap in R's spatial and network analysis ecosystems (see the [gRaphical Models](https://cran.r-project.org/web/views/gR.html) and [Spatial](https://cran.r-project.org/web/views/Spatial.html) task views). `sfnetworks` is created to fill this gap, and form a connecting edge between the two worlds.

The core of the package is a data structure that can be provided as input to both graph analytical functions of `tidygraph` and to spatial analytical functions of `sf`, without the need for conversion. On top of that, it contains a set of spatial network specific functions that can be used alongside the offer of the two "parent packages".

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
We look very much forward to contributions to the package. Please refer to the [contributing](CONTRIBUTING.md) guide for further details. 

This project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.  

## Acknowledgment

This project gratefully acknowledges financial [support](https://www.r-consortium.org/projects) from the

<a href="https://www.r-consortium.org/projects/awarded-projects">
<img src="https://raw.githubusercontent.com/RConsortium/artwork/main/r_consortium/R_Consortium-logo-horizontal-color.png" width="300">
</a>
