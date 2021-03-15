# Tidy Geospatial Networks in R <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![R build status](https://github.com/luukvdmeer/sfnetworks/workflows/R-CMD-check/badge.svg)](https://github.com/luukvdmeer/sfnetworks/actions)
[![Codecov test coverage](https://codecov.io/gh/luukvdmeer/sfnetworks/branch/master/graph/badge.svg)](https://codecov.io/gh/luukvdmeer/sfnetworks/)
[![CRAN](http://www.r-pkg.org/badges/version/sfnetworks)](https://cran.r-project.org/package=sfnetworks)
[![Downloads](https://cranlogs.r-pkg.org/badges/sfnetworks?color=orange)](https://cran.r-project.org/package=sfnetworks)

<!-- badges: end -->

`sfnetworks` is an R package for analysis of geospatial networks. It connects the functionalities of the `tidygraph` package for network analysis and the `sf` package for spatial data science.

## Background

Thanks to active developer and user communities, R is becoming an increasingly popular language for interactive geospatial data analysis. A large step forward was the release of the [sf package](https://r-spatial.github.io/sf/), which provides classes and functions to represent and manipulate spatial vector data. Unlike its predecessors, `sf` is compatible with the popular data science oriented packages that form the [tidyverse](https://www.tidyverse.org/), most notably the data manipulation toolbox [dplyr](https://dplyr.tidyverse.org/), and more generally with the concept of [tidy data](https://vita.had.co.nz/papers/tidy-data.pdf).

R is also well-suited for network analysis, thanks to the R interface of the [igraph library](https://igraph.org/). The [tidygraph package](https://tidygraph.data-imaginist.com/index.html) extends `igraph` into the domain of the tidyverse, enabling compatibility with `dplyr` by treating a graph as a collection of two tidy data frames describing respectively the nodes and edges.

Given the strength of R in spatial *and* network analysis, one would expect it to be an ideal language for analysis of geospatial networks. Geospatial networks are simultaneously graph and spatial objects, with nodes and edges embedded in geographic space. Well known examples include transport networks, river basins, power grids, ecological networks and geolocated social networks.

Although several R packages exist that address geospatial networks, they often focus on a specific application within the broader domain of geospatial network analysis, or complicate tidyverse compatibility. At present, no general purpose, tidyverse compatible classes exist for geospatial network data, representing a gap in R's spatial and network analysis ecosystems (see the [gRaphical Models](https://cran.r-project.org/view=gR) and [Spatial](https://cran.r-project.org/view=Spatial) task views). `sfnetworks` is created to fill this gap, and form a connecting edge between the two worlds.

The core of the package is a data structure that can be provided as input to both graph analytical functions of `tidygraph` and to spatial analytical functions of `sf`, without the need for conversion. On top of that, it contains a set of spatial network specific functions that can be used alongside the offer of the two 'parent packages'.

## Installation

You can install the latest stable version of `sfnetworks` from [CRAN](https://cran.r-project.org/package=sfnetworks) with:

``` r
install.packages("sfnetworks")
```

The `sfnetworks` package is still in very active development, Therefore, the package is also not on CRAN yet. Install it directly from GitHub using the `remotes` package in R.

You can install the development version from [GitHub](https://github.com/luukvdmeer/sfnetworks) with:

```r
remotes::install_github("luukvdmeer/sfnetworks")
```

**Note:** Two important dependencies of `sfnetworks`, the `sf` package for spatial data science and the `igraph` package for network analysis, require some low-level software libraries to be installed on your system. Depending on which operating system you use, this can mean that you have to install these system requirements first, before you can install `sfnetworks`. See the installation guides of [sf](https://github.com/r-spatial/sf#installing) and [igraph](https://github.com/igraph/rigraph#installation) for details.

## Usage

The main goal of `sfnetworks` is to connect the `tidygraph` package for network analysis and the `sf` package for spatial data science. To make the most out of it, it is recommended to make yourself familiar with these two 'parent packages' if you don't know them yet.

- [sf documentation](https://r-spatial.github.io/sf/)
- [tidygraph documentation](https://tidygraph.data-imaginist.com/)

There are currently five vignettes that guide you through the functionalities of `sfnetworks`:

- [The sfnetwork data structure](https://luukvdmeer.github.io/sfnetworks/articles/structure.html)
- [Network pre-processing and cleaning](https://luukvdmeer.github.io/sfnetworks/articles/preprocess_and_clean.html)
- [Spatial joins and filters](https://luukvdmeer.github.io/sfnetworks/articles/join_filter.html)
- [Routing](https://luukvdmeer.github.io/sfnetworks/articles/routing.html)
- [Spatial morphers](https://luukvdmeer.github.io/sfnetworks/articles/morphers.html)

<img align="left" src="https://github.com/loreabad6/ggraph-spatial-examples/blob/main/figs/sfnetworks-showcase.gif" >

(GIF (c) by [Lore Abad](https://github.com/loreabad6))

## Contribution

We look very much forward to contributions to the package. See the [contributing guide](https://github.com/luukvdmeer/sfnetworks/blob/master/CONTRIBUTING.md) for further details.

This project is released with a [Contributor Code of Conduct](https://github.com/luukvdmeer/sfnetworks/blob/master/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## Acknowledgment

This project gratefully acknowledges financial [support](https://www.r-consortium.org/projects) from the

<a href="https://www.r-consortium.org/projects/awarded-projects">
<img src="https://raw.githubusercontent.com/RConsortium/artwork/main/r_consortium/R_Consortium-logo-horizontal-color.png" width="300">
</a>
