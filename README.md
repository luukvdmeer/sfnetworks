# Tidy Geospatial Networks in R <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![R build status](https://github.com/luukvdmeer/sfnetworks/workflows/R-CMD-check/badge.svg)](https://github.com/luukvdmeer/sfnetworks/actions)
[![Codecov test coverage](https://codecov.io/gh/luukvdmeer/sfnetworks/branch/master/graph/badge.svg)](https://app.codecov.io/gh/luukvdmeer/sfnetworks)
[![CRAN](http://www.r-pkg.org/badges/version/sfnetworks)](https://cran.r-project.org/package=sfnetworks)
[![Downloads](https://cranlogs.r-pkg.org/badges/sfnetworks)](https://cran.r-project.org/package=sfnetworks)
<!-- badges: end -->

`sfnetworks` is an R package for analysis of geospatial networks. It connects the functionalities of the `tidygraph` package for network analysis and the `sf` package for spatial data science.

## Background

Geospatial networks are graphs embedded in geographical space. That means that both the nodes and edges in the graph can be represented as geographic features: the nodes most commonly as points, and the edges as linestrings. They play an important role in many different domains, ranging from transportation planning and logistics to ecology and epidemiology. The structure and characteristics of geospatial networks go beyond standard graph topology, and therefore it is crucial to explicitly take space into account when analyzing them.

We created `sfnetworks` to facilitate such an integrated workflow. It combines the forces of two popular R packages: [sf](https://r-spatial.github.io/sf/) for spatial data science and [tidygraph](https://tidygraph.data-imaginist.com/index.html) for standard graph analysis. The core of the package is a dedicated data structure for geospatial networks, that can be provided as input to both the graph analytical functions of tidygraph as well as the spatial analytical functions of sf, without the need for conversion. Additionally, we implemented a set of geospatial network specific functions, such as routines for shortest path calculation, network cleaning and topology modification. `sfnetworks` is designed as a general-purpose package suitable for usage across different application domains, and can be seamlessly integrated in [tidyverse](https://www.tidyverse.org/) workflows.

## Installation

You can install the latest stable version of `sfnetworks` from [CRAN](https://cran.r-project.org/package=sfnetworks) with:

``` r
install.packages("sfnetworks")
```

You can install the development version from [GitHub](https://github.com/luukvdmeer/sfnetworks) with:

```r
remotes::install_github("luukvdmeer/sfnetworks")
```

**Note:** Two important dependencies of `sfnetworks`, the `sf` package for spatial data science and the `igraph` package for network analysis (which is the main "analysis backend" of `tidygraph`), require some low-level software libraries to be installed on your system. Depending on which operating system you use, this can mean that you have to install these system requirements first, before you can install `sfnetworks`. See the installation guides of [sf](https://github.com/r-spatial/sf#installing) and [igraph](https://github.com/igraph/rigraph#installation) for details.

## Usage

The main goal of `sfnetworks` is to connect the `tidygraph` package for network analysis and the `sf` package for spatial data science. To make the most out of it, it is recommended to make yourself familiar with these two 'parent packages' if you don't know them yet.

- [sf documentation](https://r-spatial.github.io/sf/)
- [tidygraph documentation](https://tidygraph.data-imaginist.com/)

There are currently five vignettes that guide you through the functionalities of `sfnetworks`:

- [The sfnetwork data structure](https://luukvdmeer.github.io/sfnetworks/articles/sfn01_structure.html)
- [Network pre-processing and cleaning](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html)
- [Spatial joins and filters](https://luukvdmeer.github.io/sfnetworks/articles/sfn03_join_filter.html)
- [Routing](https://luukvdmeer.github.io/sfnetworks/articles/sfn04_routing.html)
- [Spatial morphers](https://luukvdmeer.github.io/sfnetworks/articles/sfn05_morphers.html)

<img align="left" src="https://raw.githubusercontent.com/loreabad6/ggraph-spatial-examples/main/figs/sfnetworks-showcase.gif" >

(GIF (c) by [Lore Abad](https://github.com/loreabad6))

## Contribution

We look very much forward to contributions to the package. See the [contributing guide](https://github.com/luukvdmeer/sfnetworks/blob/master/CONTRIBUTING.md) for further details.

This project is released with a [Contributor Code of Conduct](https://github.com/luukvdmeer/sfnetworks/blob/master/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## Acknowledgment

This project gratefully acknowledges financial [support](https://www.r-consortium.org/projects) from the

<a href="https://www.r-consortium.org/projects/awarded-projects">
<img src="https://raw.githubusercontent.com/RConsortium/artwork/main/r_consortium/R_Consortium-logo-horizontal-color.png" width="300">
</a>
