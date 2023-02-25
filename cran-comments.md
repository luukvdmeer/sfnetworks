In this release we have made the following changes:

* Unit tests that where failing after an igraph update now run successfully again. Refs [#232](https://github.com/luukvdmeer/sfnetworks/issues/232). This means the CRAN checks that where failing should now be passing successfully again.
* Unit test stuck in an infinite loop caused by usage of `sf::st_sample()` on a polygon crossing the date line updated accordingly. See [this sf issue](https://github.com/r-spatial/sf/issues/1984) for details on the underlying problem.
* The `to_spatial_contracted()` morpher now correctly handles cases for undirected networks in which only loop edges are created after contraction. Refs [#237](https://github.com/luukvdmeer/sfnetworks/issues/237).
* The `to_spatial_contracted()` morpher now directly returns the original network when none of the contraction groups contain more than one node.
* Umbrella packages tidyverse and spatstat are no longer suggested packages. Only individual members of these packages are now suggested packages.

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 1 reverse dependencies (0 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
