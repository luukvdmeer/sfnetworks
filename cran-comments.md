In this release we have made the following changes:

* The `st_network_paths()` function is now correctly aligned with the newest release of `igraph` by selecting only the first element when multiple from nodes are given. No changes in user behavior compared to previous releases of `sfnetworks`. This change fixes the present errors in the CRAN checks.

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 11 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
