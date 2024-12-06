In this release we have made the following changes:

* The `print()` method for sfnetwork objects now correctly prints networks with active edges that are spatially implicit.
* The `print()` method for sfnetwork objects no longer uses the deprecated function `tibble::trunc_mat()`.
* `to_spatial_contracted()` now correctly handles group indices that are not ordered.
* The usage of `igraph::adjacent_vertices()` and `igraph::incident_edges()` inside `to_spatial_smooth()` is now aligned to updates in igraph v2.1.1 that changed the zero-based indexing of the return to one-based indexing. This alignment is done dynamically, without forcing the most recent igraph version as required dependency.
* Whenever the installed igraph version is >= 2.1.0, `igraph::get_edge_ids()` is used instead of the deprecated `igraph::get.edge.ids()` inside `to_spatial_smooth()`.

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 5 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
