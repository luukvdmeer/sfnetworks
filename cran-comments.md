In this release I have made the following changes:

- Compatibility with the `s2` package for spherical geometry operations by adding a `s2::as_s2_geography()` method for sfnetwork objects. In the new version of `sf` (one of our core dependencies), the `s2` package will be used for geometric operations involving longitude-latitude coordinates, see https://github.com/r-spatial/sf/issues/1649.

- Minor bug fixes:
  - When setting `length_as_weight = TRUE` in the sfnetwork construction function, the added weight column now preserves specification of units.
  - `st_network_blend()` now internally uses `sf::st_cast()` instead of `sfheaders::sfc_cast()` to avoid errors with some CRS specifications.

- Documentation improvements:
  - Extended function documentation for the shortest paths functions.
  - Updated vignettes.

## R CMD check results

0 errors | 0 warnings | 0 notes
