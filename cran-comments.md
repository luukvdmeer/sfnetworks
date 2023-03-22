In this release we have made the following changes:

* Argument names of sfnetwork S3 methods for `sf::st_geometry()`, `sf::st_bbox()`, `sf::st_m_range()` and `sf::st_set_precision()` are updated to be consistent with their corresponding generic functions in sf.
* Arguments `active` and `...` are removed from the sfnetwork S3 method for `sf::st_precision()` to be consistent with its corresponding generic function in sf.
* Argument `active` is removed from the sfnetwork S3 method for `sf::st_crs()` to be consistent with the above-mentioned change regarding `sf::st_precision()` (since both CRS and precision can not differ between nodes and edges).

These changes solve the warnings in the CRAN checks regarding S3 method consistency.

## R CMD check results

0 errors | 0 warnings | 0 notes

When running checks on R-hub, the following note occured only on Windows (Server 2022, R-devel 64-bit):

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this is a bug in R-hub rather than a bug in our package, and can likely be ignored.

## revdepcheck results

We checked 1 reverse dependencies (0 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
