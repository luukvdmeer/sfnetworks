This is a resubmission to reinstate the package. The package was archived because of a failing test, caused by a base R update on R-devel that affected the way the test was executed. The test has been updated accordingly.

In addition, the following changes have been made:

- The return value of plot.sfnetwork is now documented such that the corresponding .Rd file contains the required \value tag.
- The actual implementation of plot.sfnetwork is updated to be cleaner and more efficient.
  
It also has been double-checked that no functions or vignettes write to the filespace of the user.

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 1 reverse dependencies (0 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
