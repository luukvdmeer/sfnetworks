In this release we have made the following changes:

* To address CRAN error on Fedora, the package version is now specified as a character instead of numeric.

* The `sfnetwork()` construction function now has an argument `message` which can be set to `FALSE` when the network validity checks should not print informational messages to the console. Refs [#261](https://github.com/luukvdmeer/sfnetworks/issues/261).

* Code and documentation was updated where needed to align with changes in base R and/or package dependencies. No changes to program logic or behavior.

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
