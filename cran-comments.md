This is a resubmission to reinstate the package. I have made the following changes:
- Regarding comment: "Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar). Missing Rd-tags: plot.sfnetwork.Rd: \value"
  - A value entry has *not* been added to plot.Rd, since it seems to us this is not common for plot methods in any R packages, including base R.
  
- Regarding comment: "Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. In your examples/vignettes/tests you can write to tempdir()."
  - We have made sure functions do not write in the user's filespace

- Additionally
  - Updated plot algorithm to be faster and more efficient. Refs [#226](https://github.com/luukvdmeer/sfnetworks/issues/226) and [#228](https://github.com/luukvdmeer/sfnetworks/pull/228)

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 1 reverse dependencies (0 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
