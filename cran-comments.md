## Resubmission
This is a resubmission. I have made the following changes:

- License components with restrictions and base license
  - We omited "| file LICENSE" from the DESCRIPTION file
 
- Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
  - 
  
- You have examples for unexported functions. Please either omit these examples or export the functions.
  - To avoid a strong dependency from these packages, we have applied a dynamic registration of the methods on load, hence we would like to keep the functions without exporting them, but also give examples of their use.
  
- Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). This is not allowed by CRAN policies. In
your examples/vignettes/tests you can write to tempdir().
  - We have made sure functions do not write in the user's filespace

- Please always make sure to reset to user's options(), working directory
or par() after you changed it in examples and vignettes and demos.
  - par() and options() parameters are reset after each example and vignette.


## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
