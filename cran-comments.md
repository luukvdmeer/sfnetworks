## Resubmission
This is a resubmission. I have made the following changes:

- Regarding comment: "License components with restrictions and base license"
  - We omited "| file LICENSE" from the DESCRIPTION file
 
- Regarding comment: "Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)"
  - Value entries have been added to the following .Rd files: as_tibble.Rd, is.sfnetwork.Rd and sf.Rd
  - Value entries have been extended in the following .Rd files: st_network_bbox.Rd, st_network_blend.Rd, st_network_join.Rd
  - A value entry has *not* been added to plot.Rd, since it seems to us this is not common for plot methods in any R packages, including base R.
  - A arguments entry has *not* been added to node_coordinates.Rd since these functions do not have arguments. Following the tidygraph philosophy, they are meant to be called inside the environment of "dplyr verbs" like mutate and filter, where the network that is being worked on is known and not needed as an argument to the function. This behaviour is documented in the function documentation.
  
- Regarding comment: "You have examples for unexported functions. Please either omit these examples or export the functions."
  - To avoid large dependencies for only one additional function, we use a dynamic registration of the spatstat::linnet and ggplot2::autoplot methods on load. That is the reason they are not exported. We have now removed their example sections.
  
- Regarding comment: "Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. In your examples/vignettes/tests you can write to tempdir()."
  - We have made sure functions do not write in the user's filespace

- Regarding comment: "Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos."
  - par() and options() parameters are now reset after each example and vignette.

- Additionally:
  - We updated the implementation of the blending algorithm to be faster and more reliable. This fixed several small bugs in the package.
  - We fixed a bug that caused required attributes to be dropped during network construction.

## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
