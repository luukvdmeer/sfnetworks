In this release I have made the following changes:

- Addition of `n_active` and `n_inactive` arguments to the print method of an sfnetwork object. The arguments define how many rows are printed for respectively the active and inactive network element. The values of these arguments can also be set globally by running e.g. `options(sfn_max_print_active = 1, sfn_max_print_inactive = 2)`.

- The example dataset `Roxel` is updated to comply with recent updates on the way a CRS is specified in an sf object.

- GitHub Actions workflows are updated to comply with new developments.

- Documentation updates:
  - Vignette file names are updated such that the appear in correct order on CRAN.
  - Example section of the plot method for sfnetwork objects now includes an example of how to add graticules and axes.

In addition, potential issues with the new release of package dependency `igraph` were investigated. It was noticed that all [CRAN package checks](https://cran.r-project.org/web/checks/check_results_sfnetworks.html) for `sfnetworks` passed without issues also after the new `igraph` release, and that no "Additional issues" where listed.

## R CMD check results

0 errors | 0 warnings | 0 notes
