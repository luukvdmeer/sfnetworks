## Resubmission
This is a resubmission. I have made the following changes:

- All functions that interact with the `spatstat` package have been updated to comply with the new structure of that package (see https://github.com/spatstat/spatstat/tree/v1.64-2#spatstat-is-now-split-into-several-packages). These include `as_sfnetwork.linnet()`, `as_sfnetwork.psp()` and `as.linnet.sfnetwork()`. Documentation entries containing links to the spatstat documentation have been updated accordingly as well.

- Internally, usage of `match()` for checking coordinate equality is replaced by a new `st_match()` utility function specifically designed for this task.

- It is now clearly documented that using `sf::st_reverse()` to reverse edge linestrings is only possible with GEOS versions >= 3.7. Test and examples for this function now first check if the required GEOS version is installed before running.

## R CMD check results

0 errors | 0 warnings | 0 notes
