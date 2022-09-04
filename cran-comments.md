In this release we have made the following changes:

- Changed the code used to test the version of PROJ on attach. The old approach used to create an error for sf < 1.0.
- Adjusted one of the vignettes following the changes in dplyr 1.0.8.
- Removed conflicting URL from package documentation `spatial_edge_measures`### New features

* Updates to `to_spatial_smooth()` morpher:
  - Argument `summarise_attributes` added to summarise attribute values of concatenated edges. Refs [#120](https://github.com/luukvdmeer/sfnetworks/issues/120).
  - Argument `require_equal` added to specify if and which attributes should be checked for equality before removing a pseudo node. Refs [#124](https://github.com/luukvdmeer/sfnetworks/issues/124).
  - Argument `protect` added to specify nodes that should never be removed, even if they are a pseudo node. Refs [#177](https://github.com/luukvdmeer/sfnetworks/issues/177).
  - Concatenated edges after smoothing are now allowed to cross themselves. Refs [#117](https://github.com/luukvdmeer/sfnetworks/issues/117).
* Updates to `st_network_cost()`:
  - Duplicated nodes are now accepted in the `to` argument. Refs [#183](
  https://github.com/luukvdmeer/sfnetworks/issues/183).
  - Cost matrix output now contains units. Refs [#119](
  https://github.com/luukvdmeer/sfnetworks/issues/119).
  - Argument `direction` added to specify if outbound, inbound or all edges should be considered. This replaces the argument `mode` from `igraph::distances()`. The default is "out", while before it was "all". For undirected networks this argument is ignored.
* Edge measure function gained an argument `degrees` which can be set to `TRUE` to return angles in degrees instead of radians.
* By default `st_network_paths()` now encodes nodes by their name, whenever a name attribute is present in the nodes table. This can be disabled by setting `use_names = FALSE`. Refs [#154](https://github.com/luukvdmeer/sfnetworks/issues/154).
* Functions `sf::st_precision()` and `sf::st_set_precision()` now have a method for `sfnetwork` objects, such that coordinate precision can be queried and set. Refs [#209](https://github.com/luukvdmeer/sfnetworks/issues/209).
* Functions `sf::st_intersection()` and `sf::st_difference()` now have a method for `sfnetwork` objects, such that networks can be spatially clipped. The method for `sf::st_crop()` now uses the same workflow. These functions do not work yet on edges of undirected networks. Refs [#133](https://github.com/luukvdmeer/sfnetworks/issues/133).
* Function `sf::st_drop_geometry()` is now a generic and therefore got an `sfnetwork` method.
* Several other sf functions got an `sfnetwork` method, merely to be consistent in the type of functions we provide a method for. Only those functions that mutate the geometry column of an sf object in such a way that would break the valid spatial network structure are not supported.
* Methods for `sf::st_coordinates()`, `sf::st_bbox()` and `sf::st_crs()` gained an `active` argument such that this information can be extracted from any network element without first activating it. Refs [#215](https://github.com/luukvdmeer/sfnetworks/issues/215).

### Bug fixes

* Rd files do not contain code anymore that is incompatible with HTML5. Refs [#221](https://github.com/luukvdmeer/sfnetworks/issues/221).
* Print methods now return x invisibly. Refs [#217](https://github.com/luukvdmeer/sfnetworks/issues/217).

### Refactoring

* Error messages when edge measure or query functions are applied to a network with active nodes, or when node measure or query functions are applied to a network with active edges, are now more informative. Refs [#216](https://github.com/luukvdmeer/sfnetworks/issues/216).
* Calculating straight-line distances of edges is now more performant. Refs [#180](https://github.com/luukvdmeer/sfnetworks/issues/180).
* Edge measure function `edge_circuity()` does not return units objects anymore, since circuity is unitless.

### Community

* Vignettes updated to showcase new features.
* Code of conduct updated according to newer template.
* Contributing guidelines extended with templates for commit messages.

## revdepcheck results

We checked 1 reverse dependencies (0 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## R CMD check results

0 errors | 0 warnings | 0 notes
