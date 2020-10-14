# development

* Implementation of snapping techniques in an st_snap_to_network function, which can find either the nearest node or nearest point on an edge to given input geometries. Refs [#54](https://github.com/luukvdmeer/sfnetworks/issues/54) and [#57](https://github.com/luukvdmeer/sfnetworks/issues/57)
* New spatial morphers:
  - The to_spatial_smoothed morpher removes pseudo-nodes from the network. Refs [#70](https://github.com/luukvdmeer/sfnetworks/issues/70)
* Bug fixes:
  - print method of sfnetwork objects now handles networks without edges. Fixes [#69](https://github.com/luukvdmeer/sfnetworks/issues/69)
  - bug in internal utility function for retrieving node indices of edge boundaries is now fixed. Bug was identified in [#72](https://github.com/luukvdmeer/sfnetworks/issues/72)
* Improved documentation:
  - examples added to most functions. Refs [#45]((https://github.com/luukvdmeer/sfnetworks/issues/45))

# sfnetworks v0.3.1 

* Bug fixes:
  - as_sfnetwork now handles circular linestrings. Fixes [#59](https://github.com/luukvdmeer/sfnetworks/issues/59)
* Addition of a "node_key" argument to the construction functions, in line with the recent update of tidygraph. Refs [#53](https://github.com/luukvdmeer/sfnetworks/issues/53)
* Better integration with Z and M coordinates by adding them as coordinate columns when calling spatial morpher to_spatial_coordinates. Refs [#62](https://github.com/luukvdmeer/sfnetworks/issues/62)
* Improved pkgdown structure. Refs [#44](https://github.com/luukvdmeer/sfnetworks/issues/44)
* First implementation of continuous benchmarking. Refs [#6](https://github.com/luukvdmeer/sfnetworks/issues/6)

# sfnetworks v0.3.0 "Gievenbeck"

* Several spatial network extensions for the tidygraph and sf functionalities:
  - Spatial wrappers around igraph shortest paths functions. Refs [#28](https://github.com/luukvdmeer/sfnetworks/issues/28)
  - Various spatial morpher functions. Refs [#19](https://github.com/luukvdmeer/sfnetworks/issues/19)
  - Various edge measure algorithms, including edge circuity. Refs [#51](https://github.com/luukvdmeer/sfnetworks/issues/51)
* More methods for as_sfnetwork. Refs [#41](https://github.com/luukvdmeer/sfnetworks/issues/41)
  - Support [sfNetwork](https://docs.ropensci.org/stplanr/reference/SpatialLinesNetwork.html) from [stplanr](https://docs.ropensci.org/stplanr/)
  - Support [linnet](https://rdrr.io/cran/spatstat/man/linnet.html) and [psp](https://rdrr.io/cran/spatstat/man/psp.object.html) from [spatstat](https://rdrr.io/cran/spatstat/)
* Preserving sf attributes for nodes and edges inside the sfnetwork object. Refs [#24](https://github.com/luukvdmeer/sfnetworks/issues/24)
* Structural and performance improvements of the code base. This includes:
  - Construction checks are run only when needed, adding a force argument to skip validity tests.
  - Allowing to choose between spatially explicit and implicit edges during construction, adding an edges_as_lines argument. Refs [#47](https://github.com/luukvdmeer/sfnetworks/issues/47)
  - Using st_boundary to find line endpoints increases performance for sfnetwork construction from sf objects. Refs [#30](https://github.com/luukvdmeer/sfnetworks/issues/30)
  - Cleaning up sf methods for sfnetwork objects.
  - Relying on internally stored attributes rather than first extracting sf objects.
  - Option to plot without making edges explicit.
* Improved function documentation.
* An additional [vignette](https://luukvdmeer.github.io/sfnetworks/articles/extensions.html).

# sfnetworks v0.2.0 "Neutor"

* Major stable release
* Basic construction function and initial foreign objects conversion. Refs [#9](https://github.com/luukvdmeer/sfnetworks/issues/9)
* Methods for sf functions
* Roxel data as lazyData
* Internal checks before construction
* ghactions for sf and pkgdown
* Basic print and plot methods
* First vignette
* Code of conduct, contribution and license files

# sfnetworks v0.1.0

* Initial release
