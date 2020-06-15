# sfnetworks (development version)

* More methods for as_sfnetwork. Refs [#41](https://github.com/luukvdmeer/sfnetworks/issues/41)
  - Improve default method
  - Support [sfNetwork](https://docs.ropensci.org/stplanr/reference/SpatialLinesNetwork.html) from [stplanr](https://docs.ropensci.org/stplanr/)
  - Support [linnet](https://rdrr.io/cran/spatstat/man/linnet.html) and [psp](https://rdrr.io/cran/spatstat/man/psp.object.html) from [spatstat](https://rdrr.io/cran/spatstat/)
* Allow choice between spatially explicit and implicit edges. Refs [#47](https://github.com/luukvdmeer/sfnetworks/issues/47)
* Construction checks are run only when needed, adding a force argument to skip validity tests
* Performance improvements and more efficient construction
  - Using st_boundary uncreases performance for sfnetwork construction compared to lwgeom::st_startpoint and lwgeom::st_endpoint. Refs [#30](https://github.com/luukvdmeer/sfnetworks/issues/30)
* Wrapper around igraph shortest paths functions. Refs [#28](https://github.com/luukvdmeer/sfnetworks/issues/28)
* Option to plot without making edges explicit
* Enable morphing of sfnetwork object adding various spatial morphers. Refs [#19](https://github.com/luukvdmeer/sfnetworks/issues/19)

# sfnetworks v0.2.0 "Neutor"

* Major stable release
* Basic construction function and initial foreign objects conversion. Refs [#9](https://github.com/luukvdmeer/sfnetworks/issues/9)
  - Allow only points as nodes and lines as edges
  - Add edges_as_lines argument
* Methods for sf functions
* Roxel data as lazyData
* Internal checks before construction
* ghactions for sf and pkgdown
* Basic print and plot methods
* First vignette
* Code of conduct, contribution and license files

# sfnetworks v0.1.0

* Initial release
