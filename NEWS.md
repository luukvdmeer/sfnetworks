# development

* Backward incompatible changes to existing functions:
  - The output format of `st_shortest_paths` and `st_all_shortest_paths` changed to fit better in tidyverse workflows. See [#77](https://github.com/luukvdmeer/sfnetworks/issues/77)
  - The 'snap' argument is removed from the shortest paths related functions, which will now always snap points to the nearest node on the network.
  - Functions `st_network_distance`, `edge_straight_length` and the spatial morpher `to_spatial_dense_graph` are renamed to respectively `st_cost`, `edge_displacement` and `to_spatial_subdivision`, which reflects their purpose better and does not interfere with existing graph theory terminology.
  - Function arguments that were named 'graph' are renamed to 'x', to have more consistency accross the package.
* Backward compatible changes to existing functions:
  - The construction function `sfnetwork` now has an argument 'length_as_weight' that, if set to TRUE, will add the lengths of edges as a weight attribute to the edges data. Refs [#65](https://github.com/luukvdmeer/sfnetworks/issues/65)
  - There is now an `as_sfnetwork` method for `sfc` objects. Refs [#41](https://github.com/luukvdmeer/sfnetworks/issues/41)
  - All existing 'st_' functions in sfnetwork are now generic, such that they can easily be modified by extensions of sfnetwork objects. Refs [#80](https://github.com/luukvdmeer/sfnetworks/issues/80)
  - The `to_spatial_explicit_edges` morpher now accepts arguments that are forwarded directly to `st_as_sf`. Refs [#83](https://github.com/luukvdmeer/sfnetworks/issues/83)
  - Functions that split edges now give a warning that attributes are assumed to be constant. Refs [#84](https://github.com/luukvdmeer/sfnetworks/issues/84)
  - The `edge_length` function can now also be applied to spatially implicit edges.
* New functions:
    - The new function `st_blend` implements a process that we called 'blending points into a network'. The functions accepts a network and a set of points. For each point p in the set of given points, it finds the projection p\* of p on the network, splits the edges of the network at the location of p\*, and finally adds p\* along with the attributes of p as a node to the network. Refs [#27](https://github.com/luukvdmeer/sfnetworks/issues/27) and [#54](https://github.com/luukvdmeer/sfnetworks/issues/27)
    - The new function `st_network_join` does a network specific join of two sfnetworks. It combines a spatial full join on the nodes data with a bind_rows operation on the edges data, and updates the from and to indices of the edges accordingly. Refs [#22](https://github.com/luukvdmeer/sfnetworks/issues/22)
    - The new spatial morpher `to_spatial_smooth` iteratively removes pseudo-nodes from the network. Refs [#70](https://github.com/luukvdmeer/sfnetworks/issues/70)
    - Several spatial predicates are implemented as node and edge query functions, to interpretate spatial relations between network elements and other geospatial features directly inside tidy filter and mutate calls. Refs [#60](https://github.com/luukvdmeer/sfnetworks/issues/60)
    - There is now an `autoplot` method for sfnetworks, allowing to easily plot a sfnetwork as a `ggplot2` object. Refs [#86](https://github.com/luukvdmeer/sfnetworks/issues/86)
* Bug fixes:
  - Networks can now be constructed by only providing nodes, and no edges. Refs [#81](https://github.com/luukvdmeer/sfnetworks/issues/81)
  - The print method for sfnetwork objects now correctly handles networks without edges as well as completely empty networks. Fixes [#69](https://github.com/luukvdmeer/sfnetworks/issues/69) and [#89](https://github.com/luukvdmeer/sfnetworks/issues/89)
  - The `to_spatial_simple` morpher now correctly handles parallel edges of the same length. Fixes [#75](https://github.com/luukvdmeer/sfnetworks/issues/75)
  - The shortest path functions now correctly handle empty geometries. Refs [#87](https://github.com/luukvdmeer/sfnetworks/issues/87)
* Documentation improvements:
  - Examples are added to the function documentations. Refs [#45]((https://github.com/luukvdmeer/sfnetworks/issues/45))
* The internal code base is completely restructured, such that it is more performant and easier to read, debug and extend.

# sfnetworks v0.3.1 

* Bug fixes:
  - `as_sfnetwork` now handles circular linestrings. Fixes [#59](https://github.com/luukvdmeer/sfnetworks/issues/59)
* Addition of a "node_key" argument to the construction functions, in line with the recent update of tidygraph. Refs [#53](https://github.com/luukvdmeer/sfnetworks/issues/53)
* Better integration with Z and M coordinates by adding them as coordinate columns when calling spatial morpher `to_spatial_coordinates`. Refs [#62](https://github.com/luukvdmeer/sfnetworks/issues/62)
* Improved pkgdown structure. Refs [#44](https://github.com/luukvdmeer/sfnetworks/issues/44)
* First implementation of continuous benchmarking. Refs [#6](https://github.com/luukvdmeer/sfnetworks/issues/6)

# sfnetworks v0.3.0 "Gievenbeck"

* Several spatial network extensions for the tidygraph and sf functionalities:
  - Spatial wrappers around igraph shortest paths functions. Refs [#28](https://github.com/luukvdmeer/sfnetworks/issues/28)
  - Various spatial morpher functions. Refs [#19](https://github.com/luukvdmeer/sfnetworks/issues/19)
  - Various edge measure algorithms, including edge circuity. Refs [#51](https://github.com/luukvdmeer/sfnetworks/issues/51)
* More methods for `as_sfnetwork`. Refs [#41](https://github.com/luukvdmeer/sfnetworks/issues/41)
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
