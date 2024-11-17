# sfnetworks v1.0.0 (in progress)

### Network creation

- Creating networks directly from linestring geometries is now implemented in the new function `create_from_spatial_lines()`. The `as_sfnetwork()` method for `sf` objects will call this function when geometries are linestrings, and forward the `...` arguments to it. This now also allows to already subdivide the edges at locations where interior points are shared, setting `subdivide = TRUE`.
- Creating networks directly from point geometries is now implemented in the new function `create_from_spatial_points()`. The `as_sfnetwork()` method for `sf` objects will call this function when geometries are points, and forward the `...` arguments to it.
- There are now many more options to create spatial networks directly from spatial point data. How nodes should be connected can be specified by providing a logical adjacency matrix in addition to the point data. This adjacency matrix can also be sparse, e.g. the output of a spatial predicate function in `{sf}`. Furthermore, `{sfnetworks}` can create the adjacency matrix for you according to a specified method. In that case, you only need to specify the name of the method. Supported options are: a complete graph, a sequence, a minimum spanning tree, a delaunay triangulation, a Gabriel graph, a relative nearest neighbor graph, an a k nearest neighbor graph. See [here](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_create_represent.html#from-spatial-points) for a detailed explanation with examples.
- The new function `play_geometric()` can create random geometric networks.
- There is a new method for `as_sfnetwork()` to create `dodgr_streetnet` objects from the {dodgr} package directly into a `sfnetwork`. This internally calls `dodgr_to_sfnetwork()`. For the conversion in the other direction, use `sfnetwork_to_dodgr()`.
- It is now also possible to convert between `sfnetwork` objects and neighbor lists, using the new functions `nb_to_sfnetwork()` and `sfnetwork_to_nb()`. Neighbor lists are sparse adjacency matrices that can be found e.g. in the `{spdep}` package and the `{sf}` package (as the output of spatial predicate functions).
- Since the interpretation of the weights argument when `weights = NULL` changed (see below), the argument `length_as_weight` of the `sfnetwork()` construction function has been deprecated. Instead, you can now set `compute_length = TRUE` to store edge lengths in a attribute named *length*. However, this attribute will not anymore automatically be recognized as edge weights in routing functions.

### Routing

- As mentioned above, the interpretation of the setting `weights = NULL` has changed for all routing functions. Before, an edge attribute named *weight* would be automatically recognized as edge weights, just like in `{igraph}`. Although convenient, it proved to be very confusing since in `{tidygraph}`, the setting `weights = NULL` does not have the same meaning. There is always means that no edge weights are used, no matter if a *weight* attribute is present. We now decided that (since we are primarily integrating `{tidygraph}` and `{sf}`) to follow the `{tidygraph}` design choice, meaning that `weights = NULL` always means that no edge weights are used. However, in the routing functions of `{sfnetworks}` the default is no longer `weights = NULL`, but `weights = edge_length()`. Hence, geographic length is the default edge weight in all routing functions of `{sfnetworks}`
- The way in which edge weights can be specified in routing functions has been updated to better fit in tidy data analysis workflows. You can now directly provide edge measure functions as value to the `weight` argument. This also allows to provide custom edge measure functions, for example ones that are time-dependent. Furthermore, to reference a column in the edges table, you can now do this using tidy evaluation, i.e. unquoted column names rather than quoted ones. For dual weighted routing, the new `dual_weights()` function can be used. See [here](https://luukvdmeer.github.io/sfnetworks/articles/sfn05_routing#specifying-edge-weights) for a full overview of possible specification formats.
- The way in which *from* and *to* nodes in routing functions can be specified has been updated to better fit in tidy data analysis workflows. You can now directly provide node query or node measure functions as value to the `from` and `to` arguments. Furthermore, to reference a column in the nodes table, you can now do this using tidy evaluation, i.e. unquoted column names rather than quoted ones. See [here](https://luukvdmeer.github.io/sfnetworks/articles/sfn05_routing#specifying-origins-and-destinations) for a full overview of possible specification formats.
- It is now possible to choose between different "routing backends" through the `router` argument in all routing functions. The default routing backend is *igraph*, meaning that routing functions from the `{igraph}` package will be called internally. The second supported routing backend now is *dodgr*, which will call routing functions from the `{dodgr}` package instead. All conversion happens internally, such that as a user you can use the same functions and arguments independent from which routing engine you choose. See [here](https://luukvdmeer.github.io/sfnetworks/articles/sfn05_routing#choosing-a-routing-backend) for more details.
- The output returned by `st_network_paths()` is restructured. Instead of `tbl_df`, the function will now return a `sf` object, with the course of each path stored as a linestring geometry. It will also return the total cost of each path in a column named *cost*. Columns *node_paths* and *edge_paths* are renamed to *node_path* and *edge_path*, respectively. The boolean column *path_found* specifies if the requested path was found. If not, the path will be assigned an infinite cost and empty geometry. New boolean arguments `return_cost` and `return_geometry` can be set to `FALSE` if you do not want the cost and/or geometry columns to be returned.
- The `type` argument of `st_network_paths()` is deprecated. To compute *all* shortest paths instead of a single shortest path, set `all = TRUE` instead. Support for computing all simple paths is dropped.
- The `st_network_paths()` function now supports one-to-one k shortest paths routing. This is implemented through the new `k` argument, which you can set to an integer higher than 1.
- The `use_names` argument of `st_network_paths()` now has a default value of `FALSE`. This means that even if the nodes have a *name* column, they will be encoded by their integer indices in the output object.
- The `use_names` argument is now also added to `st_network_cost()`, letting you specify if you want node names to be used for column and rownames in the returned matrix. Also here, it defaults to `FALSE`.
- The new function `st_network_distance()` is added as a synonym for `st_network_cost()` where the edge weights are fixed to be geographic distance. This is done to provide an intuitive network-specific alternative to `sf::st_distance()`.
- The new function `st_network_travel()` now provides an interface to the `TSP` package to solve traveling salesman problems. This requires `TSP` to be installed. See [here](https://luukvdmeer.github.io/sfnetworks/articles/sfn05_routing#traveling-salesman-problem) for an example.
- The new function `st_network_iso()` now implements the computation of isodistance/isochrone polygons around a given source node. It first computes the neighborhood of the node, and then draws a concave hull around it. See [here](https://luukvdmeer.github.io/sfnetworks/articles/sfn05_routing#isodistance-polygons) for an example.

### Morphers
- The new morpher `to_spatial_unique()` allows to contract nodes at equal spatial locations, while specifying how their attributes should be combined.
- The new morpher `to_spatial_mixed()` allows to mimic a mixed network representation (i.e. a network with both directed and undirected edges) by duplicating and reversing those edges that should be undirected.
- The new morpher `to_spatial_reversed()` reverses edges, including their linestring geometries. Selected edges can be protected from reversion using the `protect` argument.
- The new morpher `to_spatial_implicit()` drops edge geometries.
- The `summarise_attributes` argument that appears in several morphers is renamed to `attribute_summary`, to avoid differences between UK and US spelling. For now, `summarise_attributes` will be automatically converted to `attribute_summary`, while giving a soft deprecation warning.
- The `to_spatial_subdivision()` morpher now has the argument `all`. If set to `TRUE`, edges will be subdivided at each interior point (i.e. creating one edge per segment), instead of only at interior points that are shared between multiple edges.
- The `to_spatial_subdivision()` morpher now has the argument `merge_equal`. If set to `TRUE`, edges will only be subvidived, but subdivision points that are shared between multiple edges will not be merged into a single node.
- The `to_spatial_subdivision()` moprher now has the argument `protect`, which allows to protect specified edges from being subdivided. The edges to be protected can be specified in several ways, e.g. by their integer index, by using edge query functions, or by referencing a column using tidy evaluation.
- The `protect` argument of the `to_spatial_smooth()` morpher is now updated to fit better in tidy data analysis workflows. Nodes to be protected from being smoothed can be specified in the same way as origins and destination nodes in routing functions, see above.
- The `require_equal` argument of the `to_spatial_smooth()` morpher is now updated to fit better in tidy data analysis workflows. Attributes to check for equality can now be specified using tidy selection. This means you can also use tidy selection helpers from `{dplyr}`.
- The `to_spatial_contracted()` morpher now has the argument `compute_centroid`. If set to `FALSE`, contracted groups of nodes will not have their centroid as new geometry, but simply the geometry of the first node in the group. This can improve performance significantly on large networks.
- The `simplify` argument of the `to_spatial_contracted()` morpher now has `TRUE` as the default value. This means that by default the contracted network will be simplified.
- The `to_spatial_shortest_paths()` morpher now automatically orders the nodes and edges in each returned network to match the order in which they are visited by the path.
- The `from` argument of `to_spatial_neighborhood()` is renamed to `node`. For now, `from` will be automatically converted to `node`, while giving a soft deprecation warning.
- The `to_spatial_neighborhood()` morpher now internally calls `st_network_cost()`, and forwards `...` arguments to it.
- The `to_spatial_neighborhood()` morpher now accepts multiple threshold values, returning one network per specified threshold.
- The internal workers of the morphers dedicated to network cleaning are now exported as well, to make it possibe to perform data cleaning outside of the tidygraph framework. These are `simplify_network()` for `to_spatial_simple()`, `subdivide_edges()` for `to_spatial_subdivision()`, `smooth_pseudo_nodes()` for `to_spatial_smooth()`, and contract nodes for `to_spatial_contracted()`.

### Spatial grouping

- The new function `group_spatial_dbscan()` provides a tidy interface to the `{dbscan}` package to group nodes spatially using the DBSCAN spatial clustering algorithm, based on network distances between nodes.

### Blending

- `st_network_blend()` now allows to blend points that have the same projected location on the network, by setting `ignore_duplicates = FALSE`. All but the first one of those will be added as isolated nodes, which can then be merged using the new morpher `to_spatial_unique()`.

### Node specific functions

- The new centrality function `centrality_straightness()` allows to compute the straightness centrality of nodes.
- The new node query function `node_is_pseudo()` and `node_is_dangling()` allow to easily query pseudo (nodes with one incoming and one outgoing edges) and dangling (nodes with a degree centrality of 1) nodes.
- The new node predicate function `node_is_nearest()` defines if a node is the nearest node to any feature in a given set of spatial features.

### Edge specific functions

- The new edge measure function `edge_segment_count()` returns the number of segments in each edge.
- The new edge predicate function `edge_is_nearest()` defines if a edge is the nearest edge to any feature in a given set of spatial features.
- Several internal functions to modify edge geometries are now exported:
  - `make_edges_valid()` makes edge geometries fit in the spatial network structure by either replacing their endpoints with the nodes that are referenced in the *from* and *to* columns (if `preserve_geometries = FALSE`), or by adding unmatched endpoints as new nodes to the network and updating the *from* and *to* columns (if `preserve_geometries = TRUE`).
  - `make_edges_directed()` turns a undirected network into a directed network by updating the *from* and *to* columns according to the direction given by the linestring geometries. This is the internal worker of the morpher `to_spatial_directed()`.
  - `make_edges_mixed()` duplicates and reverses edges in a directed network that should be undirected. This is the internal worker of the morpher `to_spatial_mixed()`.
  - `make_edges_explicit()` adds a geometry column to spatially implicit edges. This is the internal worker of the morpher `to_spatial_explicit()`.
  - `make_edges_implicit()` drops the geometry column of spatially explicit edges. This is the internal worker of the morpher `to_spatial_implicit()`.
  - `make_edges_follow_indices()` updates edge geometries in undirected networks to match the node indices specified in the *from* and *to* columns, in case they are swapped.

### Other new functions

- The new function `st_network_faces()` allows to extract the faces of a spatial network as a `sf` object with polygons geometries.
- The new function `st_project_on_network()` replaces geometries of `sf` objects with their projection on a spatial network.
- The new functions `bind_spatial_nodes()` and `bind_spatial_edges()` allow to bind additional nodes or edges to the network. These are the spatial alternatives to `tidygraph::bind_nodes()` and `tidygraph::bind_edges()`, which cannot handle geometry list columns.

### Methods for sf

- There is now a `sfnetwork` method for `sf::st_segmentize()`, allowing you to add interior points to edge geometries at fixed intervals.
- The `sfnetwork` methods for `sf::st_intersection()`,  `sf::st_difference()`, and `sf::st_crop()` now also work as expected on undirected networks.
- The `st_geometry<-` method for `sfnetwork` objects now allows to replace node geometries with any set of points, and edge geometries with any set of lines. Internally, the network structure will be kept valid by replacing endpoints of edge geometries (when replacing nodes), or by adding unmatched edge endpoints as new nodes to the network (when replacing edges).
- The `sf::st_join()` method for `sfnetwork` objects now allows multiple matches for the same node. In these case, the node will be duplicated once per additional match, and duplicates are added as isolated nodes to the resulting network.

### Upkeep with tidygraph

- Functions in `{sfnetworks}` now work well with the new concept of *focused graphs*, as recently implemented in `{tidygraph}`. See [here](https://www.data-imaginist.com/posts/2023-12-18-a-new-focus-on-tidygraph/index.html#let-us-focus-on-the-news) for details.
- There is now a `sfnetwork` method for `tidygraph::reroute()`. However, if you only want to reverse edges, we recommend to use the morpher `to_spatial_reversed()` instead, as `reroute` will only replace endpoints of edge geometries, and not reverse complete linestring geometries.
- The tidygraph verbs `morph()`, `unmorph()`, `crystallize()`, and `convert()` are now re-exported by `{sfnetworks}`, such that it is not needed anymore to load `{tidygraph}` explicitly in order to use the spatial morphers. Furthermore, the utility function `tidygraph::with_graph()` is now re-exported.

### Plotting

- The `plot()` method for `sfnetwork` objects now allows different style settings for nodes and edges, using the new `node_args` and `edge_args` arguments.
- The `plot()` method for `sfnetwork` objects now allows to plot multiple networks on top of each other.

### Data extraction utilities

- Several utility functions to extract data from a `sfnetwork` object are now exported:
  - The functions `node_data()` and `edge_data()` extract the node and edge table, respectively. Nodes are always extracted as a `sf` object. Edges are extracted as `sf` object if they are spatially explicit, and as regular `tbl_df` if they are spatially implicit.
  - The functions `node_ids()` and `edge_ids()` extract the indices of the nodes and edges, respectively. The indices correspond to rownumbers in the node and edge tables.
  - The functions `nearest_nodes()` and `nearest_edges()` return respectively the nearest nodes and nearest edges to a set of spatial features.
  - The functions `nearest_node_ids()` and `nearest_edge_ids()` return respectively the indices of the nearest nodes and nearest edges to a set of spatial features. The indices correspond to rownumbers in the node and edge tables.
  - The functions `n_nodes()` and `n_edges()` return the respectively the number of nodes and edges in the network.

### Other utilities

- Added `is_sfnetwork()` as an alias of `is.sfnetwork()`.
- The new function `validate_network()` allows to validate the spatial network structure of a `sfnetwork` object.
- The new function `wrap_igraph()` allows to wrap any function from `{igraph}` that returns a network, and make it return a `sfnetwork` object instead of a `igraph` object.
- The functions `st_duplicated()`, `st_match()` and `st_round()` are added as spatial variations to common base R functions, respectively for determining spatial duplicates, geometry matching, and coordinate rounding.

### Other updates

- When determining spatial equality of nodes, `{sfnetworks}` now by default uses a 12-digit precision. This gives a considerable performance improvement especially on large networks. Precision can be changed by explicitly setting coordinate precision using `sf::st_set_precision()`.
- All messages, warnings and errors in `{sfnetworks}` are now raised using the `{cli}` and `{rlang}` packages.

### Bug fixes

- The print method now works correctly again after aligning with updates in `{tidygraph}`.
- The morpher `to_spatial_contracted()` now correctly handles group indices that are not ordered.
- The `plot()` method for `sfnetwork` objects now correctly plots networks with spatially implicit edges that are active.
- `st_network_bbox()` now also computes bounding boxes for networks with spatially implicit edges.

### Dependencies
- The minimum required version for `{sf}` is now 1.0-11
- The minimum required version for `{tidygraph}` is now 1.3.0
- The minimum required version for `{igraph}` is now 2.1.0
- The `{crayon}` package is not a dependency anymore.
- Base R packages `{methods}` and `{stats}` are added as new dependencies.
- Additional packages `{cli}`, `{lifecycle}`, `{pillar}` and `{tidyselect}` are added as new dependencies.

# sfnetworks v0.6.4

### New features

* The `sfnetwork()` construction function now has an argument `message` which can be set to `FALSE` when the network validity checks should not print informational messages to the console. Refs [#261](https://github.com/luukvdmeer/sfnetworks/issues/261).

### Maintenance

* Code and documentation was updated where needed to align with changes in base R and/or package dependencies. No changes to program logic or behavior.

# sfnetworks v0.6.3

### Bug fixes

* Argument names of sfnetwork S3 methods for `sf::st_geometry()`, `sf::st_bbox()`, `sf::st_m_range()` and `sf::st_set_precision()` are updated to be consistent with their corresponding generic functions in sf.
* Arguments `active` and `...` are removed from the sfnetwork S3 method for `sf::st_precision()` to be consistent with its corresponding generic function in sf.
* Argument `active` is removed from the sfnetwork S3 method for `sf::st_crs()` to be consistent with the above-mentioned change regarding `sf::st_precision()` (since both CRS and precision can not differ between nodes and edges).

# sfnetworks v0.6.2

### Bug fixes

* The `to_spatial_contracted()` morpher now correctly handles cases for undirected networks in which only loop edges are created after contraction. Refs [#237](https://github.com/luukvdmeer/sfnetworks/issues/237).

### Refactoring

* The `to_spatial_contracted()` morpher now directly returns the original network when none of the contraction groups contain more than one node.

### Other

* Umbrella packages tidyverse and spatstat are no longer suggested packages. Only individual members of these packages are now suggested packages.
* Updated unit tests.

# sfnetworks v0.6.1

### Bug fixes

* Unit tests in `test_join.R` now successfully run also on R-devel.

### Refactoring

* Updated plot algorithm to be faster and more efficient. Refs [#226](https://github.com/luukvdmeer/sfnetworks/issues/226).

# sfnetworks v0.6.0 "Coerde"

### New features

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
* Edge measure function `edge_azimuth()` gained an argument `degrees` which can be set to `TRUE` to return angles in degrees instead of radians.
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
* Master branch renamed to main, and protected from directed pushes.

# sfnetworks v0.5.5

* Adjusted the code used to check the version of PROJ on attach. In particular, the new approach tests only `sf::sf_extSoftVersion()["proj.4"]` since `sf::sf_extSoftVersion()["PROJ"]` might not be defined for sf < 1.0. Refs [#198](https://github.com/luukvdmeer/sfnetworks/issues/198) and [#200](https://github.com/luukvdmeer/sfnetworks/issues/200). 
* Adjusted one of the vignettes following the changes in `dplyr` 1.0.8. Ref [#202](https://github.com/luukvdmeer/sfnetworks/pull/202). Thanks @romainfrancois. 
* Removed conflicting URL from package documentation `spatial_edge_measures`
  
# sfnetworks v0.5.4

* A startup message is included to urge users of PROJ <= 6 to recreate the CRS for the `Roxel` dataset. Refs [#190](https://github.com/luukvdmeer/sfnetworks/issues/190) and fixed with [#193](https://github.com/luukvdmeer/sfnetworks/pull/193).
* Example using GraphML in [vignette 1](https://luukvdmeer.github.io/sfnetworks/articles/sfn01_structure.html) has been removed provisionally to address [#194](https://github.com/luukvdmeer/sfnetworks/issues/194).

# sfnetworks v0.5.3

* Addition of the `n_active` and `n_inactive` arguments to the print method of an sfnetwork object. The arguments define how many rows are printed for respectively the active and inactive network element. The values of these arguments can also be set globally by running e.g. `options(sfn_max_print_active = 1, sfn_max_print_inactive = 2)`. Refs [#157](https://github.com/luukvdmeer/sfnetworks/issues/157)
* The example dataset `Roxel` is updated to comply with recent updates on the way a CRS is specified in an sf object. Refs [#167](https://github.com/luukvdmeer/sfnetworks/issues/167)
* GitHub Actions workflows are updated to comply with new developments.
* Documentation updates:
  - Vignette file names are updated such that the appear in correct order on CRAN. Refs [#162](https://github.com/luukvdmeer/sfnetworks/issues/162)
  - Example section of the plot method for sfnetwork objects now includes an example of how to add graticules and axes. Refs [#159](https://github.com/luukvdmeer/sfnetworks/issues/159)

# sfnetworks v0.5.2

* Compatibility with `s2` by adding a `s2::as_s2_geography()` method for sfnetwork objects. In the new version of `sf`, the `s2` package will be used for geometric operations involving longitude-latitude coordinates, see [here](https://github.com/r-spatial/sf/issues/1649).
* Bug fixes:
  - When setting `length_as_weight = TRUE` in the sfnetwork construction function, the added weight column now preserves specification of units.
  - `st_network_blend()` now internally uses `sf::st_cast()` instead of `sfheaders::sfc_cast()` to avoid errors with some CRS specifications.
* Documentation updates:
  - Extended documentation for the shortest paths functions.
  - Clear mention in vignettes that `tidygraph` behavior regarding the `weight` attribute settings is sometimes differing from `igraph`.

# sfnetworks v0.5.1

* Compatibility with `spatstat v2`, which is now splitted into multiple sub-packages. See [here](https://github.com/spatstat/spatstat/tree/v1.64-2#spatstat-is-now-split-into-several-packages) for details. In `sfnetworks`, this affected the functions `as_sfnetwork.linnet()`, `as_sfnetwork.psp()` and `as.linnet.sfnetwork()`. Using this functions now requires `spatstat >= 2.0.0` and `sf >= 0.9.8`.
* Bug fixes:
  - Usage of `match` for checking coordinate equality is replaced by a new `st_match` function specifically designed for this task. This fixes bugs related to numeric approximations of detailed coordinates. See [#130](https://github.com/luukvdmeer/sfnetworks/issues/130)
* Documentation updates:
  - It is now clearly documented that using `sf::st_reverse()` to reverse edge linestrings is only possible with GEOS versions >= 3.7.

# sfnetworks v0.5.0 "Nienberge"

* Addition of a `to_spatial_contracted()` morpher, to contract groups of nodes based on given grouping variables. Refs [#104](https://github.com/luukvdmeer/sfnetworks/issues/104)
* Addition of a `to_spatial_neighborhood()` morpher, to limit a network to the neighborhood of a given node, based on a given cost threshold. Refs [#90](https://github.com/luukvdmeer/sfnetworks/issues/90)
* New implementation of `st_network_blend()`, which is faster and more reliable. The `sort` argument is deprecated, since the returned network is now always sorted.
* Addition of an `summarise_attributes` argument to the `to_spatial_simple()` morpher, allowing to specify on a per-attribute basis how attribute values of merged multiple edges should be inferred from the original ones. Refs [#113](https://github.com/luukvdmeer/sfnetworks/issues/113). The same argument is also part of the new `to_spatial_contracted()` morpher, where it can be used to specify on a per-attribute basis how attribute values of contracted groups of nodes should be inferred from the original ones.
* The argument `remove_parallels` of the `to_spatial_simple()` morpher is renamed to `remove_multiples` to better fit naming conventions in `igraph`.
* The argument `store_orig_data` of the `to_spatial_smooth()` morpher is renamed to `store_original_data` to be better interpretable. This argument is also added to the morphers `to_spatial_simple()` and `to_spatial_contracted()`, allowing to store original node or edge data in a `.orig_data` column, matching the design standards of `tidygraph`.
* Addition of a `Inf_as_NaN` argument to `st_network_cost()`, to store cost values of paths between unconnected edges as `NaN` instead of `Inf`. The default value of this argument is `FALSE`. Refs [#111](https://github.com/luukvdmeer/sfnetworks/issues/111)
* The default of the `Inf_as_NaN` argument in `edge_circuity()` is changed from `TRUE` to `FALSE`, to better fit with the change mentioned above, and to make sure no changes to R defaults are made without the user explicitly specifying them.
* Whenever there are multiple matches when spatially joining information to the nodes of a network with `sf::st_join()`, only the information of the first match is now joined. Before, this used to throw an error. Refs [#108](https://github.com/luukvdmeer/sfnetworks/discussions/108)
* Removal of the morphed_sfnetwork method for `sf::st_geometry<-`, since geometries should not be replaced in a morphed state.
* The warning '.. assumes attributes are constant over geometries' is now only raised when not all attribute-geometry relationships are set to 'constant'. Refs [#123](https://github.com/luukvdmeer/sfnetworks/issues/123)
* Bug fixes:
  - The attribute-geometry relationships of edge attributes are now preserved during network construction. Fixes [#123](https://github.com/luukvdmeer/sfnetworks/issues/123)
  - `st_network_blend()` now correctly blends points that are very close to the network. Fixes [#98](https://github.com/luukvdmeer/sfnetworks/issues/98)
  - `st_network_blend()` now preserves the directedness of the input network. Fixes [#127](https://github.com/luukvdmeer/sfnetworks/issues/127)
  - `st_network_blend()` now runs even if the network contains edges of length 0. Fixes [#125](https://github.com/luukvdmeer/sfnetworks/issues/125)
  - The sfnetwork method for `sf::st_crop()` now correctly updates the nodes table after cropping the edges. Fixes [#109](https://github.com/luukvdmeer/sfnetworks/issues/109)
  - `to_spatial_smooth()` now returns the original network when no pseudo nodes are present. Fixes [#112](https://github.com/luukvdmeer/sfnetworks/issues/112)
  - `to_spatial_subdivision()` now returns the original network when there are no locations for subdivision.
  - `to_spatial_subdivision()` now returns correct node indices for undirected networks.
* Several new examples and applications added to the vignettes.

# sfnetworks v0.4.1

* Addition of an edge measure function `edge_azimuth()`, to calculate the azimuth (i.e. bearing) of edges. Refs [#107](https://github.com/luukvdmeer/sfnetworks/issues/107)
* Addition of a `to_spatial_transformed()` morpher, to temporarily transform a sfnetwork into a different CRS.
* Addition of a sfnetwork methods for `linnet` objects, to enhance interoperability between `sfnetworks` and the `spatstat` package for spatial point patterns on linear networks.
* Addition of an `Inf_as_NaN` argument to the `edge_circuity()` function, to store circuity values of loop edges as `NaN` instead of `Inf`. The default value of this argument is `TRUE`.
* Changes to `st_network_paths()`:
  - Addition of a new argument `type`, which lets you set the type of paths calculation that should be performed.
  - To calculate all shortest paths between nodes, now set `type = 'all_shortest'` instead of `all = TRUE`. The latter argument is deprecated.
  - Besides shortest paths, there is now also the possibility to calculate all simple paths between nodes, by setting `type = 'all_simple'`. Be aware that computation time gets high when providing a lot of 'to' nodes, or when the network is large and dense. Refs [#105](https://github.com/luukvdmeer/sfnetworks/issues/105)
  - Whenever `weights = NULL` *and* there is no column named 'weight' in the edges table, geographic edge length will be calculated internally and used as weights in shortest path calculation. Before, paths would be calculated without edge weights in this case. Refs [#106](https://github.com/luukvdmeer/sfnetworks/issues/106)
  - Whenever the given 'from' and/or 'to' nodes contain NA values and/or empty point geometries, igraph behaviour is now replicated by throwing an error. Before, these values would simply be ignored.
* Performance improvement of the `to_spatial_smooth()` morpher. As a result of this it does not store the original edge data anymore in a '.orig_data' column. Instead, non-merged edges keep their attributes, while merged edges loose their attributes. The '.orig_data' column can still be added by setting `store_orig_data = TRUE`, but this is not the default.
* Bug fixes:
  - `st_network_paths()` now correctly handles cases where an unexisting column is passed to the `weights` argument, by throwing an error. Fixes [#99](https://github.com/luukvdmeer/sfnetworks/issues/99)
  - The sfnetwork method for `sf::st_join()` now correctly handles inner joins (i.e. joins where `left = FALSE`).
* Addition of extra examples to the [routing](https://luukvdmeer.github.io/sfnetworks/articles/sfn04_routing.html) and [spatial morphers](https://luukvdmeer.github.io/sfnetworks/articles/sfn05_morphers.html) vignettes.
* Test coverage increased to +/- 86%.

# sfnetworks v0.4.0 "Hiltrup"

* Backward incompatible changes to existing functions:
  - Function `st_shortest_paths()` and `st_all_shortest_paths()` are now merged into a single function `st_network_paths()`. By default it call `igraph::shortest_paths()` internally. Setting `all = TRUE` will make it call `igraph::all_shortest_paths()` instead.
  - The output format of this `st_network_paths()` function is different from its predecessors. It returns a tibble instead of a list, to fit better in tidyverse workflows. See [#77](https://github.com/luukvdmeer/sfnetworks/issues/77)
  - The `snap` argument is removed from all shortest paths related functions, which will now always snap geospatial points provides as from or to locations to their nearest node on the network before calculating paths.
  - The `keep` argument is removed from the `to_spatial_simple()` morpher. It is now recommended to first sort data with `dplyr::arrange()` before calling the morpher.
  - The spatial morpher `to_spatial_coordinates()` is deprecated. Use the new node coordinate query functions instead.
  - The spatial morpher `to_spatial_dense_graph()` is deprecated. A new morpher `to_spatial_subdivision()`, with slightly different functionality, is added instead.
  - The spatial morpher `to_spatial_implicit_edges()` is deprecated. Use `sf::st_set_geometry()` instead, with activated edges and value `NULL`.
  - Functions `st_network_distance()`, `edge_straight_length()` and `to_spatial_explicit_edges()` are renamed to respectively `st_network_cost()`, `edge_displacement()` and `to_spatial_explicit()`, which either reflects their purpose better or fits better into the naming conventions within the package.
  - Function arguments that were named `graph` are renamed to `x`, to have more consistency across the package.
* Backward compatible changes to existing functions:
  - The construction function `sfnetwork()` now has an argument `length_as_weight` that, if set to TRUE, will add the lengths of edges as a weight attribute to the edges data. Refs [#65](https://github.com/luukvdmeer/sfnetworks/issues/65)
  - There is now an `as_sfnetwork()` method for sfc objects. Refs [#41](https://github.com/luukvdmeer/sfnetworks/issues/41)
  - All existing `st_network_*` functions in sfnetwork are now generic, such that they can easily be modified by extensions of sfnetwork objects. Refs [#80](https://github.com/luukvdmeer/sfnetworks/issues/80)
  - The `to_spatial_explicit_edges()` morpher now accepts arguments that are forwarded directly to `sf::st_as_sf()`. Refs [#83](https://github.com/luukvdmeer/sfnetworks/issues/83)
  - Functions that split edges now give a warning that attributes are assumed to be constant. Refs [#84](https://github.com/luukvdmeer/sfnetworks/issues/84)
  - The `edge_length()` function can now also be applied to spatially implicit edges.
  - The sfnetwork methods for `sf::st_as_sf()`, `sf::st_geometry()` and `sf::st_agr()` now have an argument `active` to directly retrieve information from a network element without activating it. Use as `st_as_sf(x, active = "nodes")`, et cetera.
  - Character encoded node names can now be provided as from and to locations to the shortest path functions.
* New functions:
  - The new function `st_network_blend()` implements a process that we called 'blending points into a network'. The functions accepts a network and a set of points. For each point p in the set of given points, it finds the projection p\* of p on the network, splits the edges of the network at the location of p\*, and finally adds p\* along with the attributes of p as a node to the network. Refs [#27](https://github.com/luukvdmeer/sfnetworks/issues/27) and [#54](https://github.com/luukvdmeer/sfnetworks/issues/27)
  - The new function `st_network_join()` does a network specific join of two sfnetworks. It combines a spatial full join on the nodes data with a bind_rows operation on the edges data, and updates the from and to indices of the edges accordingly. Refs [#22](https://github.com/luukvdmeer/sfnetworks/issues/22)
  - The new function `st_network_bbox()` calculates the bounding box of the whole network by combining the bounding boxes of nodes and edges.
  - The new spatial morpher `to_spatial_subdivision()` subdivides edges at locations where an interior point is shared with either another interior point or endpoint of another edge. Refs [#73](https://github.com/luukvdmeer/sfnetworks/issues/73)
  - The new spatial morpher `to_spatial_smooth()` iteratively removes pseudo-nodes from the network. Refs [#70](https://github.com/luukvdmeer/sfnetworks/issues/70)
  - Several spatial predicate functions are implemented as node and edge query functions, to interpret spatial relations between network elements and other geospatial features directly inside tidy filter and mutate calls. Refs [#60](https://github.com/luukvdmeer/sfnetworks/issues/60)
  - Node coordinate query functions `node_X()`, `node_Y()`, `node_Z()` and `node_M()` are implemented to query specific coordinate values from the nodes.
  - There is now an `ggplot2::autoplot()` method for sfnetworks, allowing to easily plot a sfnetwork as a ggplot2 object. Refs [#86](https://github.com/luukvdmeer/sfnetworks/issues/86)
  - There is now a `print` method for morphed sfnetworks. Refs [#88](https://github.com/luukvdmeer/sfnetworks/issues/88)
  - There are now morphed sfnetworks method for `sf::st_geometry<-()`, `sf::st_join()`, `sf::st_filter()` and `sf::st_crop()`. Refs [#85](https://github.com/luukvdmeer/sfnetworks/issues/85)
* Bug fixes:
  - Networks can now be constructed by only providing nodes, and no edges. Fixes [#81](https://github.com/luukvdmeer/sfnetworks/issues/81)
  - The print method for sfnetwork objects now correctly handles networks without edges as well as completely empty networks. Fixes [#69](https://github.com/luukvdmeer/sfnetworks/issues/69) and [#89](https://github.com/luukvdmeer/sfnetworks/issues/89)
  - The shortest path functions now correctly handle empty geometries. Fixes [#87](https://github.com/luukvdmeer/sfnetworks/issues/87)
* Documentation improvements:
  - Examples are added to the function documentations. Refs [#45](https://github.com/luukvdmeer/sfnetworks/issues/45)
  - The existing vignettes are reorganized, and combined with a lot of new information into five new vignettes. Refs [#92](https://github.com/luukvdmeer/sfnetworks/issues/92)
* Together with the documentation improvements, several new units tests brought the test coverage to +/- 80%.
* The internal code base is completely restructured, such that it is more performant and easier to read, debug and extend.

# sfnetworks v0.3.1

* Bug fixes:
  - `as_sfnetwork()` now handles circular linestrings. Fixes [#59](https://github.com/luukvdmeer/sfnetworks/issues/59)
* Addition of a "node_key" argument to the construction functions, in line with the recent update of tidygraph. Refs [#53](https://github.com/luukvdmeer/sfnetworks/issues/53)
* Better integration with Z and M coordinates by adding them as coordinate columns when calling spatial morpher `to_spatial_coordinates()`. Refs [#62](https://github.com/luukvdmeer/sfnetworks/issues/62)
* Improved pkgdown structure. Refs [#44](https://github.com/luukvdmeer/sfnetworks/issues/44)
* First implementation of continuous benchmarking. Refs [#6](https://github.com/luukvdmeer/sfnetworks/issues/6)

# sfnetworks v0.3.0 "Gievenbeck"

* Several spatial network extensions for the tidygraph and sf functionalities:
  - Spatial wrappers around igraph shortest paths functions. Refs [#28](https://github.com/luukvdmeer/sfnetworks/issues/28)
  - Various spatial morpher functions. Refs [#19](https://github.com/luukvdmeer/sfnetworks/issues/19)
  - Various edge measure algorithms, including edge circuity. Refs [#51](https://github.com/luukvdmeer/sfnetworks/issues/51)
* More methods for `as_sfnetwork()`. Refs [#41](https://github.com/luukvdmeer/sfnetworks/issues/41)
  - Support `sfNetwork` from [stplanr](https://docs.ropensci.org/stplanr/)
  - Support [linnet](https://rdrr.io/cran/spatstat.linnet/man/linnet.html) and [psp](https://rdrr.io/cran/spatstat.geom/man/psp.object.html) from [spatstat](https://rdrr.io/cran/spatstat/)
* Preserving sf attributes for nodes and edges inside the sfnetwork object. Refs [#24](https://github.com/luukvdmeer/sfnetworks/issues/24)
* Structural and performance improvements of the code base. This includes:
  - Construction checks are run only when needed, adding a force argument to skip validity tests.
  - Allowing to choose between spatially explicit and implicit edges during construction, adding an edges_as_lines argument. Refs [#47](https://github.com/luukvdmeer/sfnetworks/issues/47)
  - Using st_boundary to find line endpoints increases performance for sfnetwork construction from sf objects. Refs [#30](https://github.com/luukvdmeer/sfnetworks/issues/30)
  - Cleaning up sf methods for sfnetwork objects.
  - Relying on internally stored attributes rather than first extracting sf objects.
  - Option to plot without making edges explicit.
* Improved function documentation.
* An additional vignette "Extensions" was included.

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

# sfnetworks v0.1.0 "Altstadt"

* Initial release
