#' Create a sfnetwork
#'
#' \code{sfnetwork} is a tidy data structure for geospatial networks. It
#' extends the \code{\link[tidygraph]{tbl_graph}} data structure for
#' relational data into the domain of geospatial networks, with nodes and
#' edges embedded in geographical space, and offers smooth integration with
#' \code{\link[sf]{sf}} for spatial data analysis.
#'
#' @param nodes The nodes of the network. Should be an object of class
#' \code{\link[sf]{sf}}, or directly convertible to it using
#' \code{\link[sf]{st_as_sf}}. All features should have an associated geometry
#' of type \code{POINT}.
#'
#' @param edges The edges of the network. May be an object of class
#' \code{\link[sf]{sf}}, with all features having an associated geometry of
#' type \code{LINESTRING}. It may also be a regular \code{\link{data.frame}} or
#' \code{\link[tibble]{tbl_df}} object. In any case, the nodes at the ends of
#' each edge must be referenced in a \code{to} and \code{from} column, as
#' integers or characters. Integers should refer to the position of a node in
#' the nodes table, while characters should refer to the name of a node stored
#' in the column referred to in the \code{node_key} argument. Setting edges to
#' \code{NULL} will create a network without edges.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param node_key The name of the column in the nodes table that character
#' represented \code{to} and \code{from} columns should be matched against. If
#' \code{NA}, the first column is always chosen. This setting has no effect if
#' \code{to} and \code{from} are given as integers. Defaults to \code{'name'}.
#'
#' @param edges_as_lines Should the edges be spatially explicit, i.e. have
#' \code{LINESTRING} geometries stored in a geometry list column? If
#' \code{NULL}, this will be automatically defined, by setting the argument to
#' \code{TRUE} when the edges are given as an object of class
#' \code{\link[sf]{sf}}, and \code{FALSE} otherwise. Defaults to \code{NULL}.
#'
#' @param compute_length Should the geographic length of the edges be stored in
#' a column named \code{length}? Uses \code{\link[sf]{st_length}} to compute
#' the length of the edge geometries when edges are spatially explicit, and
#' \code{\link[sf]{st_distance}} to compute the distance between boundary nodes
#' when edges are spatially implicit. If there is already a column named
#' \code{length}, it will be overwritten. Please note that the values in this
#' column are \strong{not} automatically recognized as edge weights. This needs
#' to be specified explicitly when calling a function that uses edge weights.
#' Defaults to \code{FALSE}.
#'
#' @param length_as_weight Deprecated, use \code{compute_length} instead.
#'
#' @param force Should network validity checks be skipped? Defaults to
#' \code{FALSE}, meaning that network validity checks are executed when
#' constructing the network. These checks guarantee a valid spatial network
#' structure. For the nodes, this means that they all should have \code{POINT}
#' geometries. In the case of spatially explicit edges, it is also checked that
#' all edges have \code{LINESTRING} geometries, nodes and edges have the same
#' CRS and boundary points of edges match their corresponding node coordinates.
#' These checks are important, but also time consuming. If you are already sure
#' your input data meet the requirements, the checks are unnecessary and can be
#' turned off to improve performance.
#'
#' @param message Should informational messages (those messages that are
#' neither warnings nor errors) be printed when constructing the network?
#' Defaults to \code{TRUE}.
#'
#' @param ... Arguments passed on to \code{\link[sf]{st_as_sf}}, if nodes need
#' to be converted into an \code{\link[sf]{sf}} object during construction.
#'
#' @return An object of class \code{sfnetwork}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' p1 = st_point(c(7, 51))
#' p2 = st_point(c(7, 52))
#' p3 = st_point(c(8, 52))
#' nodes = st_as_sf(st_sfc(p1, p2, p3, crs = 4326))
#'
#' e1 = st_cast(st_union(p1, p2), "LINESTRING")
#' e2 = st_cast(st_union(p1, p3), "LINESTRING")
#' e3 = st_cast(st_union(p3, p2), "LINESTRING")
#' edges = st_as_sf(st_sfc(e1, e2, e3, crs = 4326))
#' edges$from = c(1, 1, 3)
#' edges$to = c(2, 3, 2)
#'
#' # Default.
#' sfnetwork(nodes, edges)
#'
#' # Undirected network.
#' sfnetwork(nodes, edges, directed = FALSE)
#'
#' # Using character encoded from and to columns.
#' nodes$name = c("city", "village", "farm")
#' edges$from = c("city", "city", "farm")
#' edges$to = c("village", "farm", "village")
#' sfnetwork(nodes, edges, node_key = "name")
#'
#' # Spatially implicit edges.
#' sfnetwork(nodes, edges, edges_as_lines = FALSE)
#'
#' # Store edge lenghts in a column named 'length'.
#' sfnetwork(nodes, edges, compute_length = TRUE)
#'
#' @importFrom igraph edge_attr<-
#' @importFrom lifecycle deprecated
#' @importFrom sf st_as_sf
#' @importFrom tidygraph tbl_graph with_graph
#' @export
sfnetwork = function(nodes, edges = NULL, directed = TRUE, node_key = "name",
                     edges_as_lines = NULL, compute_length = FALSE,
                     length_as_weight = deprecated(),
                     force = FALSE, message = TRUE, ...) {
  if (isTRUE(length_as_weight)) deprecate_length_as_weight("sfnetwork")
  # Prepare nodes.
  # If nodes is not an sf object:
  # --> Try to convert it to an sf object.
  # --> Arguments passed in ... will be passed on to st_as_sf.
  if (! is_sf(nodes)) {
    nodes = tryCatch(
      st_as_sf(nodes, ...),
      error = function(e) {
        stop(
          "Failed to convert nodes to sf object because: ",
          e,
          call. = FALSE
        )
      }
    )
  }
  # Create network.
  x_tbg = tbl_graph(nodes, edges, directed, node_key)
  x_sfn = structure(x_tbg, class = c("sfnetwork", class(x_tbg)))
  # Post-process network. This includes:
  # --> Checking if the network has a valid spatial network structure.
  # --> Making edges spatially explicit or implicit if requested.
  # --> Adding additional attributes if requested.
  if (is_sf(edges)) {
    # Add sf attributes to the edges table.
    # They were removed when creating the tbl_graph.
    edge_geom_colname(x_sfn) = attr(edges, "sf_column")
    edge_agr(x_sfn) = attr(edges, "agr")
    # Remove edge geometries if requested.
    if (isFALSE(edges_as_lines)) {
      x_sfn = implicitize_edges(x_sfn)
    }
    # Run validity check after implicitizing edges.
    if (! force) validate_network(x_sfn, message = message)
  } else {
    # Run validity check before explicitizing edges.
    if (! force) validate_network(x_sfn, message = message)
    # Add edge geometries if requested.
    if (isTRUE(edges_as_lines)) {
      x_sfn = explicitize_edges(x_sfn)
    }
  }
  if (compute_length) {
    if ("length" %in% names(edges)) {
      raise_overwrite("length")
    }
    edge_attr(x_sfn, "length") = with_graph(x_sfn, edge_length())
  }
  x_sfn
}

# Simplified construction function.
# Must be sure that nodes and edges together form a valid sfnetwork.
# ONLY FOR INTERNAL USE!

#' @importFrom tidygraph tbl_graph
sfnetwork_ = function(nodes, edges = NULL, directed = TRUE) {
  x_tbg = tbl_graph(nodes, edges, directed)
  if (! is.null(edges)) {
    edge_geom_colname(x_tbg) = attr(edges, "sf_column")
    edge_agr(x_tbg) = attr(edges, "agr")
  }
  structure(x_tbg, class = c("sfnetwork", class(x_tbg)))
}

# Fast function to convert from tbl_graph to sfnetwork.
# Must be sure that tbl_graph has already a valid sfnetwork structure.
# ONLY FOR INTERNAL USE!

tbg_to_sfn = function(x) {
  class(x) = c("sfnetwork", class(x))
  x
}

#' Convert a foreign object to a sfnetwork
#'
#' Convert a given object into an object of class \code{\link{sfnetwork}}.
#'
#' @param x Object to be converted into a \code{\link{sfnetwork}}.
#'
#' @param ... Additional arguments passed on to the \code{\link{sfnetwork}}
#' construction function, unless specified otherwise.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @export
as_sfnetwork = function(x, ...) {
  UseMethod("as_sfnetwork")
}

#' @describeIn as_sfnetwork By default, the provided object is first converted
#' into a \code{\link[tidygraph]{tbl_graph}} using
#' \code{\link[tidygraph]{as_tbl_graph}}. Further conversion into an
#' \code{\link{sfnetwork}} will work as long as the nodes can be converted to
#' an \code{\link[sf]{sf}} object through \code{\link[sf]{st_as_sf}}. Arguments
#' to \code{\link[sf]{st_as_sf}} can be provided as additional arguments and
#' will be forwarded to \code{\link[sf]{st_as_sf}} through the
#' code{\link{sfnetwork}} construction function.
#'
#' @importFrom tidygraph as_tbl_graph
#' @export
as_sfnetwork.default = function(x, ...) {
  as_sfnetwork(as_tbl_graph(x), ...)
}

#' @describeIn as_sfnetwork Convert spatial features of class
#' \code{\link[sf]{sf}} directly into a \code{\link{sfnetwork}}.
#' Supported geometry types are either \code{LINESTRING} or \code{POINT}. In
#' the first case, the lines become the edges in the network, and nodes are
#' placed at their boundaries. Additional arguments are forwarded to
#' \code{\link{create_from_spatial_lines}}. In the latter case, the points
#' become the nodes in the network, and are connected by edges according to a
#' specified method. Additional arguments are forwarded to
#' \code{\link{create_from_spatial_points}}.
#'
#' @examples
#' # From an sf object with LINESTRING geometries.
#' library(sf, quietly = TRUE)
#'
#' as_sfnetwork(roxel)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#'
#' plot(st_geometry(roxel))
#' plot(as_sfnetwork(roxel))
#'
#' par(oldpar)
#'
#' # From an sf object with POINT geometries.
#' # For more examples see create_from_spatial_points.
#' library(sf, quietly = TRUE)
#'
#' pts = st_centroid(roxel[10:15, ])
#'
#' as_sfnetwork(pts)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#'
#' plot(st_geometry(pts))
#' plot(as_sfnetwork(pts))
#'
#' par(oldpar)
#'
#' @export
as_sfnetwork.sf = function(x, ...) {
  dots = list(...)
  if (! is.null(dots$length_as_weight)) deprecate_length_as_weight("as_sfnetwork.sf")
  if (has_single_geom_type(x, "LINESTRING")) {
    if (! is.null(dots$edges_as_lines)) deprecate_edges_as_lines()
    create_from_spatial_lines(x, ...)
  } else if (has_single_geom_type(x, "POINT")) {
    create_from_spatial_points(x, ...)
  } else {
    stop(
      "Geometries are not all of type LINESTRING, or all of type POINT",
      call. = FALSE
    )
  }
}

#' @describeIn as_sfnetwork Convert spatial geometries of class
#' \code{\link[sf]{sfc}} directly into a \code{\link{sfnetwork}}.
#' Supported geometry types are either \code{LINESTRING} or \code{POINT}. In
#' the first case, the lines become the edges in the network, and nodes are
#' placed at their boundaries. Additional arguments are forwarded to
#' \code{\link{create_from_spatial_lines}}. In the latter case, the points
#' become the nodes in the network, and are connected by edges according to a
#' specified method. Additional arguments are forwarded to
#' \code{\link{create_from_spatial_points}}.
#'
#' @importFrom sf st_as_sf
#' @export
as_sfnetwork.sfc = function(x, ...) {
  as_sfnetwork(st_as_sf(x), ...)
}

#' @describeIn as_sfnetwork Convert spatial linear networks of class
#' \code{\link[spatstat.linnet]{linnet}} directly into an
#' \code{\link{sfnetwork}}. This requires the
#' \code{\link[spatstat.geom:spatstat.geom-package]{spatstat.geom}} package
#' to be installed.
#'
#' @examples
#' # From a linnet object.
#' if (require(spatstat.geom, quietly = TRUE)) {
#'   as_sfnetwork(simplenet)
#' }
#'
#' @export
as_sfnetwork.linnet = function(x, ...) {
  check_spatstat("spatstat.geom")
  # The easiest approach is the same as for psp objects, i.e. converting the
  # linnet object into a psp format and then applying the corresponding method.
  x_psp = spatstat.geom::as.psp(x)
  as_sfnetwork(x_psp, ...)
}

#' @describeIn as_sfnetwork Convert spatial line segments of class
#' \code{\link[spatstat.geom]{psp}} directly into a \code{\link{sfnetwork}}.
#' The lines become the edges in the network, and nodes are placed at their
#' boundary points.
#'
#' @examples
#' # From a psp object.
#' if (require(spatstat.geom, quietly = TRUE)) {
#'   set.seed(42)
#'   test_psp = psp(runif(10), runif(10), runif(10), runif(10), window=owin())
#'   as_sfnetwork(test_psp)
#' }
#'
#' @importFrom sf st_as_sf st_collection_extract
#' @export
as_sfnetwork.psp = function(x, ...) {
  check_spatstat_sf()
  # The easiest method for transforming a Line Segment Pattern (psp) object
  # into sfnetwork format is to transform it into sf format and then apply
  # the usual methods.
  x_sf = st_as_sf(x)
  # x_sf is an sf object composed by 1 POLYGON (the window of the psp object)
  # and several LINESTRINGs (the line segments). I'm not sure if and how we can
  # use the window object so I will extract only the LINESTRINGs.
  x_linestring = st_collection_extract(x_sf, "LINESTRING")
  # Apply as_sfnetwork.sf.
  as_sfnetwork(x_linestring, ...)
}

#' @describeIn as_sfnetwork Convert spatial networks of class
#' \code{\link[stplanr:sfNetwork-class]{sfNetwork}} directly into a
#' \code{\link{sfnetwork}}. This will extract the edges as an
#' \code{\link[sf]{sf}} object and re-create the network structure. The
#' directness of the original network is preserved unless specified otherwise
#' through the \code{directed} argument.
#'
#' @importFrom igraph is_directed
#' @export
as_sfnetwork.sfNetwork = function(x, ...) {
  args = list(...)
  # Retrieve the @sl slot, which contains the linestring of the network.
  args$x = x@sl
  # Define the directed argument automatically if not given, using the @g slot.
  dir_missing = is.null(args$directed)
  args$directed = if (dir_missing) is_directed(x@g) else args$directed
  # Call as_sfnetwork.sf to build the sfnetwork.
  do.call("as_sfnetwork.sf", args)
}

#' @describeIn as_sfnetwork Convert graph objects of class
#' \code{\link[tidygraph]{tbl_graph}} directly into a \code{\link{sfnetwork}}.
#' This will work if at least the nodes can be converted to an
#' \code{\link[sf]{sf}} object through \code{\link[sf]{st_as_sf}}. The
#' directness of the original graph is preserved unless specified otherwise
#' through the \code{directed} argument.
#'
#' @examples
#' # From a tbl_graph with coordinate columns.
#' library(tidygraph, quietly = TRUE)
#'
#' nodes = data.frame(lat = c(7, 7, 8), lon = c(51, 52, 52))
#' edges = data.frame(from = c(1, 1, 3), to = c(2, 3, 2))
#' tbl_net = tbl_graph(nodes, edges)
#' as_sfnetwork(tbl_net, coords = c("lon", "lat"), crs = 4326)
#'
#' @importFrom igraph is_directed
#' @export
as_sfnetwork.tbl_graph = function(x, ...) {
  # Get nodes and edges from the graph and add to the other given arguments.
  args = c(as.list(x), list(...))
  # If no directedness is specified, use the directedness from the tbl_graph.
  dir_missing = is.null(args$directed)
  args$directed = if (dir_missing) is_directed(x) else args$directed
  # Call the sfnetwork construction function.
  do.call("sfnetwork", args)
}

#' Create a spatial network from linestring geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param compute_length Should the geographic length of the edges be stored in
#' a column named \code{length}? Uses \code{\link[sf]{st_length}} to compute
#' the length of the edge geometries. If there is already a column named
#' \code{length}, it will be overwritten. Please note that the values in this
#' column are \strong{not} automatically recognized as edge weights. This needs
#' to be specified explicitly when calling a function that uses edge weights.
#' Defaults to \code{FALSE}.
#'
#' @details It is assumed that the given linestring geometries form the edges
#' in the network. Nodes are created at the line boundaries. Shared boundaries
#' between multiple linestrings become the same node.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' as_sfnetwork(roxel)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#'
#' plot(st_geometry(roxel))
#' plot(as_sfnetwork(roxel))
#'
#' par(oldpar)
#'
#' @importFrom sf st_as_sf st_sf
#' @export
create_from_spatial_lines = function(x, directed = TRUE,
                                     compute_length = FALSE) {
  # The provided lines will form the edges of the network.
  edges = st_as_sf(x)
  # Get the boundary points of the edges.
  nodes = linestring_boundary_points(edges)
  # Give each unique location a unique ID.
  indices = st_match(nodes)
  # Define for each endpoint if it is a source or target node.
  is_source = rep(c(TRUE, FALSE), length(nodes) / 2)
  # Define for each edge which node is its source and target node.
  if ("from" %in% colnames(edges)) raise_overwrite("from")
  edges$from = indices[is_source]
  if ("to" %in% colnames(edges)) raise_overwrite("to")
  edges$to = indices[!is_source]
  # Remove duplicated nodes from the nodes table.
  nodes = nodes[!duplicated(indices)]
  # Convert to sf object
  nodes = st_sf(geometry = nodes)
  # Use the same sf column name in the nodes as in the edges.
  geom_colname = attr(edges, "sf_column")
  if (geom_colname != "geometry") {
    names(nodes)[1] = geom_colname
    attr(nodes, "sf_column") = geom_colname
  }
  # Use the same class for the nodes as for the edges.
  # This mainly affects the "lower level" classes.
  # For example an sf tibble instead of a sf data frame.
  class(nodes) = class(edges)
  # Create a network out of the created nodes and the provided edges.
  # Force to skip network validity tests because we already know they pass.
  sfnetwork(nodes, edges,
    directed = directed,
    edges_as_lines = TRUE,
    compute_length = compute_length,
    force = TRUE
  )
}

#' Create a spatial network from point geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{POINT} geometries.
#'
#' @param connections How to connect the given point geometries to each other?
#' Can be specified either as an adjacency matrix, or as a character
#' describing a specific method to define the connections.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param edges_as_lines Should the created edges be spatially explicit, i.e.
#' have \code{LINESTRING} geometries stored in a geometry list column? Defaults
#' to \code{TRUE}.
#'
#' @param compute_length Should the geographic length of the edges be stored in
#' a column named \code{length}? Uses \code{\link[sf]{st_length}} to compute
#' the length of the edge geometries when edges are spatially explicit, and
#' \code{\link[sf]{st_distance}} to compute the distance between boundary nodes
#' when edges are spatially implicit. Please note that the values in this
#' column are \strong{not} automatically recognized as edge weights. This needs
#' to be specified explicitly when calling a function that uses edge weights.
#' Defaults to \code{FALSE}.
#'
#' @param k The amount of neighbors to connect to if
#' \code{connections = 'knn'}. Defaults to \code{1}, meaning that nodes are
#' only connected to their nearest neighbor. Ignored for any other value of the
#' \code{connected} argument.
#'
#' @details It is assumed that the given points form the nodes in the network.
#' How those nodes are connected by edges depends on the \code{connections}
#' argument.
#'
#' The connections can be specified through an adjacency matrix A, which is an
#' n x n matrix with n being the number of nodes, and element Aij holding a
#' \code{TRUE} value if there is an edge from node i to node j, and a
#' \code{FALSE} value otherwise. In the case of undirected networks, the matrix
#' is not tested for symmetry, and an edge will exist between node i and node j
#' if either element Aij or element Aji is \code{TRUE}. Non-logical matrices
#' are first converted into logical matrices using \code{\link{as.logical}}.
#'
#' The provided adjacency matrix may also be a list-formatted sparse matrix.
#' This is a list with one element per node, holding the integer indices of the
#' nodes it is adjacent to. An example are \code{\link[sf]{sgbp}} objects.
#'
#' Alternatively, the connections can be specified by providing the name of a
#' specific method that will create the adjacency matrix internally. Valid
#' options are:
#'
#' \itemize{
#'   \item \code{complete}: All nodes are directly connected to each other.
#'   \item \code{sequence}: The nodes are sequentially connected to each other,
#'   meaning that the first node is connected to the second node, the second
#'   node is connected to the third node, et cetera.
#'   \item \code{mst}: The nodes are connected by their spatial
#'   \href{https://en.wikipedia.org/wiki/Minimum_spanning_tree}{minimum
#'   spanning tree}, i.e. the set of edges with the minimum total edge length
#'   required to connect all nodes. The tree is always constructed on an
#'   undirected network, regardless of the value of the \code{directed}.
#'   argument. If \code{directed = TRUE}, each edge is duplicated and reversed
#'   to ensure full connectivity of the network. Can also be specified as
#'   \code{minimum_spanning_tree}.
#'   \item \code{delaunay}: The nodes are connected by their
#'   \href{https://en.wikipedia.org/wiki/Delaunay_triangulation}{Delaunay
#'   triangulation}.
#'   Requires the \href{https://r-spatial.github.io/spdep/index.html}{spdep}
#'   package to be installed, and assumes planar coordinates.
#'   \item \code{gabriel}: The nodes are connected as a
#'   \href{https://en.wikipedia.org/wiki/Gabriel_graph}{Gabriel graph}.
#'   Requires the \href{https://r-spatial.github.io/spdep/index.html}{spdep}
#'   package to be installed, and assumes planar coordinates.
#'   \item \code{rn}: The nodes are connected as a
#'   \href{https://en.wikipedia.org/wiki/Relative_neighborhood_graph}{relative
#'   neighborhood graph}. Can also be specified as \code{relative_neighborhood}
#'   or \code{relative_neighbourhood}.
#'   Requires the \href{https://r-spatial.github.io/spdep/index.html}{spdep}
#'   package to be installed, and assumes planar coordinates.
#'   \item \code{knn}: Each node is connected to its k nearest neighbors, with
#'   \code{k} being specified through the \code{k} argument. By default,
#'   \code{k = 1}, meaning that the nodes are connected as a
#'   \href{https://en.wikipedia.org/wiki/Nearest_neighbor_graph}{nearest
#'   neighbor graph}. Can also be specified as \code{nearest_neighbors} or
#'   \code{nearest_neighbours}.
#'   Requires the \href{https://r-spatial.github.io/spdep/index.html}{spdep}
#'   package to be installed.
#' }
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#'
#' pts = roxel[seq(1, 100, by = 10),] |>
#'   st_geometry() |>
#'   st_centroid() |>
#'   st_transform(3035)
#'
#' # Using an adjacency matrix
#' adj = matrix(c(rep(TRUE, 10), rep(FALSE, 90)), nrow = 10)
#' net = as_sfnetwork(pts, connections = adj)
#'
#' plot(net)
#'
#' # Using a sparse adjacency matrix from a spatial predicate
#' dst = units::set_units(500, "m")
#' adj = st_is_within_distance(pts, dist = dst)
#' net = as_sfnetwork(pts, connections = adj)
#'
#' plot(net)
#'
#' # Using pre-defined methods
#' cnet = as_sfnetwork(pts, connections = "complete")
#' snet = as_sfnetwork(pts, connections = "sequence")
#' mnet = as_sfnetwork(pts, connections = "mst")
#' dnet = as_sfnetwork(pts, connections = "delaunay")
#' gnet = as_sfnetwork(pts, connections = "gabriel")
#' rnet = as_sfnetwork(pts, connections = "rn")
#' nnet = as_sfnetwork(pts, connections = "knn")
#' knet = as_sfnetwork(pts, connections = "knn", k = 2)
#'
#' par(mar = c(1,1,1,1), mfrow = c(4,2))
#'
#' plot(cnet, main = "complete")
#' plot(snet, main = "sequence")
#' plot(mnet, main = "minimum spanning tree")
#' plot(dnet, main = "delaunay triangulation")
#' plot(gnet, main = "gabriel graph")
#' plot(rnet, main = "relative neighborhood graph")
#' plot(nnet, main = "nearest neighbor graph")
#' plot(knet, main = "k nearest neighbor graph (k = 2)")
#'
#' par(oldpar)
#'
#' @export
create_from_spatial_points = function(x, connections = "complete",
                                      directed = TRUE, edges_as_lines = TRUE,
                                      compute_length = FALSE, k = 1) {
  if (is_single_string(connections)) {
    nblist = switch(
      connections,
      complete = complete_neighbors(x),
      sequence = sequential_neighbors(x),
      mst = mst_neighbors(x),
      delaunay = delaunay_neighbors(x),
      gabriel = gabriel_neighbors(x),
      rn = relative_neighbors(x),
      knn = nearest_neighbors(x, k),
      minimum_spanning_tree = mst_neighbors(x),
      relative_neighborhood = relative_neighbors(x),
      relative_neighbourhood = relative_neighbors(x),
      nearest_neighbors = nearest_neighbors(x, k),
      nearest_neighbours = nearest_neighbors(x, k),
      raise_unknown_input(connections)
    )
  } else {
    nblist = custom_neighbors(x, connections)
  }
  nb2net(nblist, x, directed, edges_as_lines, compute_length)
}

custom_neighbors = function(x, connections) {
  if (is.matrix(connections)) {
    require_valid_adjacency_matrix(connections, x)
    adj2nb(connections)
  } else if (inherits(connections, c("sgbp", "nb", "list"))) {
    require_valid_neighbor_list(connections, x)
    connections
  } else {
    stop(
      "Connections should be specified as a matrix, a list-formatted sparse",
      " matrix, or a single character.",
      call. = FALSE
    )
  }
}

#' @importFrom sf st_geometry
complete_neighbors = function(x) {
  n_nodes = length(st_geometry(x))
  # Create the adjacency matrix, with everything connected to everything.
  connections = matrix(TRUE, ncol = n_nodes, nrow = n_nodes)
  diag(connections) = FALSE # No loop edges.
  # Return as neighbor list.
  adj2nb(connections)
}

#' @importFrom sf st_geometry
sequential_neighbors = function(x) {
  # Each node in x is connected to the next node in x.
  n_nodes = length(st_geometry(x))
  lapply(c(1:(n_nodes - 1)), \(x) x + 1)
}

#' @importFrom igraph as_edgelist graph_from_adjacency_matrix igraph_opt
#' igraph_options mst as_adj_list
#' @importFrom sf st_distance st_geometry
mst_neighbors = function(x, directed = TRUE, edges_as_lines = TRUE) {
  # Change default igraph options.
  # This prevents igraph returns node or edge indices as formatted sequences.
  # We only need the "raw" integer indices.
  # Changing this option improves performance especially on large networks.
  default_igraph_opt = igraph_opt("return.vs.es")
  igraph_options(return.vs.es = FALSE)
  on.exit(igraph_options(return.vs.es = default_igraph_opt))
  # Create a complete graph.
  n_nodes = length(st_geometry(x))
  connections = upper.tri(matrix(FALSE, ncol = n_nodes, nrow = n_nodes))
  net = graph_from_adjacency_matrix(connections, mode = "undirected")
  # Compute distances between adjacent nodes for each edge in that graph.
  dists = st_distance(x)[as_edgelist(net, names = FALSE)]
  # Compute minimum spanning tree of the weighted complete graph.
  mst = mst(net, weights = dists)
  # Return as a neighbor list.
  as_adj_list(mst)
}

#' @importFrom sf st_geometry
delaunay_neighbors = function(x) {
  requireNamespace("spdep") # Package spdep is required for this function.
  tri2nb(st_geometry(x))
}

#' @importFrom sf st_geometry
gabriel_neighbors = function(x) {
  requireNamespace("spdep") # Package spdep is required for this function.
  spdep::graph2nb(spdep::gabrielneigh(st_geometry(x)), sym = TRUE)
}

#' @importFrom sf st_geometry
relative_neighbors = function(x) {
  requireNamespace("spdep") # Package spdep is required for this function.
  spdep::graph2nb(spdep::relativeneigh(st_geometry(x)), sym = TRUE)
}

#' @importFrom sf st_geometry
nearest_neighbors = function(x, k = 1) {
  requireNamespace("spdep") # Package spdep is required for this function.
  spdep::knn2nb(spdep::knearneigh(st_geometry(x), k = k), sym = FALSE)
}

#' Create a spatial network with sampled nodes
#'
#' @param n The number of nodes to be sampled.
#'
#' @param radius The radius within which nodes will be connected by an edge.
#' See Details.
#'
#' @param bounds The spatial features within which the nodes should be sampled
#' as object of class \code{\link[sf]{sf}}, \code{\link[sf]{sfc}},
#' \code{\link[sf:st]{sfg}} or \code{\link[sf:st_bbox]{bbox}}. If set to
#' \code{NULL}, nodes will be sampled within a unit square.
#'
#' @param edges_as_lines Should the created edges be spatially explicit, i.e.
#' have \code{LINESTRING} geometries stored in a geometry list column? Defaults
#' to \code{TRUE}.
#'
#' @param compute_length Should the geographic length of the edges be stored in
#' a column named \code{length}? Uses \code{\link[sf]{st_length}} to compute
#' the length of the edge geometries when edges are spatially explicit, and
#' \code{\link[sf]{st_distance}} to compute the distance between boundary nodes
#' when edges are spatially implicit. Please note that the values in this
#' column are \strong{not} automatically recognized as edge weights. This needs
#' to be specified explicitly when calling a function that uses edge weights.
#' Defaults to \code{FALSE}.
#'
#' @param ... Additional arguments passed on to \code{\link[sf]{st_sample}}.
#' Ignored if \code{bounds = NULL}.
#'
#' @details Two nodes will be connected by an edge if the distance between them
#' is within the given radius. If nodes are sampled on a unit square (i.e. when
#' \code{bounds = NULL}) this radius is unitless. If bounds are given as a
#' spatial feature, the radius is assumed to be in meters for geographic
#' coordinates, and in the units of the coordinate reference system for
#' projected coordinates. Alternatively, units can also be specified explicitly
#' by providing a \code{\link[units]{units}} object.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#'
#' # Sample 10 nodes on a unit square
#' # Connect nodes by an edge if they are within 0.25 distance from each other
#' net = play_spatial(10, 0.25)
#' net
#'
#' plot(net)
#'
#' # Sample 10 nodes within a spatial bounding box
#' # Connect nodes by an edge if they are within 1 km from each other
#' net = play_spatial(10, units::set_units(1, "km"), bounds = st_bbox(roxel))
#' net
#'
#' plot(net)
#'
#' par(oldpar)
#'
#' @importFrom sf st_is_within_distance st_sample
#' @importFrom tidygraph play_geometry
#' @export
play_spatial = function(n, radius, bounds = NULL, edges_as_lines = TRUE,
                        compute_length = FALSE, ...) {
  if (is.null(bounds)) {
    # Use play_geometry to create and link n nodes inside a unit square.
    x_tbg = play_geometry(n, radius)
    # Convert to sfnetwork.
    x_sfn = as_sfnetwork(
      x_tbg,
      directed = FALSE,
      edges_as_lines = edges_as_lines,
      compute_length = compute_length,
      force = TRUE,
      coords = c("x", "y")
    )
  } else {
    # Sample n points within the given spatial feature.
    pts = st_sample(bounds, n, ...)
    # Define the connections between the points based on distance.
    conns = st_is_within_distance(pts, dist = radius)
    # Remove loop edges.
    # Currently setting remove_self = TRUE in the predicate does not work ...
    # ... if coordinates are geographic and s2 is used.
    conns = mapply(setdiff, conns, seq_along(conns), SIMPLIFY = FALSE)
    # Create the sfnetwork.
    x_sfn = create_from_spatial_points(
      pts,
      connections = conns,
      directed = FALSE,
      edges_as_lines = edges_as_lines,
      compute_length = compute_length
    )
  }
  x_sfn
}
