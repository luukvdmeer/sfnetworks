#' @importFrom sf st_nearest_feature
#' @importFrom tidygraph pull
set_shortest_paths_parameters = function(graph, from, to, weights, snap) {
  # Get node index of from node.
  if (is.sf(from) | is.sfc(from) | is.sfg(from)) {
    from = switch(
      snap,
      nearest_node = sf::st_nearest_feature(from, activate(graph, "nodes")),
      stop("Unknown snapping technique: ", snap, call. = FALSE)
    )
  }
  # Get node indices of to nodes.
  if (is.sf(to) | is.sfc(to) | is.sfg(to)) {
    to = switch(
      snap,
      nearest_node = sf::st_nearest_feature(to, activate(graph, "nodes")),
      stop("Unknown snapping technique: ", snap, call. = FALSE)
    )
  }
  # If weights is a string, retrieve the column which name matches the string.
  if (is.character(weights)) {
    weights = tidygraph::pull(activate(graph, "edges"), weights)
  }
  list(graph = graph, from = from, to = to, weights = weights)
}

#' Shortest paths between points in geographical space
#'
#' Wrappers around the shortest path calculation functionalities in
#' \code{\link[igraph:shortest_paths]{igraph}}, allowing to
#' provide any geospatial point as `from` argument and any set of geospatial
#' points as `to` argument.
#'
#' @param graph An object of class \code{\link{sfnetwork}}.
#'
#' @param from The geospatial point from which the shortest paths will be
#' calculated. Can be an object an object of class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}, containing a single feature. When multiple features
#' are given, only the first one is taken.
#' Alternatively, it can be a numeric constant, referring to the index of the
#' node from which the shortest paths will be calculated. Only in the case of
#' \code{st_network_distances} the restriction of a single feature does not
#' apply. Then, it can also be an \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' object with multiple features, or alternatively a vector of node indices.
#'
#' @param to The (set of) geospatial point(s) to which the shortest paths will be
#' calculated. Can be an object of class \code{\link[sf:st]{sfg}}, or an object
#' of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}. Alternatively, it can
#' be a numeric vector, containing the indices of the nodes to which the
#' shortest paths will be calculated. By default, all nodes in the network are
#' included.
#'
#' @param weights Possibly a numeric vector giving edge weights, or a column
#' name referring to an attribute column in the edges table that contains those
#' weights. If set to \code{NULL} and the network has a column named 'weight' in
#' the edges table, then those values are used. If set to \code{NA} then no
#' weights are used (even if the edges have a weight attribute).
#'
#' @param snap Which technique to use for snapping given geospatial points to the
#' network. Can be either 'nearest_node', which will use the nearest node to the
#' given point, or 'nearest_point_on_edge', which will use the nearest location
#' on the nearest edge to the given point. Defaults to 'nearest_node', which is
#' currently the only implemented option.
#'
#' @param ... Arguments passed on to the corresponding
#' \code{\link[igraph:shortest_paths]{igraph}} function.
#'
#' @details See the \code{\link[igraph:shortest_paths]{igraph}} documentation.
#'
#' @return The return value is the same as for the corresponding \code{igraph}
#' function, see \code{\link[igraph:shortest_paths]{here}}.
#'
#' @name spatial_shortest_paths
NULL

#' @describeIn spatial_shortest_paths Wrapper around
#' \code{igraph::shortest_paths}.
#'
#' @examples
#' library(sf)
#' library(tidygraph)
#'
#' net = st_transform(as_sfnetwork(roxel, directed = FALSE), 3035)
#'
#' # 1. providing node indices
#'
#' st_shortest_paths(net, 1, 9)$vpath
#'
#' # 2. providing nodes as spatial points
#'
#' p1 = st_geometry(net, "nodes")[1]
#' p2 = st_geometry(net, "nodes")[9]
#' st_shortest_paths(net, p1, p2)$vpath
#'
#' # 3. providing spatial points outside of the network
#'
#' p3 = st_sfc(p1[[1]] + st_point(c(500, 500)), crs = st_crs(p1))
#' p4 = st_sfc(p2[[1]] + st_point(c(-500, -500)), crs = st_crs(p2))
#' st_shortest_paths(net, p3, p4)$vpath
#'
#' # 4. Providing weigths from column name
#'
#' net = net %>%
#'   activate("edges") %>%
#'   mutate(length = sf::st_length(.))
#' st_shortest_paths(net, p1, p2, weights = "length")$vpath
#'
#' # 5. Providing weigths from column named 'weight'
#'
#' net = net %>%
#'   activate("edges") %>%
#'   mutate(weight = length)
#' st_shortest_paths(net, p1, p2)$vpath
#'
#' @importFrom igraph shortest_paths V
#' @export
st_shortest_paths = function(graph, from, to = V(graph), weights = NULL,
                             snap = "nearest_node", ...) {
  params = set_shortest_paths_parameters(graph, from, to, weights, snap)
  if (length(params$from) > 1) {
    warning(
      "Multiple from points are given. Only the first one will be used",
      call. = FALSE
    )
  }
  do.call(igraph::shortest_paths, c(params, ...))
}

#' @describeIn spatial_shortest_paths Wrapper around
#' \code{igraph::all_shortest_paths}.
#' 
#' @examples
#'
#' ## Calculate all shortest paths between two points
#'
#' st_all_shortest_paths(net, 5, 1)$res
#'
#' @importFrom igraph all_shortest_paths V
#' @export
st_all_shortest_paths = function(graph, from, to = V(graph), weights = NULL,
                                 snap = "nearest_node") {
  params = set_shortest_paths_parameters(graph, from, to, weights, snap)
  if (length(params$from) > 1) {
    warning(
      "Multiple from points are given. Only the first one will be used",
      call. = FALSE
    )
  }
  do.call(igraph::all_shortest_paths, params)
}

#' @describeIn spatial_shortest_paths Wrapper around \code{igraph::distances}.
#'
#' @examples
#'
#' ## Calculate a distance matrix
#'
#' ps1 = c(st_geometry(p1), st_sfc(p3))
#' ps2 = c(st_geometry(p2), st_sfc(p4))
#'
#' st_network_distance(net, ps1, ps2)
#'
#' @importFrom igraph distances V
#' @export
st_network_distance = function(graph, from = V(graph), to = V(graph),
                                weights = NULL, snap = "nearest_node", ...) {
  params = set_shortest_paths_parameters(graph, from, to, weights, snap)
  names(params)[which(names(params) == "from")] = "v"
  do.call(igraph::distances, c(params, ...))
}
