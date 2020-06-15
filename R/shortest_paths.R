#' @importFrom sf st_nearest_feature
#' @importFrom tidygraph pull
set_shortest_paths_parameters = function(graph, from, to, weights, snap) {
  # Get node index of from node.
  if (is.sf(from) | is.sfc(from) | is.sfg(from)) {
    from = switch(
      snap,
      nearest_node = sf::st_nearest_feature(from, activate(graph, "nodes")),
      stop(snap, " is not a supported snapping technique")
    )
  }
  # Get node indices of to nodes.
  if (is.sf(to) | is.sfc(to) | is.sfg(to)) {
    to = switch(
      snap,
      nearest_node = sf::st_nearest_feature(to, activate(graph, "nodes")),
      stop(snap, " is not a supported snapping technique")
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
#' @importFrom igraph shortest_paths V
#' @export
st_shortest_paths = function(graph, from, to = V(graph), weights = NULL, 
                             snap = "nearest_node", ...) {
  params = set_shortest_paths_parameters(graph, from, to, weights, snap)
  if (length(params$from) > 1) {
    warning("Multiple from points are given. Only the first one will be used")
  }
  do.call(igraph::shortest_paths, c(params, ...))
} 

#' @describeIn spatial_shortest_paths Wrapper around 
#' \code{igraph::all_shortest_paths}.
#' @importFrom igraph all_shortest_paths V
#' @export
st_all_shortest_paths = function(graph, from, to = V(graph), weights = NULL, 
                                 snap = "nearest_node") {
  params = set_shortest_paths_parameters(graph, from, to, weights, snap)
  if (length(params$from) > 1) {
    warning("Multiple from points are given. Only the first one will be used")
  }
  do.call(igraph::all_shortest_paths, params)
}

#' @describeIn spatial_shortest_paths Wrapper around \code{igraph::distances}.
#' @importFrom igraph distances V
#' @export
st_network_distance = function(graph, from = V(graph), to = V(graph), 
                                weights = NULL, snap = "nearest_node", ...) {
  params = set_shortest_paths_parameters(graph, from, to, weights, snap)
  names(params)[which(names(params) == "from")] = "v"
  do.call(igraph::distances, c(params, ...))
}