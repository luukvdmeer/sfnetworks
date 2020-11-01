#' @importFrom sf st_nearest_feature
#' @importFrom tidygraph pull
set_shortest_paths_parameters = function(x, from, to, weights) {
  # Check if single to point has empty geometry.
  if (is.sf(to) | is.sfc(to) | is.sf(from) | is.sfc(from)) {
    if (length(from) == length(which(is_empty(from)))) {
      stop("From points contain only empty geometries", call. = F)
    }
    if (length(to) == length(which(is_empty(to)))) {
      stop("To points contain only empty geometries", call. = F)
    }
  }
  # Get node index of from node.
  if (length(from) > 1) {
    warning(
      "Multiple from points are given. Only the first (non-empty) one will be used",
      call. = FALSE
    )
  }
  if (is.sf(from) | is.sfc(from)) {
    from = from[!is_empty(from)]
    from = sf::st_nearest_feature(from, activate(x, "nodes"))
  }
  # Get node indices of to nodes.
  if (is.sf(to) | is.sfc(to)) {
    if (any(is_empty(to))) {
      warning(
        "To points containing empty geometries have been removed",
        call. = FALSE
      )
    }
    to = to[!is_empty(to)]
    to = sf::st_nearest_feature(to, activate(x, "nodes"))
    # Warning for empty geometry removal.
  }
  # If weights is a string, retrieve the column which name matches the string.
  if (is.character(weights)) {
    weights = tidygraph::pull(activate(x, "edges"), weights)
  }
  list(graph = x, from = from, to = to, weights = weights)
}

#' Shortest paths between points in geographical space
#'
#' Wrappers around the shortest path calculation functionalities in
#' \code{\link[igraph:shortest_paths]{igraph}}, allowing to
#' provide any geospatial point as `from` argument and any set of geospatial
#' points as `to` argument. If such a geospatial point is not equal to a node
#' in the network, it will be snapped to its nearest node before calculating
#' the shortest paths.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
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
#' calculated. Can be an object of  class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#' Alternatively, it can be a numeric vector, containing the indices of the nodes
#' to which the shortest paths will be calculated. By default, all nodes in the
#' network are included.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Can be a numeric vector giving edge weights, or a column name referring to
#' an attribute column in the edges table containing those weights. If set to
#' \code{NULL}, the values of a column named 'weight' in the edges table will
#' be used automatically, as long as this column is present. If set to
#' \code{NA}, no weights are used (even if the edges have a weight column).
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
#' st_shortest_paths(net, 1, 9)
#'
#' # 2. providing nodes as spatial points
#'
#' p1 = st_geometry(net, "nodes")[1]
#' p2 = st_geometry(net, "nodes")[9]
#' st_shortest_paths(net, p1, p2)
#'
#' # 3. providing spatial points outside of the network
#'
#' p3 = st_sfc(p1[[1]] + st_point(c(500, 500)), crs = st_crs(p1))
#' p4 = st_sfc(p2[[1]] + st_point(c(-500, -500)), crs = st_crs(p2))
#' st_shortest_paths(net, p3, p4)
#'
#' # 4. Providing weights from column name
#'
#' net = net %>%
#'   activate("edges") %>%
#'   mutate(length = sf::st_length(.))
#' st_shortest_paths(net, p1, p2, weights = "length")
#'
#' # 5. Providing weights from column named 'weight'
#'
#' net = net %>%
#'   activate("edges") %>%
#'   mutate(weight = length)
#' st_shortest_paths(net, p1, p2)
#'
#' @importFrom igraph shortest_paths V
#' @export
st_shortest_paths = function(x, from, to = V(x), weights = NULL, ...) {
  params = set_shortest_paths_parameters(x, from, to, weights)
  igraph_call = do.call(igraph::shortest_paths, c(params, ...))
  names(igraph_call) = c('node_path', 'edge_path', 'predecessors', 'inbound_edges')
  tibble::as_tibble(
    do.call(
      # Call `cbind` on resulting list of lists to create a matrix
      # with `vpath` and `epath` as columns and each path as rows
      cbind, lapply(
        # Select only relevant output: node and edge paths
        igraph_call[c('node_path', 'edge_path')],
        # A nested `lapply` is used to access the second list level
        # and change from `igraph` class to integer
        function(x) lapply(x, as.integer)
      )
    )
  )
}

#' @describeIn spatial_shortest_paths Wrapper around
#' \code{igraph::all_shortest_paths}.
#'
#' @examples
#'
#' ## Calculate all shortest paths between two points
#'
#' st_all_shortest_paths(net, 5, 1)
#'
#' @importFrom igraph all_shortest_paths V
#' @export
st_all_shortest_paths = function(x, from, to = V(x), weights = NULL) {
  params = set_shortest_paths_parameters(x, from, to, weights)
  igraph_call = do.call(igraph::all_shortest_paths, params)
  names(igraph_call) = c('node_path', 'nrgeo')
  tibble::as_tibble(
    do.call(
      # Call `cbind` on resulting list of lists to create a matrix
      # with `res` as column and each path as rows
      cbind, lapply(
        # Select only relevant output: node paths
        igraph_call['res'],
        # A nested `lapply` is used to access the second list level
        # and change from `igraph` class to integer
        function(x) lapply(x, as.integer)
      )
    )
  )
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
st_network_distance = function(x, from = V(x), to = V(x), weights = NULL, ...) {
  params = set_shortest_paths_parameters(x, from, to, weights)
  names(params)[which(names(params) == "from")] = "v"
  do.call(igraph::distances, c(params, ...))
}
