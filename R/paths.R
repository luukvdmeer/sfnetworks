#' Paths between points in geographical space
#'
#' Combined wrapper around \code{\link[igraph]{shortest_paths}},
#' \code{\link[igraph]{all_shortest_paths}} and
#' \code{\link[igraph]{all_simple_paths}} from \code{\link[igraph]{igraph}},
#' allowing to provide any geospatial point as \code{from} argument and any
#' set of geospatial points as \code{to} argument. If such a geospatial point
#' is not equal to a node in the network, it will be snapped to its nearest
#' node before calculating the shortest or simple paths.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param from The geospatial point from which the paths will be
#' calculated. Can be an object an object of class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}, containing a single feature. When multiple features
#' are given, only the first one is taken.
#' Alternatively, it can be an integer, referring to the index of the
#' node from which the paths will be calculated, or a character,
#' referring to the name of the node from which the paths will be
#' calculated.
#'
#' @param to The (set of) geospatial point(s) to which the paths will be
#' calculated. Can be an object of  class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#' Alternatively, it can be a numeric vector, containing the indices of the nodes
#' to which the paths will be calculated, or a character vector,
#' containing the names of the nodes to which the paths will be
#' calculated. By default, all nodes in the network are included.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Can be a numeric vector giving edge weights, or a column name referring to
#' an attribute column in the edges table containing those weights. If set to
#' \code{NULL}, the values of a column named \code{weight} in the edges table
#' will be used automatically, as long as this column is present. If not, the
#' geographic edge lengths will be calculated internally and used as weights.
#' If set to \code{NA}, no weights are used, even if the edges have a
#' \code{weight} column. Ignored when \code{type = 'all_simple'}.
#'
#' @param type Character defining which type of path calculation should be
#' performed. If set to \code{'shortest'} paths are calculated using
#' \code{\link[igraph]{shortest_paths}}, if set to
#' \code{'all_shortest'} paths are calculated using
#' \code{\link[igraph]{all_shortest_paths}}, if set to
#' \code{'all_simple'} paths are calculated using
#' \code{\link[igraph]{all_simple_paths}}. Defaults to \code{'shortest'}.
#'
#' @param ... Arguments passed on to the corresponding
#' \code{\link[igraph:shortest_paths]{igraph}} or
#' \code{\link[igraph:all_simple_paths]{igraph}} function. Arguments
#' \code{predecessors} and \code{inbound.edges} are ignored.
#'
#' @details See the \code{\link[igraph:shortest_paths]{igraph}} or
#' \code{\link[igraph:all_simple_paths]{igraph}} documentation.
#'
#' @seealso \code{\link{st_network_cost}}
#'
#' @return An object of class \code{\link[tibble]{tbl_df}} with one row per
#' returned path. Depending on the setting of the \code{type} argument,
#' columns can be \code{node_paths} (a list column with for each path the
#' ordered indices of nodes present in that path) and \code{edge_paths}
#' (a list column with for each path the ordered indices of edges present in
#' that path). \code{'all_shortest'} and \code{'all_simple'} return only
#' \code{node_paths}, while \code{'shortest'} returns both.
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' # Create a network with edge lengths as weights.
#' # These weights will be used automatically in shortest paths calculation.
#' net = as_sfnetwork(roxel, directed = FALSE) %>%
#'   st_transform(3035) %>%
#'   activate("edges") %>%
#'   mutate(weight = edge_length())
#'
#' # Providing node indices.
#' paths = st_network_paths(net, from = 495, to = 121)
#' paths
#'
#' node_path = paths %>%
#'   slice(1) %>%
#'   pull(node_paths) %>%
#'   unlist()
#' node_path
#'
#' par(mar = c(1,1,1,1))
#' plot(net, col = "grey")
#' plot(slice(activate(net, "nodes"), node_path), col = "red", add = TRUE)
#'
#' # Providing nodes as spatial points.
#' # Points that don't equal a node will be snapped to their nearest node.
#' p1 = st_geometry(net, "nodes")[495] + st_sfc(st_point(c(50, -50)))
#' st_crs(p1) = st_crs(net)
#' p2 = st_geometry(net, "nodes")[121] + st_sfc(st_point(c(-10, 100)))
#' st_crs(p2) = st_crs(net)
#'
#' paths = st_network_paths(net, from = p1, to = p2)
#' paths
#'
#' node_path = paths %>%
#'   slice(1) %>%
#'   pull(node_paths) %>%
#'   unlist()
#' node_path
#'
#' par(mar = c(1,1,1,1))
#' plot(net, col = "grey")
#' plot(c(p1, p2), col = "black", pch = 8, add = TRUE)
#' plot(slice(activate(net, "nodes"), node_path), col = "red", add = TRUE)
#'
#' # Using another column for weights.
#' net %>%
#'   activate("edges") %>%
#'   mutate(foo = runif(n(), min = 0, max = 1)) %>%
#'   st_network_paths(p1, p2, weights = "foo")
#'
#' # Obtaining all simple paths between two nodes.
#' # Beware, this function can take long when:
#' # --> Providing a lot of 'to' nodes.
#' # --> The network is large and dense.
#' st_network_paths(net, from = 1, to = 12, type = "all_simple")
#'
#' # Obtaining all shortest paths between two nodes.
#' # Not using edge weights.
#' # Hence, a shortest path is the paths with the least number of edges.
#' net = as_sfnetwork(roxel, directed = TRUE)
#' st_network_paths(net, from = 5, to = 1, weights = NA, type = "all_shortest")
#'
#' @importFrom igraph V
#' @export
st_network_paths = function(x, from, to = igraph::V(x), weights = NULL,
                            type = "shortest", ...) {
  UseMethod("st_network_paths")
}

#' @importFrom igraph V
#' @importFrom sf st_geometry
#' @export
st_network_paths.sfnetwork = function(x, from, to = igraph::V(x),
                                      weights = NULL, type = "shortest",
                                      ...) {
  # If 'from' points are given as simple feature geometries:
  # --> Convert them to node indices.
  if (is.sf(from) | is.sfc(from)) {
    from = set_path_endpoints(x, from)
  }
  # If 'to' points are given as simple feature geometries:
  # --> Convert them to node indices.
  if (is.sf(to) | is.sfc(to)) {
    to = set_path_endpoints(x, to)
  }
  # Igraph does not support multiple 'from' nodes.
  if (length(from) > 1) {
    warning(
      "Although argument 'from' has length > 1, ",
      "only the first element is used",
      call. = FALSE
    )
  }
  # Igraph does not support NA values in 'from' and 'to' nodes.
  if (any(is.na(c(from, to)))) {
    stop(
      "NA values present in argument 'from' and/or 'to'", 
      call. = FALSE
    )
  }
  # Call paths calculation function according to type argument.
  switch(
    type,
    shortest = get_shortest_paths(x, from, to, weights, ...),
    all_shortest = get_all_shortest_paths(x, from, to, weights, ...),
    all_simple = get_all_simple_paths(x, from, to, ...),
    raise_unknown_input(type)
  )
}

#' @importFrom igraph shortest_paths
#' @importFrom tibble as_tibble
get_shortest_paths = function(x, from, to, weights, ...) {
  # Set weights.
  weights = set_path_weights(x, weights)
  # Call igraph function.
  paths = shortest_paths(x, from, to, weights = weights, output = "both", ...)
  # Extract paths of node indices and edge indices.
  npaths = lapply(paths[[1]], as.integer)
  epaths = lapply(paths[[2]], as.integer)
  # Return as columns in a tibble.
  as_tibble(do.call(cbind, list(node_paths = npaths, edge_paths = epaths)))
}

#' @importFrom igraph all_shortest_paths
#' @importFrom tibble as_tibble
get_all_shortest_paths = function(x, from, to, weights, ...) {
  # Set weights.
  weights = set_path_weights(x, weights)
  # Call igraph function.
  paths = all_shortest_paths(x, from, to, weights = weights, ...)
  # Extract paths of node indices.
  npaths = lapply(paths[[1]], as.integer)
  # Return as column in a tibble.
  as_tibble(do.call(cbind, list(node_paths = npaths)))
}

#' @importFrom igraph all_simple_paths
#' @importFrom tibble as_tibble
get_all_simple_paths = function(x, from, to, ...) {
  # Call igraph function.
  paths = all_simple_paths(x, from, to, ...)
  # Extract paths of node indices.
  npaths = lapply(paths, as.integer)
  # Return as column in a tibble.
  as_tibble(do.call(cbind, list(node_paths = npaths)))
}

#' Compute a cost matrix of a spatial network
#'
#' Wrapper around \code{\link[igraph]{distances}} to calculate costs of
#' pairwise shortest paths between points in a spatial network. It allows to
#' provide any set of geospatial point as \code{from} and \code{to} arguments.
#' If such a geospatial point is not equal to a node in the network, it will
#' be snapped to its nearest node before calculating costs.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param from The (set of) geospatial point(s) from which the shortest paths
#' will be calculated. Can be an object of  class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#' Alternatively, it can be a numeric vector, containing the indices of the nodes
#' from which the shortest paths will be calculated, or a character vector,
#' containing the names of the nodes from which the shortest paths will be
#' calculated. By default, all nodes in the network are included.
#'
#' @param to The (set of) geospatial point(s) to which the shortest paths will
#' be calculated. Can be an object of  class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#' Alternatively, it can be a numeric vector, containing the indices of the nodes
#' to which the shortest paths will be calculated, or a character vector,
#' containing the names of the nodes to which the shortest paths will be
#' calculated. By default, all nodes in the network are included.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Can be a numeric vector giving edge weights, or a column name referring to
#' an attribute column in the edges table containing those weights. If set to
#' \code{NULL}, the values of a column named \code{weight} in the edges table
#' will be used automatically, as long as this column is present. If not, the
#' geographic edge lengths will be calculated internally and used as weights.
#' If set to \code{NA}, no weights are used, even if the edges have a
#' \code{weight} column.
#'
#' @param ... Arguments passed on to \code{\link[igraph]{distances}}.
#'
#' @details See the \code{\link[igraph:distances]{igraph}} documentation.
#'
#' @seealso \code{\link{st_network_paths}}
#'
#' @return An n times m numeric matrix where n is the length of the \code{from}
#' argument, and m is the length of the \code{to} argument.
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' # Create a network with edge lenghts as weights.
#' # These weights will be used automatically in shortest paths calculation.
#' net = as_sfnetwork(roxel, directed = FALSE) %>%
#'   st_transform(3035) %>%
#'   activate("edges") %>%
#'   mutate(weight = edge_length())
#'
#' # Providing node indices.
#' st_network_cost(net, from = c(495, 121), to = c(495, 121))
#'
#' # Providing nodes as spatial points.
#' # Points that don't equal a node will be snapped to their nearest node.
#' p1 = st_geometry(net, "nodes")[495] + st_sfc(st_point(c(50, -50)))
#' st_crs(p1) = st_crs(net)
#' p2 = st_geometry(net, "nodes")[121] + st_sfc(st_point(c(-10, 100)))
#' st_crs(p2) = st_crs(net)
#'
#' st_network_cost(net, from = c(p1, p2), to = c(p1, p2))
#'
#' # Using another column for weights.
#' net %>%
#'   activate("edges") %>%
#'   mutate(foo = runif(n(), min = 0, max = 1)) %>%
#'   st_network_cost(c(p1, p2), c(p1, p2), weights = "foo")
#'
#' # Not providing any from or to points includes all nodes by default.
#' with_graph(net, graph_order()) # Our network has 701 nodes.
#' cost_matrix = st_network_cost(net)
#' dim(cost_matrix)
#'
#' @importFrom igraph V
#' @export
st_network_cost = function(x, from = igraph::V(x), to = igraph::V(x),
                               weights = NULL, ...) {
  UseMethod("st_network_cost")
}

#' @importFrom igraph distances V
#' @export
st_network_cost.sfnetwork = function(x, from = igraph::V(x), to = igraph::V(x),
                                         weights = NULL, ...) {
  # If 'from' and/or 'to' points are given as simple feature geometries:
  # --> Convert them to node indices.
  if (is.sf(from) | is.sfc(from)) from = set_path_endpoints(x, from)
  if (is.sf(to) | is.sfc(to)) to = set_path_endpoints(x, to)
  # Igraph does not support NA values in 'from' and 'to' nodes.
  if (any(is.na(c(from, to)))) {
    stop(
      "NA values present in argument 'from' and/or 'to'", 
      call. = FALSE
    )
  }
  # Igraph does not support duplicated 'to' nodes.
  # This can happen without the user knowing when POINT geometries
  # are given to the 'to' argument that happen to snap to a same node.
  if (any(duplicated(to))) {
    warning(
      "Duplicated values in argument 'to' were removed.",
      call. = FALSE
    )
    to = unique(to)
  }
  # Set weights.
  weights = set_path_weights(x, weights)
  # Call igraph function.
  distances(x, from, to, weights = weights, ...)
}

#' @importFrom sf st_geometry st_nearest_feature
set_path_endpoints = function(x, p) {
  st_nearest_feature(st_geometry(p), nodes_as_sf(x))
}

#' @importFrom igraph edge_attr
#' @importFrom tidygraph activate with_graph
set_path_weights = function(x, weights) {
  # Case 1: Weights is a character pointing to a column in the edges table.
  # --> Return the values in that column, if it exists.
  if (is.character(weights) & length(weights) == 1) {
    values = edge_attr(x, weights)
    if (is.null(values)) {
      stop(
        "Edge attribute '", weights, "' not found",
        call. = FALSE
      )
    }
    values
  }
  # Case 2: Weights is NULL and there is no 'weight' column in the edges table.
  # --> Use the length of the edge linestrings as weights.
  if (is.null(weights) & is.null(edge_attr(x, "weight"))) {
    with_graph(activate(x, "edges"), edge_length())
  }
  # All other cases: igraph will handle the given weights.
  # No need for pre-processing.
  weights
}
