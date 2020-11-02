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
#' are given, only the first one is taken. Empty geometries are ignored.
#' Alternatively, it can be a numeric constant, referring to the index of the
#' node from which the shortest paths will be calculated. Only in the case of
#' \code{st_network_distances} the restriction of a single feature does not
#' apply. Then, it can also be an \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' object with multiple features, or alternatively a vector of node indices.
#'
#' @param to The (set of) geospatial point(s) to which the shortest paths will be
#' calculated. Can be an object of  class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}. Empty geometries will be ignored.
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
#' \code{\link[igraph:shortest_paths]{igraph}} function. Arguments 
#' \code{predecessors} and \code{inbound.edges} are ignored.
#'
#' @details See the \code{\link[igraph:shortest_paths]{igraph}} documentation.
#'
#' @return For \code{st_shortest_paths} and \code{st_all_shortest_paths}, an 
#' object of class \code{\link[tibble]{tbl_df}} with one row per returned 
#' path. Depending on the setting of the 'output' argument, columns can
#' be \code{node_paths} (a list column with for each path the ordered indices 
#' of nodes present in that path) and \code{edge_paths} (a list column with 
#' for each path the ordered indices of edges present in that path). 
#' 
#' For \code{st_network_distance}, an nxm matrix where n is the length of the 
#' 'from' argument, and m is the length of the 'to' argument.
#'
#' @name spatial_shortest_paths
NULL

#' @describeIn spatial_shortest_paths Wrapper around
#' \code{igraph::shortest_paths}.
#'
#' @param output Character defining how to report the shortest paths. Can be 
#' \code{'nodes'} meaning that only indices of nodes in the paths are
#' returned, \code{'edges'} meaning that only indices of edges in the paths
#' are returned, or \code{'both'} meaning that both node and edge indices are
#' returned. Defaults to \code{'both'}.
#' 
#' @examples
#' library(sf)
#' library(tidygraph)
#'
#' net = as_sfnetwork(roxel, directed = FALSE) %>%
#'   st_transform(3035)
#'
#' # 1. Providing node indices
#'
#' st_shortest_paths(net, 1, 9)
#'
#' # 2. Providing nodes as spatial points
#'
#' p1 = st_geometry(net, "nodes")[1]
#' p2 = st_geometry(net, "nodes")[9]
#' st_shortest_paths(net, p1, p2)
#'
#' # 3. Providing spatial points outside of the network
#'
#' p3 = st_sfc(p1[[1]] + st_point(c(500, 500)), crs = st_crs(p1))
#' p4 = st_sfc(p2[[1]] + st_point(c(-500, -500)), crs = st_crs(p2))
#' st_shortest_paths(net, p3, p4)
#'
#' # 4. Providing weights from column name
#'
#' net %>%
#'   activate("edges") %>%
#'   mutate(length = edge_length()) %>%
#'   st_shortest_paths(p1, p2, weights = "length")
#'
#' # 5. Providing weights from column named 'weight'
#'
#' net %>%
#'   activate("edges") %>%
#'   mutate(weight = length) %>%
#'   st_shortest_paths(p1, p2)
#'
#' @importFrom igraph V
#' @export
st_shortest_paths = function(x, from, to = igraph::V(x), weights = NULL, 
                             output = "both", ...) {
  UseMethod("st_shortest_paths")
}

#' @importFrom igraph shortest_paths V
#' @importFrom tibble as_tibble
#' @export
st_shortest_paths.sfnetwork = function(x, from, to = igraph::V(x), 
                                       weights = NULL, output = "both", ...) {
  # Set common shortest paths arguments.
  args = set_paths_args(x, from, to, weights)
  # Set output.
  output = switch(
    output,
    both = list(output = "both"),
    nodes = list(output = "vpath"),
    edges = list(output = "epath"),
    raise_unknown_input(output)
  )
  # Call igraph function.
  paths = do.call(shortest_paths, c(args, output, ...))
  # Extract paths of node indices and edge indices.
  npaths = lapply(paths[[1]], as.integer)
  epaths = lapply(paths[[2]], as.integer)
  # Return node and edge paths as columns in a tibble.
  as_tibble(do.call(cbind, list(node_paths = npaths, edge_paths = epaths)))
}

#' @describeIn spatial_shortest_paths Wrapper around
#' \code{igraph::all_shortest_paths}.
#'
#' @examples
#' # 7. Calculate all shortest paths between two points
#'
#' st_all_shortest_paths(net, 5, 1)
#'
#' @importFrom igraph V
#' @export
st_all_shortest_paths = function(x, from, to = igraph::V(x), weights = NULL) {
  UseMethod("st_all_shortest_paths")
}

#' @importFrom igraph all_shortest_paths V
#' @export
st_all_shortest_paths.sfnetwork = function(x, from, to = igraph::V(x), 
                                           weights = NULL) {
  args = set_paths_args(x, from, to, weights)
  paths = do.call(all_shortest_paths, args)
  # Extract paths of node indices.
  npaths = lapply(paths[[1]], as.integer)
  # Return as column in a tibble.
  as_tibble(do.call(cbind, list(node_paths = npaths)))
}

#' @describeIn spatial_shortest_paths Wrapper around \code{igraph::distances}.
#'
#' @examples
#' # 8. Compute a distance matrix with network distances
#'
#' pts1 = c(p1, p3)
#' pts2 = c(p2, p4)
#' st_network_distance(net, pts1, pts2)
#'
#' @importFrom igraph V
#' @export
st_network_distance = function(x, from = igraph::V(x), to = igraph::V(x), 
                               weights = NULL, ...) {
  UseMethod("st_network_distance")
}

#' @importFrom igraph distances V
#' @export
st_network_distance.sfnetwork = function(x, from = igraph::V(x), to = igraph::V(x), 
                                         weights = NULL, ...) {
  args = set_paths_args(x, from, to, weights)
  names(args)[2] = "v" # In igraph::distances argument 'from' is called 'v'
  do.call(distances, c(args, ...))
}

#' @importFrom igraph ecount edge_attr
set_paths_args = function(x, from, to, weights) {
  if (length(from) > 1) {
    warning(
      "Although argument 'from' has length > 1, ",
      "only the first element is used",
      call. = FALSE
    )
  }
  from = set_path_endpoints(x, from, name = "from")
  # Set to.
  to = set_path_endpoints(x, to, name = "to")
  # Set weights.
  if (is.character(weights)) weights = edge_attr(x, weights)
  # Return in a named list.
  list(graph = x, from = from, to = to, weights = weights)
}

#' @importFrom sf st_geometry st_nearest_feature
set_path_endpoints = function(x, p, name) {
  # Case 1: input is geospatial point geometries.
  if (is.sf(p) || is.sfc(p)) {
    missing = is_empty(p)
    if (any(missing)) {
      if (all(missing)) {
        stop(
          "Geometries in argument '", name, "' are all empty",
          call. = FALSE
        )
      } else {
        warning(
          "Empty geometries in argument '", name, "' are ignored",
          call. = FALSE
        )
      }
    }
    return (st_nearest_node(p[!missing], x, geometries = FALSE))
  } 
  # Case 2: input is numeric node indices.
  if (is.numeric(p)) {
    missing = is.na(p)
    if (any(missing)) {
      if (all(missing)) {
        stop(
          "Indices in argument '", name, "' are all NA",
          call. = FALSE
        )
      } else {
        warning(
          "NA indices in argument '", name, "' are ignored",
          call. = FALSE
        )
      }
    }
    return (p[!missing])
  }
  stop(
    "Objects of class ", 
    class(p), 
    " not accepted as input to argument '", name, "'",
    call. = FALSE
  )
}
