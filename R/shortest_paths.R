#' Shortest paths between points in geographical space
#'
#' Wrapper around the shortest path calculation functionalities in
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
#' Alternatively, it can be an integer, referring to the index of the
#' node from which the shortest paths will be calculated, or a character,
#' referring to the name of the node from which the shortest paths will be
#' calculated.
#'
#' @param to The (set of) geospatial point(s) to which the shortest paths will be
#' calculated. Can be an object of  class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}. Empty geometries will be ignored.
#' Alternatively, it can be a numeric vector, containing the indices of the nodes
#' to which the shortest paths will be calculated, or a character vector,
#' containing the names of the nodes to which the shortest paths will be
#' calculated. By default, all nodes in the network are included.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Can be a numeric vector giving edge weights, or a column name referring to
#' an attribute column in the edges table containing those weights. If set to
#' \code{NULL}, the values of a column named 'weight' in the edges table will
#' be used automatically, as long as this column is present. If set to
#' \code{NA}, no weights are used (even if the edges have a weight column).
#'
#' @param output Character defining how to report the shortest paths. Can be
#' \code{'nodes'} meaning that only indices of nodes in the paths are
#' returned, \code{'edges'} meaning that only indices of edges in the paths
#' are returned, or \code{'both'} meaning that both node and edge indices are
#' returned. Defaults to \code{'both'}. Ignored when \code{all = TRUE}.
#'
#' @param all Whether to calculate all shortest paths or a single shortest path
#' between two nodes. If \code{TRUE}, the igraph function
#' \code{all_shortest_paths} is called internally, if \code{FALSE} the igraph
#' function \code{shortest_paths} is called internally. If \code{TRUE}, the
#' returned tibble will only have a \code{node_paths} column, no matter what
#' the setting of \code{output} is. Defaults to \code{FALSE}.
#'
#' @param ... Arguments passed on to the corresponding
#' \code{\link[igraph:shortest_paths]{igraph}} function. Arguments
#' \code{predecessors} and \code{inbound.edges} are ignored.
#'
#' @details See the \code{\link[igraph:shortest_paths]{igraph}} documentation.
#'
#' @seealso \code{\link{st_network_cost}}
#'
#' @return An object of class \code{\link[tibble]{tbl_df}} with one row per
#' returned path. Depending on the setting of the 'output' argument, columns
#' can be \code{node_paths} (a list column with for each path the ordered
#' indices of nodes present in that path) and \code{edge_paths} (a list column
#' with for each path the ordered indices of edges present in that path).
#'
#' @examples
#' library(sf)
#' library(tidygraph)
#'
#' net = as_sfnetwork(roxel, directed = FALSE) %>%
#'   st_transform(3035)
#'
#' # Providing node indices.
#' st_network_paths(net, 1, 9)
#'
#' # Providing nodes as spatial points.
#' p1 = st_geometry(net, "nodes")[1]
#' p2 = st_geometry(net, "nodes")[9]
#' st_network_paths(net, p1, p2)
#'
#' # Providing spatial points outside of the network.
#' p3 = st_sfc(p1[[1]] + st_point(c(500, 500)), crs = st_crs(p1))
#' p4 = st_sfc(p2[[1]] + st_point(c(-500, -500)), crs = st_crs(p2))
#' st_network_paths(net, p3, p4)
#'
#' # Providing weights from column name.
#' net %>%
#'   activate("edges") %>%
#'   mutate(length = edge_length()) %>%
#'   st_network_paths(p1, p2, weights = "length")
#'
#' # Providing weights from column named 'weight'.
#' net %>%
#'   activate("edges") %>%
#'   mutate(weight = edge_length()) %>%
#'   st_network_paths(p1, p2)
#'
#' # Providing weights directly as a numeric vector.
#' net %>%
#'   activate("edges") %>%
#'   st_network_paths(p1, p2, weights = edge_length())
#'
#' # Calculate all shortest paths between two points.
#' st_network_paths(net, 5, 1, all = TRUE)
#'
#' @importFrom igraph V
#' @export
st_network_paths = function(x, from, to = igraph::V(x), weights = NULL,
                             output = "both", all = FALSE, ...) {
  UseMethod("st_network_paths")
}

#' @importFrom igraph all_shortest_paths shortest_paths V
#' @importFrom tibble as_tibble
#' @export
st_network_paths.sfnetwork = function(x, from, to = igraph::V(x),
                                      weights = NULL, output = "both",
                                      all = FALSE, ...) {
  if (length(from) > 1) {
    warning(
      "Although argument 'from' has length > 1, ",
      "only the first element is used",
      call. = FALSE
    )
  }
  # Set common shortest paths arguments.
  args = set_paths_args(x, from, to, weights)
  if(all){
    paths = do.call(all_shortest_paths, args)
    # Extract paths of node indices.
    npaths = lapply(paths[[1]], as.integer)
    # Return as column in a tibble.
    as_tibble(do.call(cbind, list(node_paths = npaths)))
  } else {
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
#' \code{\link[sf]{sfc}}. Empty geometries will be ignored.
#' Alternatively, it can be a numeric vector, containing the indices of the
#' nodes from which the shortest paths will be calculated. By default, all
#' nodes in the network are included.
#'
#' @param to The (set of) geospatial point(s) to which the shortest paths will
#' be calculated. Can be an object of  class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}. Empty geometries will be ignored.
#' Alternatively, it can be a numeric vector, containing the indices of the
#' nodes to which the shortest paths will be calculated. By default, all nodes
#' in the network are included.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Can be a numeric vector giving edge weights, or a column name referring to
#' an attribute column in the edges table containing those weights. If set to
#' \code{NULL}, the values of a column named 'weight' in the edges table will
#' be used automatically, as long as this column is present. If set to
#' \code{NA}, no weights are used (even if the edges have a weight column).
#'
#' @param ... Arguments passed on to \code{\link[igraph]{distances}}.
#'
#' @details See the \code{\link[igraph:distances]{igraph}} documentation.
#'
#' @seealso \code{\link{st_network_paths}}
#'
#' @return An nxm numeric matrix where n is the length of the \code{from}
#' argument, and m is the length of the \code{to} argument.
#'
#' @examples
#' library(sf)
#'
#' net = as_sfnetwork(roxel, directed = FALSE) %>%
#'   st_transform(3035)
#'
#' p1 = st_geometry(net, "nodes")[1]
#' p2 = st_geometry(net, "nodes")[9]
#' p3 = st_sfc(p1[[1]] + st_point(c(500, 500)), crs = st_crs(p1))
#' p4 = st_sfc(p2[[1]] + st_point(c(-500, -500)), crs = st_crs(p2))
#' pts1 = c(p1, p3)
#' pts2 = c(p2, p4)
#'
#' st_network_cost(net, pts1, pts2)
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
  args = set_paths_args(x, from, to, weights)
  names(args)[2] = "v" # In igraph::distances argument 'from' is called 'v'
  # In igraph::distances argument 'to' cannot have duplicated indices
  # This can happen without the user knowing when POINT geometries
  # are given to the 'to' argument that happen to snap to a same node
  if(any(duplicated(args$to))) {
    warning("Duplicated 'to' node indices were removed.")
    args$to = unique(args$to)
  }
  do.call(distances, c(args, ...))
}

#' @importFrom igraph ecount edge_attr
set_paths_args = function(x, from, to, weights) {
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
    return (st_nearest_feature(p[!missing], nodes_as_sf(x)))
  }
  # Case 2: input is numeric node indices.
  if (is.numeric(p) | is.character(p)) {
    missing = is.na(p)
    if (any(missing)) {
      if (all(missing)) {
        stop(
          "Indices or names in argument '", name, "' are all NA",
          call. = FALSE
        )
      } else {
        warning(
          "NA indices or names in argument '", name, "' are ignored",
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
