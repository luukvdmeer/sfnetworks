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
#' are given, only the first one is used.
#' Alternatively, it can be an integer, referring to the index of the
#' node from which the paths will be calculated, or a character,
#' referring to the name of the node from which the paths will be
#' calculated.
#'
#' @param to The (set of) geospatial point(s) to which the paths will be
#' calculated. Can be an object of  class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#' Alternatively it can be a numeric vector containing the indices of the nodes
#' to which the paths will be calculated, or a character vector
#' containing the names of the nodes to which the paths will be
#' calculated. By default, all nodes in the network are included.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Can be a numeric vector of the same length as the number of edges, a
#' \link[=spatial_edge_measures]{spatial edge measure function}, or a column in
#' the edges table of the network. Tidy evaluation is used such that column
#' names can be specified as if they were variables in the environment (e.g.
#' simply \code{length} instead of \code{igraph::edge_attr(x, "length")}).
#' If set to \code{NULL} or \code{NA} no edge weights are used, and the
#' shortest path is the path with the fewest number of edges, ignoring space.
#' The default is \code{\link{edge_length}}, which computes the geographic
#' lengths of the edges.
#'
#' @param type Character defining which type of path calculation should be
#' performed. If set to \code{'shortest'} paths are calculated using
#' \code{\link[igraph]{shortest_paths}}, if set to
#' \code{'all_shortest'} paths are calculated using
#' \code{\link[igraph]{all_shortest_paths}}, if set to
#' \code{'all_simple'} paths are calculated using
#' \code{\link[igraph]{all_simple_paths}}. Defaults to \code{'shortest'}.
#'
#' @param use_names If a column named \code{name} is present in the nodes
#' table, should these names be used to encode the nodes in a path, instead of
#' the node indices? Defaults to \code{TRUE}. Ignored when the nodes table does
#' not have a column named \code{name}.
#'
#' @param ... Arguments passed on to the corresponding
#' \code{\link[igraph:shortest_paths]{igraph}} or
#' \code{\link[igraph:all_simple_paths]{igraph}} function. Arguments
#' \code{predecessors} and \code{inbound.edges} are ignored.
#'
#' @details Spatial features provided to the \code{from} and/or
#' \code{to} argument don't necessarily have to be points. Internally, the
#' nearest node to each feature is found by calling
#' \code{\link[sf]{st_nearest_feature}}, so any feature with a geometry type
#' that is accepted by that function can be provided as \code{from} and/or
#' \code{to} argument.
#'
#' When directly providing integer node indices or character node names to the
#' \code{from} and/or \code{to} argument, keep the following in mind. A node
#' index should correspond to a row-number of the nodes table of the network.
#' A node name should correspond to a value of a column in the nodes table
#' named \code{name}. This column should contain character values without
#' duplicates.
#'
#' For more details on the wrapped functions from \code{\link[igraph]{igraph}}
#' see the \code{\link[igraph]{shortest_paths}} or
#' \code{\link[igraph]{all_simple_paths}} documentation pages.
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
#' net = as_sfnetwork(roxel, directed = FALSE) |>
#'   st_transform(3035)
#'
#' # Compute the shortest path between two nodes.
#' # Note that geographic edge length is used as edge weights by default.
#' paths = st_network_paths(net, from = 495, to = 121)
#' paths
#'
#' node_path = paths |>
#'   slice(1) |>
#'   pull(node_paths) |>
#'   unlist()
#'
#' node_path
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#'
#' plot(net, col = "grey")
#' plot(slice(net, node_path), col = "red", add = TRUE)
#' par(oldpar)
#'
#' # Compute the shortest paths from one to multiple nodes.
#' # This will return a tibble with one row per path.
#' st_network_paths(net, from = 495, to = c(121, 131, 141))
#'
#' # Compute the shortest path between two spatial point features.
#' # These are snapped to their nearest node before finding the path.
#' p1 = st_geometry(net, "nodes")[495] + st_sfc(st_point(c(50, -50)))
#' st_crs(p1) = st_crs(net)
#' p2 = st_geometry(net, "nodes")[121] + st_sfc(st_point(c(-10, 100)))
#' st_crs(p2) = st_crs(net)
#'
#' paths = st_network_paths(net, from = p1, to = p2)
#' paths
#'
#' node_path = paths |>
#'   slice(1) |>
#'   pull(node_paths) |>
#'   unlist()
#'
#' node_path
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#'
#' plot(net, col = "grey")
#' plot(c(p1, p2), col = "black", pch = 8, add = TRUE)
#' plot(slice(net, node_path), col = "red", add = TRUE)
#' par(oldpar)
#'
#' # Use a spatial edge measure to specify edge weights.
#' # By default edge_length() is used.
#' st_network_paths(net, p1, p2, weights = edge_displacement())
#'
#' # Use a column in the edges table to specify edge weights.
#' # This uses tidy evaluation.
#' net |>
#'   activate("edges") |>
#'   mutate(foo = runif(n(), min = 0, max = 1)) |>
#'   st_network_paths(p1, p2, weights = foo)
#'
#' # Compute the shortest paths without edge weights.
#' # This is the path with the fewest number of edges, ignoring space.
#' st_network_paths(net, p1, p2, weights = NULL)
#'
#' # Compute all shortest paths between two nodes.
#' # If there is more than one shortest path, this returns one path per row.
#' st_network_paths(net, from = 5, to = 1, type = "all_shortest")
#'
#' @importFrom igraph V
#' @export
st_network_paths = function(x, from, to = igraph::V(x),
                            weights = edge_length(), type = "shortest",
                            use_names = TRUE, ...) {
  UseMethod("st_network_paths")
}

#' @importFrom igraph V
#' @importFrom rlang enquo eval_tidy expr
#' @importFrom sf st_geometry
#' @importFrom tidygraph .E .register_graph_context
#' @export
st_network_paths.sfnetwork = function(x, from, to = igraph::V(x),
                                      weights = edge_length(),
                                      type = "shortest",
                                      use_names = TRUE, ...) {
  # Parse from and to arguments.
  # --> Convert geometries to node indices.
  # --> Raise warnings when igraph requirements are not met.
  if (is_sf(from) | is_sfc(from)) from = get_nearest_node_index(x, from)
  if (is_sf(to) | is_sfc(to)) to = get_nearest_node_index(x, to)
  if (length(from) > 1) raise_multiple_elements("from")
  if (any(is.na(c(from, to)))) raise_na_values("from and/or to")
  # Parse weights argument using tidy evaluation on the network edges.
  .register_graph_context(x, free = TRUE)
  weights = enquo(weights)
  weights = eval_tidy(weights, .E())
  if (is_single_string(weights)) {
    # Allow character values for backward compatibility.
    deprecate_weights_is_string("st_network_paths")
    weights = eval_tidy(expr(.data[[weights]]), .E())
  }
  if (is.null(weights)) {
    # Convert NULL to NA to align with tidygraph instead of igraph.
    deprecate_weights_is_null("st_network_paths")
    weights = NA
  }
  # Call paths calculation function according to type argument.
  switch(
    type,
    shortest = get_shortest_paths(x, from, to, weights, use_names,...),
    all_shortest = get_all_shortest_paths(x, from, to, weights, use_names,...),
    all_simple = get_all_simple_paths(x, from, to, use_names,...),
    raise_unknown_input(type)
  )
}

#' @importFrom igraph shortest_paths vertex_attr_names
#' @importFrom tibble as_tibble
get_shortest_paths = function(x, from, to, weights, use_names = TRUE, ...) {
  # Call igraph function.
  paths = shortest_paths(x, from, to, weights = weights, output = "both", ...)
  # Extract vector of node indices or names.
  if (use_names && "name" %in% vertex_attr_names(x)) {
    npaths = lapply(paths[[1]], attr, "names")
  } else {
    npaths = lapply(paths[[1]], as.integer)
  }
  # Extract vector of edge indices.
  epaths = lapply(paths[[2]], as.integer)
  # Return as columns in a tibble.
  as_tibble(do.call(cbind, list(node_paths = npaths, edge_paths = epaths)))
}

#' @importFrom igraph all_shortest_paths vertex_attr_names
#' @importFrom tibble as_tibble
get_all_shortest_paths = function(x, from, to, weights, use_names = TRUE,...) {
  # Call igraph function.
  paths = all_shortest_paths(x, from, to, weights = weights, ...)
  # Extract vector of node indices or names.
  if (use_names && "name" %in% vertex_attr_names(x)) {
    npaths = lapply(paths[[1]], attr, "names")
  } else {
    npaths = lapply(paths[[1]], as.integer)
  }
  # Return as column in a tibble.
  as_tibble(do.call(cbind, list(node_paths = npaths)))
}

#' @importFrom igraph all_simple_paths vertex_attr_names
#' @importFrom tibble as_tibble
get_all_simple_paths = function(x, from, to, use_names = TRUE, ...) {
  # Call igraph function.
  paths = all_simple_paths(x, from, to, ...)
  # Extract paths of node indices.
  if (use_names && "name" %in% vertex_attr_names(x)) {
    npaths = lapply(paths, attr, "names")
  } else {
    npaths = lapply(paths, as.integer)
  }
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
#' Alternatively it can be a numeric vector containing the indices of the nodes
#' from which the shortest paths will be calculated, or a character vector
#' containing the names of the nodes from which the shortest paths will be
#' calculated. By default, all nodes in the network are included.
#'
#' @param to The (set of) geospatial point(s) to which the shortest paths will
#' be calculated. Can be an object of class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#' Alternatively it can be a numeric vector containing the indices of the nodes
#' to which the shortest paths will be calculated, or a character vector
#' containing the names of the nodes to which the shortest paths will be
#' calculated. Duplicated values will be removed before calculating the cost
#' matrix. By default, all nodes in the network are included.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Can be a numeric vector of the same length as the number of edges, a
#' \link[=spatial_edge_measures]{spatial edge measure function}, or a column in
#' the edges table of the network. Tidy evaluation is used such that column
#' names can be specified as if they were variables in the environment (e.g.
#' simply \code{length} instead of \code{igraph::edge_attr(x, "length")}).
#' If set to \code{NULL} or \code{NA} no edge weights are used, and the
#' shortest path is the path with the fewest number of edges, ignoring space.
#' The default is \code{\link{edge_length}}, which computes the geographic
#' lengths of the edges.
#'
#' @param direction The direction of travel. Defaults to \code{'out'}, meaning
#' that the direction given by the network is followed and costs are calculated
#' from the points given as argument \code{from}. May be set to \code{'in'},
#' meaning that the opposite direction is followed an costs are calculated
#' towards the points given as argument \code{from}. May also be set to
#' \code{'all'}, meaning that the network is considered to be undirected. This
#' argument is ignored for undirected networks.
#'
#' @param Inf_as_NaN Should the cost values of unconnected nodes be stored as
#' \code{NaN} instead of \code{Inf}? Defaults to \code{FALSE}.
#'
#' @param ... Arguments passed on to \code{\link[igraph]{distances}}. Argument
#' \code{mode} is ignored. Use \code{direction} instead.
#'
#' @details Spatial features provided to the \code{from} and/or
#' \code{to} argument don't necessarily have to be points. Internally, the
#' nearest node to each feature is found by calling
#' \code{\link[sf]{st_nearest_feature}}, so any feature with a geometry type
#' that is accepted by that function can be provided as \code{from} and/or
#' \code{to} argument.
#'
#' When directly providing integer node indices or character node names to the
#' \code{from} and/or \code{to} argument, keep the following in mind. A node
#' index should correspond to a row-number of the nodes table of the network.
#' A node name should correspond to a value of a column in the nodes table
#' named \code{name}. This column should contain character values without
#' duplicates.
#'
#' For more details on the wrapped function from \code{\link[igraph]{igraph}}
#' see the \code{\link[igraph]{distances}} documentation page.
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
#' net = as_sfnetwork(roxel, directed = FALSE) |>
#'   st_transform(3035)
#'
#' # Compute the network cost matrix between node pairs.
#' # Note that geographic edge length is used as edge weights by default.
#' st_network_cost(net, from = c(495, 121), to = c(495, 121))
#'
#' # Compute the network cost matrix between spatial point features.
#' # These are snapped to their nearest node before computing costs.
#' p1 = st_geometry(net, "nodes")[495] + st_sfc(st_point(c(50, -50)))
#' st_crs(p1) = st_crs(net)
#' p2 = st_geometry(net, "nodes")[121] + st_sfc(st_point(c(-10, 100)))
#' st_crs(p2) = st_crs(net)
#'
#' st_network_cost(net, from = c(p1, p2), to = c(p1, p2))
#'
#' # Use a spatial edge measure to specify edge weights.
#' # By default edge_length() is used.
#' st_network_cost(net, c(p1, p2), c(p1, p2), weights = edge_displacement())
#'
#' # Use a column in the edges table to specify edge weights.
#' # This uses tidy evaluation.
#' net |>
#'   activate("edges") |>
#'   mutate(foo = runif(n(), min = 0, max = 1)) |>
#'   st_network_cost(c(p1, p2), c(p1, p2), weights = foo)
#'
#' # Compute the cost matrix without edge weights.
#' # Here the cost is defined by the number of edges, ignoring space.
#' st_network_cost(net, c(p1, p2), c(p1, p2), weights = NULL)
#'
#' # Not providing any from or to points includes all nodes by default.
#' with_graph(net, graph_order()) # Our network has 701 nodes.
#' cost_matrix = st_network_cost(net)
#' dim(cost_matrix)
#'
#' @importFrom igraph V
#' @export
st_network_cost = function(x, from = igraph::V(x), to = igraph::V(x),
                           weights = edge_length(), direction = "out",
                           Inf_as_NaN = FALSE, ...) {
  UseMethod("st_network_cost")
}

#' @importFrom igraph distances V
#' @importFrom rlang enquo eval_tidy expr
#' @importFrom tidygraph .E .register_graph_context
#' @importFrom units as_units deparse_unit
#' @export
st_network_cost.sfnetwork = function(x, from = igraph::V(x), to = igraph::V(x),
                                     weights = edge_length(),
                                     direction = "out",
                                     Inf_as_NaN = FALSE, ...) {
  # Parse from and to arguments.
  # --> Convert geometries to node indices.
  # --> Raise warnings when igraph requirements are not met.
  if (is_sf(from) | is_sfc(from)) from = get_nearest_node_index(x, from)
  if (is_sf(to) | is_sfc(to)) to = get_nearest_node_index(x, to)
  if (any(is.na(c(from, to)))) raise_na_values("from and/or to")
  # Parse weights argument using tidy evaluation on the network edges.
  .register_graph_context(x, free = TRUE)
  weights = enquo(weights)
  weights = eval_tidy(weights, .E())
  if (is_single_string(weights)) {
    # Allow character values for backward compatibility.
    deprecate_weights_is_string("st_network_cost")
    weights = eval_tidy(expr(.data[[weights]]), .E())
  }
  if (is.null(weights)) {
    # Convert NULL to NA to align with tidygraph instead of igraph.
    deprecate_weights_is_null("st_network_cost")
    weights = NA
  }
  # Parse other arguments.
  # --> The mode argument in ... is ignored in favor of the direction argument.
  dots = list(...)
  if (!is.null(dots$mode)) {
    dots$mode = NULL
    warning(
      "Argument `mode` is ignored, use `direction` instead.",
      call. = FALSE
    )
  }
  # Call the igraph distances function to compute the cost matrix.
  # Special attention is required if there are duplicated 'to' nodes:
  # --> In igraph this cannot be handled.
  # --> Therefore we call igraph::distances with unique 'to' nodes.
  # --> Afterwards we copy cost values to duplicated 'to' nodes.
  if(any(duplicated(to))) {
    to_unique = unique(to)
    match = match(to, to_unique)
    args = list(x, from, to_unique, weights = weights, mode = direction)
    matrix = do.call(igraph::distances, c(args, dots))
    matrix = matrix[, match, drop = FALSE]
  } else {
    args = list(x, from, to, weights = weights, mode = direction)
    matrix = do.call(igraph::distances, c(args, dots))
  }
  # Post-process and return.
  # --> Convert Inf to NaN if requested.
  # --> Attach units if the provided weights had units.
  if (Inf_as_NaN) matrix[is.infinite(matrix)] = NaN
  if (inherits(weights, "units")) {
    as_units(matrix, deparse_unit(weights))
  } else {
    matrix
  }
}
