#' Find paths between nodes in a spatial network
#'
#' A function implementing one-to-one and one-to-many routing on spatial
#' networks. It can be used to either find one shortest path, all shortest
#' paths, or all simple paths between one node and one or
#' more other nodes in the network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param from The node where the paths should start. Can be an integer
#' specifying its index or a character specifying its name. Can also be an
#' object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} containing a
#' single spatial feature. In that case, this feature will be snapped to its
#' nearest node before finding the paths. When multiple indices, names or
#' features are given, only the first one is used.
#'
#' @param to The nodes where the paths should end. Can be an integer vector
#' specifying their indices or a character vector specifying their name. Can
#' also be an object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' containing spatial features. In that case, these feature will be snapped to
#' their nearest node before finding the paths. By default, all nodes in the
#' network are included.
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
#' performed. If set to \code{'shortest'} paths are found using
#' \code{\link[igraph]{shortest_paths}}, if set to \code{'all_shortest'} paths
#' are found using \code{\link[igraph]{all_shortest_paths}}, if set to
#' \code{'all_simple'} paths are found using
#' \code{\link[igraph]{all_simple_paths}}. Defaults to \code{'shortest'}.
#'
#' @param direction The direction of travel. Defaults to \code{'out'}, meaning
#' that the direction given by the network is followed and paths are found from
#' the node given as argument \code{from}. May be set to \code{'in'}, meaning
#' that the opposite direction is followed an paths are found towards the node
#' given as argument \code{from}. May also be set to \code{'all'}, meaning that
#' the network is considered to be undirected. This argument is ignored for
#' undirected networks.
#'
#' @param use_names If a column named \code{name} is present in the nodes
#' table, should these names be used to encode the nodes in a path, instead of
#' the node indices? Defaults to \code{TRUE}. Ignored when the nodes table does
#' not have a column named \code{name}.
#'
#' @param return_cost Should the total cost of each path be computed? Defaults
#' to \code{TRUE}. Ignored if \code{type = 'all_simple'}.
#'
#' @param return_geometry Should a linestring geometry be constructed for each
#' path? Defaults to \code{TRUE}. The geometries are constructed by calling
#' \code{\link[sf]{st_line_merge}} on the linestring geometries of the edges in
#' the path. Ignored if \code{type = 'all_simple'} and for networks with
#' spatially implicit edges.
#'
#' @param ... Additional arguments passed on to the wrapped igraph functions.
#' Arguments \code{predecessors} and \code{inbound.edges} are ignored.
#' Instead of the \code{mode} argument, use the \code{direction} argument.
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
#' When computing simple paths by setting \code{type = 'all_simple'}, note that
#' potentially there are exponentially many paths between two nodes, and you
#' may run out of memory especially in undirected, dense, and/or lattice-like
#' networks.
#'
#' For more details on the wrapped igraph functions see the
#' \code{\link[igraph]{distances}} and
#' \code{\link[igraph]{all_simple_paths}} documentation pages.
#'
#' @seealso \code{\link{st_network_cost}}
#'
#' @return An object of class \code{\link[tibble]{tbl_df}} or
#' \code{\link[sf]{sf}} with one row per path. If \code{type = 'shortest'}, the
#' number of rows is always equal to the number of requested paths, meaning
#' that node pairs for which no path could be found are still part of the
#' output. For all other path types, the output only contains finite paths.
#'
#' Depending on the argument setting, the output may include the following
#' columns:
#'
#' \itemize{
#'   \item \code{from}: The index of the node at the start of the path.
#'   \item \code{to}: The index of the node at the end of the path.
#'   \item \code{nodes}: A vector containing the indices of all nodes on the
#'   path, in order of visit.
#'   \item \code{edges}: A vector containing the indices of all edges on the
#'   path, in order of visit. Not returned if \code{type = 'all_simple'}.
#'   \item \code{path_found}: A boolean describing if a path was found between
#'   the two nodes. Returned only if \code{type = 'shortest'}.
#'   \item \code{cost}: The total cost of the path, obtained by summing the
#'   weights of all visited edges. Returned only if \code{return_cost = TRUE}.
#'   Never returned if \code{type = 'all_simple'}.
#'   \item \code{geometry}: The geometry of the path, obtained by merging the
#'   geometries of all visited edges. Returned only if
#'   \code{return_geometry = TRUE} and the network has spatially explicit
#'   edges. Never returned if \code{type = 'all_simple'}.
#' }
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#'
#' net = as_sfnetwork(roxel, directed = FALSE) |>
#'   st_transform(3035)
#'
#' # Compute the shortest path between two nodes.
#' # Note that geographic edge length is used as edge weights by default.
#' paths = st_network_paths(net, from = 495, to = 121)
#' paths
#'
#' plot(net, col = "grey")
#' plot(st_geometry(paths), col = "red", lwd = 1.5, add = TRUE)
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
#' plot(net, col = "grey")
#' plot(c(p1, p2), col = "black", pch = 8, add = TRUE)
#' plot(st_geometry(paths), col = "red", lwd = 1.5, add = TRUE)
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
#' par(oldpar)
#'
#' @export
st_network_paths = function(x, from, to = node_ids(x),
                            weights = edge_length(), type = "shortest",
                            direction = "out", use_names = TRUE,
                            return_cost = TRUE, return_geometry = TRUE, ...) {
  UseMethod("st_network_paths")
}

#' @importFrom igraph vertex_attr vertex_attr_names
#' @importFrom rlang has_name
#' @importFrom sf st_as_sf
#' @export
st_network_paths.sfnetwork = function(x, from, to = node_ids(x),
                                      weights = edge_length(),
                                      type = "shortest", direction = "out",
                                      use_names = TRUE, return_cost = TRUE,
                                      return_geometry = TRUE, ...) {
  # Evaluate the given from node query.
  from = evaluate_node_query(from)
  if (length(from) > 1) raise_multiple_elements("from"); from = from[1]
  if (any(is.na(from))) raise_na_values("from")
  # Evaluate the given to node query.
  to = evaluate_node_query(to)
  if (any(is.na(to))) raise_na_values("to")
  # Evaluate the given weights specification.
  weights = evaluate_weight_spec(x, weights)
  # Compute the shortest paths.
  paths = igraph_paths(x, from, to, weights, type, direction, ...)
  # Convert node indices to node names if requested.
  if (use_names && "name" %in% vertex_attr_names(x)) {
    nnames = vertex_attr(x, "name")
    paths$from = do.call("c", lapply(paths$from, \(x) nnames[x]))
    paths$to = do.call("c", lapply(paths$to, \(x) nnames[x]))
    paths$nodes = lapply(paths$nodes, \(x) nnames[x])
  }
  # Enrich the paths with additional information.
  if (has_name(paths, "edges")) {
    E = paths$edges
    # Compute total cost of each path if requested.
    if (return_cost) {
      if (length(weights) == 1 && is.na(weights)) {
        costs = do.call("c", lapply(paths$edges, length))
      } else {
        costs = do.call("c", lapply(paths$edges, \(x) sum(weights[x])))
      }
      if (has_name(paths, "path_found")) costs[!paths$path_found] = Inf
      paths$cost = costs
    }
    # Construct path geometries of requested.
    if (return_geometry && has_explicit_edges(x)) {
      egeom = pull_edge_geom(x)
      pgeom = do.call("c", lapply(paths$edges, \(x) merge_lines(egeom[x])))
      paths$geometry = pgeom
      paths = st_as_sf(paths)
    }
  }
  paths
}

#' @importFrom igraph all_shortest_paths all_simple_paths shortest_paths
#' igraph_opt igraph_options
#' @importFrom methods hasArg
#' @importFrom tibble tibble
igraph_paths = function(x, from, to, weights, type = "shortest",
                        direction = "out", ...) {
  # Change default igraph options.
  # This prevents igraph returns node or edge indices as formatted sequences.
  # We only need the "raw" integer indices.
  # Changing this option improves performance especially on large networks.
  default_igraph_opt = igraph_opt("return.vs.es")
  igraph_options(return.vs.es = FALSE)
  on.exit(igraph_options(return.vs.es = default_igraph_opt))
  # The direction argument is used instead of igraphs mode argument.
  # This means the mode argument should not be set.
  if (hasArg("mode")) raise_unsupported_arg("mode", replacement = "direction")
  # Call igraph paths calculation function according to type argument.
  paths = switch(
    type,
    shortest = shortest_paths(
      x, from, to,
      weights = weights,
      output = "both",
      mode = direction,
      ...
    ),
    all_shortest = all_shortest_paths(
      x, from, to,
      weights = weights,
      mode = direction,
      ...
    ),
    all_simple = list(vpaths = all_simple_paths(
      x, from, to,
      mode = direction,
      ...
    )),
    raise_unknown_input("type", type, c("shortest", "all_shortest", "all_simple"))
  )
  # Extract the nodes in the paths, and the edges in the paths (if given).
  npaths = paths[[1]]
  epaths = if (length(paths) > 1) paths[[2]] else NULL
  # Define the nodes from which the returned paths start and at which they end.
  if (type == "shortest") {
    starts = rep(from, length(to))
    ends = to
    path_found = lengths(epaths) > 0 | starts == ends
  } else {
    starts = do.call("c", lapply(npaths, `[`, 1))
    ends = do.call("c", lapply(npaths, last_element))
    path_found = NULL
  }
  # Return in a tibble.
  tibble(
    from = starts, to = ends,
    nodes = npaths, edges = epaths,
    path_found = path_found
  )
}

#' Compute a cost matrix of a spatial network
#'
#' Compute total travel costs of shortest paths between nodes in a spatial
#' network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param from The nodes where the paths should start. Can be an integer vector
#' specifying their indices or a character vector specifying their name. Can
#' also be an object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' containing spatial features. In that case, these feature will be snapped to
#' their nearest node before finding the paths. By default, all nodes in the
#' network are included.
#'
#' @param to The nodes where the paths should end. Can be an integer vector
#' specifying their indices or a character vector specifying their name. Can
#' also be an object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' containing spatial features. In that case, these feature will be snapped to
#' their nearest node before finding the paths. By default, all nodes in the
#' network are included.
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
#' that the direction given by the network is followed and costs are computed
#' from the points given as argument \code{from}. May be set to \code{'in'},
#' meaning that the opposite direction is followed an costs are computed
#' towards the points given as argument \code{from}. May also be set to
#' \code{'all'}, meaning that the network is considered to be undirected. This
#' argument is ignored for undirected networks.
#'
#' @param Inf_as_NaN Should the cost values of unconnected nodes be stored as
#' \code{NaN} instead of \code{Inf}? Defaults to \code{FALSE}.
#'
#' @param ... Additional arguments passed on to \code{\link[igraph]{distances}}.
#' Instead of the \code{mode} argument, use the \code{direction} argument.
#'
#' @details \code{st_network_cost} allows to use any set of edge weights, while
#' \code{st_network_distance} is a intuitive synonym for cost matrix computation
#' in which the edge weights are set to their geographic length.
#'
#' Spatial features provided to the \code{from} and/or \code{to} argument don't
#' necessarily have to be points. Internally, the nearest node to each feature
#' is found by calling \code{\link[sf]{st_nearest_feature}}, so any feature
#' with a geometry type that is accepted by that function can be provided as
#' \code{from} and/or \code{to} argument.
#'
#' When directly providing integer node indices or character node names to the
#' \code{from} and/or \code{to} argument, keep the following in mind. A node
#' index should correspond to a row-number of the nodes table of the network.
#' A node name should correspond to a value of a column in the nodes table
#' named \code{name}. This column should contain character values without
#' duplicates.
#'
#' For more details on the wrapped igraph function see the
#' \code{\link[igraph]{distances}} documentation page.
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
#' # st_network_distance is a synonym for st_network_cost with default weights.
#' st_network_distance(net, from = c(495, 121), to = c(495, 121))
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
#' @export
st_network_cost = function(x, from = node_ids(x), to = node_ids(x),
                           weights = edge_length(), direction = "out",
                           Inf_as_NaN = FALSE, ...) {
  UseMethod("st_network_cost")
}

#' @importFrom igraph distances
#' @importFrom methods hasArg
#' @importFrom units as_units deparse_unit
#' @export
st_network_cost.sfnetwork = function(x, from = node_ids(x), to = node_ids(x),
                                     weights = edge_length(),
                                     direction = "out",
                                     Inf_as_NaN = FALSE, ...) {
  # Evaluate the given from node query.
  from = evaluate_node_query(from)
  if (any(is.na(from))) raise_na_values("from")
  # Evaluate the given to node query.
  to = evaluate_node_query(to)
  if (any(is.na(to))) raise_na_values("to")
  # Evaluate the given weights specification.
  weights = evaluate_weight_spec(x, weights)
  # Parse other arguments.
  # --> The direction argument is used instead of igraphs mode argument.
  # --> This means the mode argument should not be set.
  if (hasArg("mode")) raise_unsupported_arg("mode", replacement = "direction")
  # Call the igraph distances function to compute the cost matrix.
  # Special attention is required if there are duplicated 'to' nodes:
  # --> In igraph this cannot be handled.
  # --> Therefore we call igraph::distances with unique 'to' nodes.
  # --> Afterwards we copy cost values to duplicated 'to' nodes.
  if(any(duplicated(to))) {
    tou = unique(to)
    matrix = distances(x, from, tou, weights = weights, mode = direction, ...)
    matrix = matrix[, match(to, tou), drop = FALSE]
  } else {
    matrix = distances(x, from, to, weights = weights, mode = direction, ...)
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

#' @name st_network_cost
#' @export
st_network_distance = function(x, from = node_ids(x), to = node_ids(x),
                               direction = "out", Inf_as_NaN = FALSE, ...) {
  st_network_cost(
    x, from, to,
    weights = edge_length(),
    direction = direction,
    Inf_as_NaN = Inf_as_NaN,
    ...
  )
}
