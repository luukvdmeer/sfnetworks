#' Compute a cost matrix of a spatial network
#'
#' Compute total travel costs of shortest paths between nodes in a spatial
#' network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param from The nodes where the paths should start. Evaluated by
#' \code{\link{evaluate_node_query}}. By default, all nodes in the network are
#' included.
#'
#' @param to The nodes where the paths should end. Evaluated by
#' \code{\link{evaluate_node_query}}. By default, all nodes in the network are
#' included.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Evaluated by \code{\link{evaluate_edge_spec}}. The default is
#' \code{\link{edge_length}}, which computes the geographic lengths of the
#' edges.
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
#' @details For more details on the wrapped igraph function see the
#' \code{\link[igraph]{distances}} documentation page.
#'
#' @seealso \code{\link{st_network_paths}}, \code{\link{st_network_travel}}
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
#' # Use a node type query function to specify origins and/or destinations.
#' st_network_cost(net, from = 499, to = node_is_connected(499))
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
  # Compute the cost matrix.
  compute_costs(
    x, from, to, weights,
    direction = direction,
    Inf_as_NaN = Inf_as_NaN,
    ...
  )
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

#' @importFrom igraph distances
#' @importFrom methods hasArg
#' @importFrom units as_units deparse_unit
compute_costs = function(x, from, to, weights, direction = "out",
                         Inf_as_NaN = FALSE, ...) {
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
