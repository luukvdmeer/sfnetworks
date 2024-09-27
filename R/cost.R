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
#' Evaluated by \code{\link{evaluate_weight_spec}}. The default is
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
#' @param router The routing backend to use for the cost matrix computation.
#' Currently supported options are \code{'igraph'} and \code{'dodgr'}. See
#' Details.
#'
#' @param ... Additional arguments passed on to the underlying function of the
#' chosen routing backend. See Details.
#'
#' @details The sfnetworks package does not implement its own routing algorithms
#' to compute cost matrices. Instead, it relies on "routing backends", i.e.
#' other R packages that have implemented such algorithms. Currently two
#' different routing backends are supported.
#'
#' The default is \code{\link[igraph]{igraph}}. This package supports
#' many-to-many cost matrix computation with the \code{\link[igraph]{distances}}
#' function. The igraph router does not support dual-weighted routing.
#'
#' The second supported routing backend is \code{\link[dodgr]{dodgr}}. This
#' package supports many-to-many cost matrix computation with the
#' \code{\link[dodgr]{dodgr_dists}} function. It also supports dual-weighted
#' routing. The dodgr package is a conditional dependency of sfnetworks. Using
#' the dodgr router requires the dodgr package to be installed.
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
                           Inf_as_NaN = FALSE, router = "igraph", ...) {
  UseMethod("st_network_cost")
}

#' @importFrom rlang enquo
#' @export
st_network_cost.sfnetwork = function(x, from = node_ids(x), to = node_ids(x),
                                     weights = edge_length(),
                                     direction = "out",
                                     Inf_as_NaN = FALSE,
                                     router = "igraph", ...) {
  # Evaluate the given from node query.
  from = evaluate_node_query(x, enquo(from))
  if (any(is.na(from))) raise_na_values("from")
  # Evaluate the given to node query.
  to = evaluate_node_query(x, enquo(to))
  if (any(is.na(to))) raise_na_values("to")
  # Evaluate the given weights specification.
  weights = evaluate_weight_spec(x, enquo(weights))
  # Compute the cost matrix.
  compute_costs(
    x, from, to, weights,
    direction = direction,
    Inf_as_NaN = Inf_as_NaN,
    router = router,
    ...
  )
}

#' @name st_network_cost
#' @export
st_network_distance = function(x, from = node_ids(x), to = node_ids(x),
                               direction = "out", Inf_as_NaN = FALSE,
                               router = "igraph", ...) {
  st_network_cost(
    x, from, to,
    weights = edge_length(),
    direction = direction,
    Inf_as_NaN = Inf_as_NaN,
    router = router,
    ...
  )
}

#' @importFrom units as_units deparse_unit
compute_costs = function(x, from, to, weights, direction = "out",
                         Inf_as_NaN = FALSE, router = "igraph", ...) {
  # Compute cost matrix with the given router.
  costs = switch(
    router,
    igraph = igraph_costs(x, from, to, weights, direction, ...),
    dodgr = dodgr_costs(x, from, to, weights, direction, ...),
    raise_unknown_input("router", router, c("igraph", "dodgr"))
  )
  # Post-process and return.
  # --> Convert Inf to NaN if requested.
  # --> Attach units if the provided weights had units.
  if (Inf_as_NaN) costs[is.infinite(costs)] = NaN
  if (inherits(weights, "units")) {
    as_units(costs, deparse_unit(weights))
  } else {
    costs
  }
}

#' @importFrom igraph distances
#' @importFrom methods hasArg
igraph_costs = function(x, from, to, weights, direction = "out", ...) {
  # The direction argument is used instead of igraphs mode argument.
  # This means the mode argument should not be set.
  if (hasArg("mode")) raise_unsupported_arg("mode", replacement = "direction")
  # Call igraph::distances function to compute the cost matrix.
  # Special attention is required if there are duplicated 'to' nodes:
  # --> In igraph this cannot be handled.
  # --> Therefore we call igraph::distances with unique 'to' nodes.
  # --> Afterwards we copy cost values to duplicated 'to' nodes.
  if(any(duplicated(to))) {
    tou = unique(to)
    mat = distances(x, from, tou, weights = weights, mode = direction, ...)
    mat = mat[, match(to, tou), drop = FALSE]
  } else {
    mat = distances(x, from, to, weights = weights, mode = direction, ...)
  }
  mat
}

#' @importFrom rlang check_installed
dodgr_costs = function(x, from, to, weights, direction = "out", ...) {
  check_installed("dodgr") # Package dodgr is required for this function.
  # Convert the network to dodgr format.
  x_dodgr = sfnetwork_to_minimal_dodgr(x, weights, direction)
  # Call dodgr::dodgr_dists to compute the cost matrix.
  mat = dodgr::dodgr_dists(x_dodgr, as.character(from), as.character(to), ...)
  mat[is.na(mat)] = Inf
  mat
}
