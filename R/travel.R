#' Find the optimal route through a set of nodes in a spatial network
#'
#' Solve the travelling salesman problem by finding the shortest route through
#' a set of nodes that visits each of those nodes once.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param nodes Nodes to be visited. Evaluated by
#' \code{\link{evaluate_node_query}}.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Evaluated by \code{\link{evaluate_weight_spec}}. The default is
#' \code{\link{edge_length}}, which computes the geographic lengths of the
#' edges.
#'
#' @param optimizer The optimization backend to use for defining the optimal
#' visiting order of the given nodes. Currently the only supported option is
#' \code{'TSP'}. See Details.
#'
#' @param router The routing backend to use for the cost matrix computation and
#' the path computation. Currently supported options are \code{'igraph'} and
#' \code{'dodgr'}. See Details.
#'
#' @param return_paths After defining the optimal visiting order of nodes,
#' should the actual paths connecting those nodes be computed and returned?
#' Defaults to \code{TRUE}. If set to \code{FALSE}, a vector of indices in
#' visiting order is returned instead, with each index specifying the position
#' of the visited node in the \code{from} argument.
#'
#' @param use_names If a column named \code{name} is present in the nodes
#' table, should these names be used to encode the nodes in the route, instead
#' of the node indices? Defaults to \code{FALSE}. Ignored when the nodes table
#' does not have a column named \code{name} and if \code{return_paths = FALSE}.
#'
#' @param return_cost Should the total cost of each path between two subsequent
#' nodes be computed? Defaults to \code{TRUE}.
#' Ignored if \code{return_paths = FALSE}.
#'
#' @param return_geometry Should a linestring geometry be constructed for each
#' path between two subsequent nodes? Defaults to \code{TRUE}. The geometries
#' are constructed by calling \code{\link[sf]{st_line_merge}} on the linestring
#' geometries of the edges in the path. Ignored if \code{return_paths = FALSE}
#' and for networks with spatially implicit edges.
#'
#' @param ... Additional arguments passed on to the underlying function of the
#' chosen optimization backend. See Details.
#'
#' @details The sfnetworks package does not implement its own route optimization
#' algorithms. Instead, it relies on "optimization backends", i.e. other R
#' packages that have implemented such algorithms. Currently the only supported
#' optimization backend to solve the travelling salesman problem is the
#' \code{\link[TSP:TSP-package]{TSP}} package, which provides the
#' \code{\link[TSP]{solve_TSP}} function for this task.
#'
#' An input for most route optimization algorithms is the matrix containing the
#' travel costs between the nodes to be visited. This is computed using
#' \code{\link{st_network_cost}}. The output of most route optimization
#' algorithms is the optimal order in which the given nodes should be visited.
#' To compute the actual paths that connect the nodes in that order, the
#' \code{\link{st_network_paths}} function is used. Both cost matrix computation
#' and shortest paths computation allow to specify a "routing backend", i.e. an
#' R package that implements algorithms to solve those tasks. See the
#' documentation of the corresponding functions for details.
#'
#' @seealso \code{\link{st_network_paths}}, \code{\link{st_network_cost}}
#'
#' @return An object of class \code{\link[sf]{sf}} with one row per leg of the
#' optimal route, containing the path of that leg.
#' If \code{return_geometry = FALSE} or edges are spatially implicit, a
#' \code{\link[tibble]{tbl_df}} is returned instead. See the documentation of
#' \code{\link{st_network_paths}} for details. If \code{return_paths = FALSE},
#' a vector of indices in visiting order is returned, with each index
#' specifying the position of the visited node in the \code{from} argument.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#'
#' net = as_sfnetwork(roxel, directed = FALSE) |>
#'   st_transform(3035)
#'
#' # Compute the optimal route through three nodes.
#' # Note that geographic edge length is used as edge weights by default.
#' route = st_network_travel(net, c(1, 10, 100))
#' route
#'
#' plot(net, col = "grey")
#' plot(st_geometry(net)[route$from], pch = 20, cex = 2, add = TRUE)
#' plot(st_geometry(route), col = "orange", lwd = 3, add = TRUE)
#'
#' # Instead of returning a path we can return a vector of visiting order.
#' st_network_travel(net, c(1, 10, 100), return_paths = FALSE)
#'
#' # Use spatial point features to specify the visiting locations.
#' # These are snapped to their nearest node before finding the path.
#' p1 = st_geometry(net, "nodes")[1] + st_sfc(st_point(c(50, -50)))
#' p2 = st_geometry(net, "nodes")[10] + st_sfc(st_point(c(-10, 100)))
#' p3 = st_geometry(net, "nodes")[100] + st_sfc(st_point(c(-10, 100)))
#' pts = c(p1, p2, p3)
#' st_crs(pts) = st_crs(net)
#'
#' route = st_network_travel(net, pts)
#' route
#'
#' plot(net, col = "grey")
#' plot(pts, pch = 20, cex = 2, add = TRUE)
#' plot(st_geometry(net)[route$from], pch = 4, cex = 2, add = TRUE)
#' plot(st_geometry(route), col = "orange", lwd = 3, add = TRUE)
#'
#' par(oldpar)
#'
#' @export
st_network_travel = function(x, nodes, weights = edge_length(),
                             optimizer = "TSP",
                             router = getOption("sfn_default_router", "igraph"),
                             return_paths = TRUE, use_names = FALSE,
                             return_cost = TRUE, return_geometry = TRUE, ...) {
  UseMethod("st_network_travel")
}

#' @importFrom rlang enquo
#' @export
st_network_travel.sfnetwork = function(x, nodes, weights = edge_length(),
                                       optimizer = "TSP",
                                       router = getOption("sfn_default_router", "igraph"),
                                       return_paths = TRUE, use_names = FALSE,
                                       return_cost = TRUE,
                                       return_geometry = TRUE, ...) {
  # Evaluate the node query for the given nodes.
  nodes = evaluate_node_query(x, enquo(nodes))
  if (any(is.na(nodes))) raise_na_values("nodes")
  # Evaluate the given weights specification.
  weights = evaluate_weight_spec(x, enquo(weights))
  # Compute the optimal route.
  find_optimal_route(
    x, nodes, weights,
    optimizer = optimizer,
    router = router,
    use_names = use_names,
    return_paths = return_paths,
    return_cost = return_cost,
    return_geometry = return_geometry,
    ...
  )
}

#' @importFrom dplyr bind_rows
find_optimal_route = function(x, nodes, weights = edge_length(),
                              optimizer = "TSP",
                              router = getOption("sfn_default_router", "igraph"),
                              return_paths = TRUE, use_names = FALSE,
                              return_cost = TRUE, return_geometry = TRUE, ...) {
  # Compute cost matrix with the given router.
  costmat = compute_costs(x, nodes, nodes, weights = weights, router = router)
  # Use numeric row and column names.
  rownames(costmat) = nodes
  colnames(costmat) = nodes
  # Find the optimal visiting order with the given optimizer.
  route = switch(
    optimizer,
    TSP = tsp_route(costmat),
    raise_unknown_input("optimizer", optimizer, c("TSP"))
  )
  if (! return_paths) return(match(route, nodes))
  # Each leg of the route will require a shortest path computation.
  # Define the from and to nodes for each path computation.
  from = route
  to = c(route[-1], route[1])
  # Calculate the shortest path for each route leg.
  find_leg = function(...) {
    find_paths(
      x = x,
      ...,
      weights = weights,
      use_names = use_names,
      return_cost = return_cost,
      return_geometry = return_geometry
    )
  }
  bind_rows(Map(find_leg, from = from, to = to))
}

#' @importFrom rlang check_installed
#' @importFrom stats as.dist
#' @importFrom units drop_units
tsp_route = function(x, ...) {
  check_installed("TSP") # Package TSP is required for this function.
  # Drop units if present.
  if (inherits(x, "units")) x = drop_units(x)
  # Create the object that formulates the travelling salesman problem.
  # If the cost matrix is symmetric this should be a TSP object.
  # Otherwise it should be a ATSP object.
  if (isSymmetric(x)) {
    tsp_obj = TSP::TSP(as.dist(x))
  } else {
    tsp_obj = TSP::ATSP(as.dist(x))
  }
  # Solve the problem.
  tour = TSP::solve_TSP(tsp_obj, ...)
  # Return the node indices in order of visit.
  as.numeric(names(tour))
}
