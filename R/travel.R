#' Compute route optimization algorithms
#'
#' The travelling salesman problem is currently implemented
#'
#' @param pois Locations that the travelling salesman will visit. Evaluated by
#' \code{\link{evaluate_node_query}}.
#'
#' @param return_paths Should the shortest paths between `pois` be computed?
#' Defaults to `TRUE`. If `FALSE`, a vector with indices in the visiting order
#' is returned.
#'
#' @param ... Additional arguments passed on to the `TSP::solve_tsp()` function.
#'
#' @inheritParams st_network_paths
#'
#' @return An object of class \code{\link[tibble]{tbl_df}} or
#' \code{\link[sf]{sf}} with one row per path, or a vector with ordered indices
#' for `pois`.
#'
#' @importFrom stats as.dist
#' @importFrom TSP solve_TSP TSP ATSP
#' @export

st_network_travel = function(x, pois, weights = edge_length(),
                             algorithm = "tsp",
                             return_paths = TRUE,
                             use_names = TRUE,
                             return_cost = TRUE,
                             return_geometry = TRUE,
                             ...) {
  # Evaluate the node query for the pois.
  pois = evaluate_node_query(pois)
  if (any(is.na(pois))) raise_na_values("pois")
  # Evaluate the given weights specification.
  weights = evaluate_weight_spec(x, weights)
  # Compute cost matrix
  costmat = st_network_cost(x, from = pois, to = pois, weights = weights)
  # Use nearest node indices as row and column names
  row.names(costmat) = pois
  colnames(costmat) = pois
  # Convert to tsp object
  tsp_obj = switch(
    algorithm,
    "tsp" = TSP(as.dist(costmat)),
    "atsp" = ATSP(as.dist(costmat))
  )
  # Solve TSP
  tour = solve_TSP(tsp_obj, ...)
  # Return only the TSP result as node indices
  if(!return_paths) {
    as.numeric(tour)
  } else {
    tour_idxs = as.numeric(names(tour))
    # Define the nodes to calculate the shortest paths from.
    # Define the nodes to calculate the shortest paths to.
    # All based on the calculated order of visit.
    from_idxs = tour_idxs
    to_idxs = c(tour_idxs[2:length(tour_idxs)], tour_idxs[1])

    # Calculate the specified paths.
    bind_rows(
      Map(
        \(...) st_network_paths(x = net, ...,
                                weights = weights,
                                use_names = use_names,
                                return_cost = return_cost,
                                return_geometry = return_geometry),
        from = from_idxs,
        to = to_idxs
      )
    )
  }
}
