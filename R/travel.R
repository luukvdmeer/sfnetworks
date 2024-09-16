#' Compute route optimization algorithms
#'
#' The travelling salesman problem is currently implemented
#'
#' @param pois Locations that the travelling salesman will visit.
#' Can be an integer vector specifying nodes indices or a character vector
#' specifying node names. Can also be an object of class \code{\link[sf]{sf}}
#' or \code{\link[sf]{sfc}} containing spatial features.
#' In that case, these feature will be snapped to their nearest node before
#' solving the algorithm.
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
  # Parse pois argument.
  # --> Convert geometries to node indices.
  ### check # --> Raise warnings when requirements are not met.
  if (is_sf(pois) | is_sfc(pois)) pois = nearest_node_ids(x, pois)
  # if (any(is.na(pois))) raise_na_values("pois")
  # Parse weights argument using tidy evaluation on the network edges.
  .register_graph_context(x, free = TRUE)
  weights = enquo(weights)
  weights = eval_tidy(weights, .E())
  if (is_single_string(weights)) {
    # Allow character values for backward compatibility.
    deprecate_weights_is_string("st_network_travel")
    weights = eval_tidy(expr(.data[[weights]]), .E())
  }
  if (is.null(weights)) {
    # Convert NULL to NA to align with tidygraph instead of igraph.
    deprecate_weights_is_null("st_network_travel")
    weights = NA
  }
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
