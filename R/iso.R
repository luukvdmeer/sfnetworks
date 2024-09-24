#' @importFrom sf st_combine st_concave_hull st_sf
#' @importFrom units as_units deparse_unit
#' @export
st_network_iso = function(x, node, threshold, weights = edge_length(), ...,
                          delineate = TRUE, ratio = 1, allow_holes = FALSE) {
  # Compute the cost matrix from the specified node to all other nodes.
  costs = st_network_cost(x, from = node, ...)
  # Parse the given threshold values.
  if (inherits(costs, "units") && ! inherits(threshold, "units")) {
    threshold = as_units(threshold, deparse_unit(costs))
  }
  # For each given threshold:
  # --> Define which nodes are inside the isoline.
  # --> Extract and combine the geometries of those nodes.
  node_geom = pull_node_geom(x)
  get_single_iso = function(k) {
    in_iso = costs[1, ] <= k
    iso = st_combine(node_geom[in_iso])
    if (delineate) {
      iso = st_concave_hull(iso, ratio = ratio, allow_holes = allow_holes)
    }
    iso
  }
  geoms = do.call("c", lapply(threshold, get_single_iso))
  st_sf(threshold = threshold, geometry = geoms)
}