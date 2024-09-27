#' Compute isolines around nodes in a spatial network
#'
#' Isolines are curves along which a function has a constant value. In spatial
#' networks, they are used to delineate areas that are reachable from a given
#' node within a given travel cost. If the travel cost is distance, they are
#' known as isodistances, while if the travel cost is time, they are known as
#' isochrones. This function finds all network nodes that lie inside an isoline
#' around a specified node.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param node The node around which the isolines will be drawn. Evaluated by
#' \code{\link{evaluate_node_query}}. When multiple nodes are given, only the
#' first one is used.
#'
#' @param cost The constant cost value of the isoline. Should be a numeric
#' value in the same units as the given edge weights. Alternatively, units can
#' be specified explicitly by providing a \code{\link[units]{units}} object.
#' Multiple values may be given, which will result in multiple isolines being
#' drawn.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Evaluated by \code{\link{evaluate_weight_spec}}. The default is
#' \code{\link{edge_length}}, which computes the geographic lengths of the
#' edges.
#'
#' @param ... Additional arguments passed on to \code{\link{st_network_cost}}
#' to compute the cost matrix from the specified node to all other nodes in the
#' network.
#'
#' @param delineate Should the nodes inside the isoline be delineated? If
#' \code{FALSE}, the nodes inside the isoline are returned as a
#' \code{MULTIPOINT} geometry. If \code{TRUE}, the concave hull of that
#' geometry is returned instead. Defaults to \code{TRUE}.
#'
#' @param ratio The ratio of the concave hull. Defaults to \code{1}, meaning
#' that the convex hull is computed. See \code{\link[sf]{st_concave_hull}} for
#' details. Ignored if \code{delineate = FALSE}.
#'
#' @param allow_holes May the concave hull have holes? Defaults to \code{FALSE}.
#' Ignored if \code{delineate = FALSE}.
#'
#' @returns An object of class \code{\link[sf]{sf}} with one row per requested
#' isoline. The object contains the following columns:
#'
#' \itemize{
#'   \item \code{cost}: The constant cost value of the isoline.
#'   \item \code{geometry}: If \code{delineate = TRUE}, the concave hull of all
#'   nodes that lie inside the isoline. Otherwise, those nodes combined into a
#'   single \code{MULTIPOINT} geometry.
#' }
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#'
#' center = st_centroid(st_combine(st_geometry(roxel)))
#'
#' net = as_sfnetwork(roxel, directed = FALSE)
#'
#' iso = net |>
#'   st_network_iso(node_is_nearest(center), c(1000, 500, 250), ratio = 0.3)
#'
#' colors = c("#fee6ce90", "#fdae6b90", "#e6550d90")
#'
#' plot(net)
#' plot(st_geometry(iso), col = colors, add = TRUE)
#'
#' par(oldpar)
#'
#' @export
st_network_iso = function(x, node, cost, weights = edge_length(), ...,
                          delineate = TRUE, ratio = 1, allow_holes = FALSE) {
  UseMethod("st_network_iso")
}

#' @importFrom methods hasArg
#' @importFrom rlang enquo
#' @importFrom sf st_combine st_concave_hull st_sf
#' @importFrom units as_units deparse_unit
#' @export
st_network_iso.sfnetwork = function(x, node, cost, weights = edge_length(),
                                    ..., delineate = TRUE, ratio = 1,
                                    allow_holes = FALSE) {
  # Evaluate the given node query.
  # Always only the first node is used.
  node = evaluate_node_query(x, enquo(node))
  if (length(node) > 1) raise_multiple_elements("node"); node = node[1]
  # Evaluate the given weights specification.
  weights = evaluate_weight_spec(x, enquo(weights))
  # If the "to" nodes are also given this query has to be evaluated as well.
  # Otherwise it defaults to all nodes in the network.
  if (hasArg("to")) {
    to = evaluate_node_query(x, enquo(to))
  } else {
    to = node_ids(x, focused = FALSE)
  }
  # Compute the cost matrix from the specified node to all other nodes.
  matrix = compute_costs(x, node, to, weights = weights, ...)
  # Parse the given cost values.
  if (inherits(matrix, "units") && ! inherits(cost, "units")) {
    cost = as_units(cost, deparse_unit(matrix))
  }
  # For each given cost:
  # --> Define which nodes are inside the isoline.
  # --> Extract and combine the geometries of those nodes.
  node_geom = pull_node_geom(x)
  get_single_iso = function(k) {
    in_iso = matrix[1, ] <= k
    iso = st_combine(node_geom[in_iso])
    if (delineate) {
      iso = st_concave_hull(iso, ratio = ratio, allow_holes = allow_holes)
    }
    iso
  }
  geoms = do.call("c", lapply(cost, get_single_iso))
  st_sf(cost = cost, geometry = geoms)
}