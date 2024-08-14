#' Query node coordinates
#'
#' These functions allow to query specific coordinate values from the
#' geometries of the nodes.
#'
#' @return A numeric vector of the same length as the number of nodes in the
#' network.
#'
#' @details Just as with all query functions in tidygraph, these functions
#' are meant to be called inside tidygraph verbs such as
#' \code{\link[tidygraph]{mutate}} or \code{\link[tidygraph]{filter}}, where
#' the network that is currently being worked on is known and thus not needed
#' as an argument to the function. If you want to use an algorithm outside of
#' the tidygraph framework you can use \code{\link[tidygraph]{with_graph}} to
#' set the context temporarily while the algorithm is being evaluated.
#'
#' @note If a requested coordinate value is not available for a node, \code{NA}
#' will be returned.
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' # Create a network.
#' net = as_sfnetwork(roxel)
#'
#' # Use query function in a filter call.
#' filtered = net |>
#'   activate(nodes) |>
#'   filter(node_X() > 7.54)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#' plot(net, col = "grey")
#' plot(filtered, col = "red", add = TRUE)
#' par(oldpar)
#'
#' # Use query function in a mutate call.
#' net |>
#'   activate(nodes) |>
#'   mutate(X = node_X(), Y = node_Y())
#'
#' # Use query function directly.
#' X = with_graph(net, node_X())
#' head(X)
#'
#' @name node_coordinates
NULL

#' @name node_coordinates
#' @importFrom tidygraph .G
#' @export
node_X = function() {
  require_active_nodes()
  extract_node_coords(.G(), "X")
}

#' @name node_coordinates
#' @importFrom tidygraph .G
#' @export
node_Y = function() {
  require_active_nodes()
  extract_node_coords(.G(), "Y")
}

#' @name node_coordinates
#' @importFrom tidygraph .G
#' @export
node_Z = function() {
  require_active_nodes()
  extract_node_coords(.G(), "Z")
}

#' @name node_coordinates
#' @importFrom tidygraph .G
#' @export
node_M = function() {
  require_active_nodes()
  extract_node_coords(.G(), "M")
}

#' @importFrom cli cli_warn
#' @importFrom sf st_coordinates
extract_node_coords = function(x, value) {
  all_coords = st_coordinates(pull_node_geom(x, focused = TRUE))
  tryCatch(
    all_coords[, value],
    error = function(e) {
      cli_warn("{value} coordinates are not available.")
      rep(NA, length(x))
    }
  )
}

#' Query nodes with spatial predicates
#'
#' These functions allow to interpret spatial relations between nodes and
#' other geospatial features directly inside \code{\link[tidygraph]{filter}}
#' and \code{\link[tidygraph]{mutate}} calls. All functions return a logical
#' vector of the same length as the number of nodes in the network. Element i
#' in that vector is \code{TRUE} whenever the chosen spatial predicate applies
#' to the spatial relation between the i-th node and any of the features in
#' \code{y}.
#'
#' @param y The geospatial features to test the nodes against, either as an
#' object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @param ... Arguments passed on to the corresponding spatial predicate
#' function of sf. See \code{\link[sf]{geos_binary_pred}}. The argument
#' \code{sparse} should not be set.
#'
#' @return A logical vector of the same length as the number of nodes in the
#' network.
#'
#' @details See \code{\link[sf]{geos_binary_pred}} for details on each spatial
#' predicate. The function \code{node_is_nearest} instead wraps around
#' \code{\link[sf]{st_nearest_feature}} and returns \code{TRUE} for element i
#' if the i-th node is the nearest node to any of the features in \code{y}.
#'
#' Just as with all query functions in tidygraph, these functions are meant to
#' be called inside tidygraph verbs such as \code{\link[tidygraph]{mutate}} or
#' \code{\link[tidygraph]{filter}}, where the network that is currently being
#' worked on is known and thus not needed as an argument to the function. If
#' you want to use an algorithm outside of the tidygraph framework you can use
#' \code{\link[tidygraph]{with_graph}} to set the context temporarily while the
#' algorithm is being evaluated.
#'
#' @note Note that \code{node_is_within_distance} is a wrapper around the
#' \code{st_is_within_distance} predicate from sf. Hence, it is based on
#' 'as-the-crow-flies' distance, and not on distances over the network. For
#' distances over the network, use \code{\link[tidygraph]{node_distance_to}}
#' with edge lengths as weights argument.
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' # Create a network.
#' net = as_sfnetwork(roxel) |>
#'   st_transform(3035)
#'
#' # Create a geometry to test against.
#' p1 = st_point(c(4151358, 3208045))
#' p2 = st_point(c(4151340, 3207520))
#' p3 = st_point(c(4151756, 3207506))
#' p4 = st_point(c(4151774, 3208031))
#'
#' poly = st_multipoint(c(p1, p2, p3, p4)) |>
#'   st_cast('POLYGON') |>
#'   st_sfc(crs = 3035)
#'
#' # Use predicate query function in a filter call.
#' within = net |>
#'   activate(nodes) |>
#'   filter(node_is_within(poly))
#'
#' disjoint = net |>
#'   activate(nodes) |>
#'   filter(node_is_disjoint(poly))
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#' plot(net)
#' plot(within, col = "red", add = TRUE)
#' plot(disjoint, col = "blue", add = TRUE)
#' par(oldpar)
#'
#' # Use predicate query function in a mutate call.
#' net |>
#'   activate(nodes) |>
#'   mutate(within = node_is_within(poly)) |>
#'   select(within)
#'
#' # Use predicate query function directly.
#' within = with_graph(net, node_within(poly))
#' head(within)
#'
#' @name spatial_node_predicates
NULL

#' @name spatial_node_predicates
#' @importFrom sf st_intersects
#' @importFrom tidygraph .G
#' @export
node_intersects = function(y, ...) {
  require_active_nodes()
  evaluate_node_predicate(st_intersects, .G(), y, ...)
}

#' @name spatial_node_predicates
#' @importFrom sf st_disjoint
#' @importFrom tidygraph .G
#' @export
node_is_disjoint = function(y, ...) {
  require_active_nodes()
  evaluate_node_predicate(st_disjoint, .G(), y, ...)
}

#' @name spatial_node_predicates
#' @importFrom sf st_touches
#' @importFrom tidygraph .G
#' @export
node_touches = function(y, ...) {
  require_active_nodes()
  evaluate_node_predicate(st_touches, .G(), y, ...)
}

#' @name spatial_node_predicates
#' @importFrom sf st_within
#' @importFrom tidygraph .G
#' @export
node_is_within = function(y, ...) {
  require_active_nodes()
  evaluate_node_predicate(st_within, .G(), y, ...)
}

#' @name spatial_node_predicates
#' @importFrom sf st_equals
#' @importFrom tidygraph .G
#' @export
node_equals = function(y, ...) {
  require_active_nodes()
  evaluate_node_predicate(st_equals, .G(), y, ...)
}

#' @name spatial_node_predicates
#' @importFrom sf st_covered_by
#' @importFrom tidygraph .G
#' @export
node_is_covered_by = function(y, ...) {
  require_active_nodes()
  evaluate_node_predicate(st_covered_by, .G(), y, ...)
}

#' @name spatial_node_predicates
#' @importFrom sf st_is_within_distance
#' @importFrom tidygraph .G
#' @export
node_is_within_distance = function(y, ...) {
  require_active_nodes()
  evaluate_node_predicate(st_is_within_distance, .G(), y, ...)
}

#' @name spatial_node_predicates
#' @export
node_is_nearest = function(y) {
  require_active_nodes()
  x = .G()
  vec = rep(FALSE, n_nodes(x))
  vec[nearest_node_ids(x, y, focused = FALSE)] = TRUE
  vec[node_ids(x, focused = TRUE)]
}

evaluate_node_predicate = function(predicate, x, y, ...) {
  N = pull_node_geom(x, focused = TRUE)
  lengths(predicate(N, y, sparse = TRUE, ...)) > 0
}

#' Correct node geometries to match edge boundary locations
#'
#' This function makes invalid edge geometries valid by adding their boundary
#' point as a new node to the network whenever it does not equal the location
#' of their boundary node as specified by the *from* or *to* indices. It
#' subsequently updates the *from* and *to* columns of the edges to correspond
#' to the new nodes.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link{sfnetwork}} with corrected node
#' geometries.
#'
#' @note This function works only if the edge geometries are meant to start at
#' their specified *from* node and end at their specified *to* node. In
#' undirected networks this is not necessarily the case, since edge geometries
#' are allowed to start at their specified *to* node and end at their specified
#' *from* node. Therefore, in undirected networks those edges first have to be
#' reversed before running this function.
#'
#' @importFrom dplyr bind_rows
#' @importFrom igraph is_directed
#' @importFrom sf st_geometry st_sf
#' @noRd
correct_node_geometries = function(x) {
  # Extract node and edge data.
  nodes = nodes_as_sf(x)
  edges = edges_as_sf(x)
  # Check which edge boundary points do not match their specified nodes.
  boundary_points = linestring_boundary_points(edges)
  boundary_node_ids = edge_boundary_node_ids(x)
  boundary_nodes = st_geometry(nodes)[boundary_node_ids]
  no_match = !have_equal_geometries(boundary_points, boundary_nodes)
  # For boundary points that do not match their node:
  # Boundary points that don't match their node become new nodes themselves.
  new_nodes = list()
  new_nodes[node_geom_colname(x)] = list(boundary_points[which(no_match)])
  new_nodes = st_sf(new_nodes)
  all_nodes = bind_rows(nodes, new_nodes)
  # Update the from and to columns of the edges accordingly.
  n_nodes = nrow(nodes)
  n_new_nodes = nrow(new_nodes)
  boundary_node_ids[no_match] = c((n_nodes + 1):(n_nodes + n_new_nodes))
  n_boundaries = length(boundary_node_ids)
  edges$from = boundary_node_ids[seq(1, n_boundaries - 1, 2)]
  edges$to = boundary_node_ids[seq(2, n_boundaries, 2)]
  # Return a new network with the added nodes and updated edges.
  sfnetwork_(all_nodes, edges, is_directed(x)) %preserve_network_attrs% x
}