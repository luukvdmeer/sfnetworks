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
#' filtered = net %>%
#'   activate("nodes") %>%
#'   filter(node_X() > 7.54)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#' plot(net, col = "grey")
#' plot(filtered, col = "red", add = TRUE)
#' par(oldpar)
#'
#' # Use query function in a mutate call.
#' net %>%
#'   activate("nodes") %>%
#'   mutate(X = node_X(), Y = node_Y())
#'
#' @name node_coordinates
NULL

#' @name node_coordinates
#' @export
node_X = function() {
  require_active_nodes()
  x = .G()
  get_coords(pull_node_geom(x), "X")
}

#' @name node_coordinates
#' @export
node_Y = function() {
  require_active_nodes()
  x = .G()
  get_coords(pull_node_geom(x), "Y")
}

#' @name node_coordinates
#' @export
node_Z = function() {
  require_active_nodes()
  x = .G()
  get_coords(pull_node_geom(x), "Z")
}

#' @name node_coordinates
#' @export
node_M = function() {
  require_active_nodes()
  x = .G()
  get_coords(pull_node_geom(x), "M")
}

#' @importFrom sf st_coordinates
get_coords = function(x, value) {
  all_coords = st_coordinates(x)
  tryCatch(
    all_coords[, value],
    error = function(e) {
      warning(value, " coordinates are not available", call. = FALSE)
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
#' function of sf. See \code{\link[sf]{geos_binary_pred}}.
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
#' net = as_sfnetwork(roxel) %>%
#'   st_transform(3035)
#'
#' # Create a geometry to test against.
#' p1 = st_point(c(4151358, 3208045))
#' p2 = st_point(c(4151340, 3207520))
#' p3 = st_point(c(4151756, 3207506))
#' p4 = st_point(c(4151774, 3208031))
#'
#' poly = st_multipoint(c(p1, p2, p3, p4)) %>%
#'   st_cast('POLYGON') %>%
#'   st_sfc(crs = 3035)
#'
#' # Use predicate query function in a filter call.
#' within = net %>%
#'   activate("nodes") %>%
#'   filter(node_is_within(poly))
#'
#' disjoint = net %>%
#'   activate("nodes") %>%
#'   filter(node_is_disjoint(poly))
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#' plot(net)
#' plot(within, col = "red", add = TRUE)
#' plot(disjoint, col = "blue", add = TRUE)
#' par(oldpar)
#'
#' # Use predicate query function in a mutate call.
#' net %>%
#'   activate("nodes") %>%
#'   mutate(within = node_is_within(poly)) %>%
#'   select(within)
#'
#' @name spatial_node_predicates
NULL

#' @name spatial_node_predicates
#' @importFrom sf st_intersects
#' @export
node_intersects = function(y, ...) {
  require_active_nodes()
  x = .G()
  lengths(st_intersects(pull_node_geom(x), y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_disjoint
#' @export
node_is_disjoint = function(y, ...) {
  require_active_nodes()
  x = .G()
  lengths(st_disjoint(pull_node_geom(x), y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_touches
#' @export
node_touches = function(y, ...) {
  require_active_nodes()
  x = .G()
  lengths(st_touches(pull_node_geom(x), y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_within
#' @export
node_is_within = function(y, ...) {
  require_active_nodes()
  x = .G()
  lengths(st_within(pull_node_geom(x), y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_equals
#' @export
node_equals = function(y, ...) {
  require_active_nodes()
  x = .G()
  lengths(st_equals(pull_node_geom(x), y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_covered_by
#' @export
node_is_covered_by = function(y, ...) {
  require_active_nodes()
  x = .G()
  lengths(st_covered_by(pull_node_geom(x), y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_is_within_distance
#' @export
node_is_within_distance = function(y, ...) {
  require_active_nodes()
  x = .G()
  lengths(st_is_within_distance(pull_node_geom(x), y, ...)) > 0
}

#' @name spatial_node_predicates
#' @export
node_is_nearest = function(y) {
  require_active_nodes()
  x = .G()
  vec = rep(FALSE, n_nodes(x))
  vec[nearest_node_ids(x, y)] = TRUE
  vec
}
