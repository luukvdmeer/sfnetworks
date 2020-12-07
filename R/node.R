#' Query nodes with spatial predicates
#'
#' These functions allow to interpretate spatial relations between nodes and
#' other geospatial features directly inside \code{\link[tidygraph]{filter}}
#' and \code{\link[tidygraph]{mutate}} calls. All functions return a logical
#' vector of the same length as the number of nodes in the network. Element i
#' in that vector is \code{TRUE} whenever \code{any(predicate(x[i], y[j]))} is
#' \code{TRUE}. Hence, in the case of using \code{node_intersects}, element i
#' in the returned vector is \code{TRUE} when node i intersects with any of
#' the features given in y.
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
#' predicate. Just as with all query functions in tidygraph, spatial node
#' measures are meant to be called inside tidygraph verbs such as
#' \code{\link[tidygraph]{mutate}} or \code{\link[tidygraph]{filter}}, where
#' the network that is currently being worked on is known and thus not needed
#' as an argument to the function. If you want to use an algorithm outside of
#' the tidygraph framework you can use \code{\link[tidygraph]{with_graph}} to
#' set the context temporarily while the algorithm is being evaluated.
#'
#' @note Note that \code{node_is_within_distance} is a wrapper around the
#' \code{st_is_within_distance} predicate from sf. Hence, it is based on
#' 'as-the-crow-flies' distance, and not on distances over the network. For
#' distances over the network, use \code{\link[tidygraph]{node_distance_to}}
#' with edge lengths as weights argument.
#'
#' @examples
#' library(sf)
#' library(tidygraph)
#'
#' net = as_sfnetwork(roxel)
#' p1 = st_point(c(7.53173, 51.95662))
#' p2 = st_point(c(7.53173, 51.95190))
#' p3 = st_point(c(7.53778, 51.95190))
#' p4 = st_point(c(7.53778, 51.95662))
#' rect = st_multipoint(c(p1, p2, p3, p4)) %>%
#'   st_cast('POLYGON') %>%
#'   st_sfc(crs = 4326)
#'
#' # Use predicate queries in a mutate call.
#' net %>%
#'   mutate(predicate = node_is_within(rect)) %>%
#'   select(predicate)
#'
#' # Use predicate queries in a filter call.
#' par(mar = c(1,1,1,1))
#' plot(net)
#' plot(
#'   net %>% filter(node_is_within(rect)),
#'   col = 'red',
#'   pch = 20,
#'   add = TRUE
#' )
#' plot(
#'   net %>% filter(node_is_disjoint(rect)),
#'   col = 'blue',
#'   pch = 20,
#'   add = TRUE
#' )
#' @name spatial_node_predicates
NULL

#' @name spatial_node_predicates
#' @importFrom sf st_intersects
#' @export
node_intersects = function(y, ...) {
  x = .G()
  require_active_nodes(x)
  lengths(st_intersects(x, y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_disjoint
#' @export
node_is_disjoint = function(y, ...) {
  x = .G()
  require_active_nodes(x)
  lengths(st_disjoint(x, y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_touches
#' @export
node_touches = function(y, ...) {
  x = .G()
  require_active_nodes(x)
  lengths(st_touches(x, y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_within
#' @export
node_is_within = function(y, ...) {
  x = .G()
  require_active_nodes(x)
  lengths(st_within(x, y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_equals
#' @export
node_equals = function(y, ...) {
  x = .G()
  require_active_nodes(x)
  lengths(st_equals(x, y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_covered_by
#' @export
node_is_covered_by = function(y, ...) {
  x = .G()
  require_active_nodes(x)
  lengths(st_covered_by(x, y, ...)) > 0
}

#' @name spatial_node_predicates
#' @importFrom sf st_is_within_distance
#' @export
node_is_within_distance = function(y, ...) {
  x = .G()
  require_active_nodes(x)
  lengths(st_is_within_distance(x, y, ...)) > 0
}
