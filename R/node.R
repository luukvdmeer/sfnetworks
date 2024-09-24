#' Query spatial node types
#'
#' These functions are a collection of node type queries that are commonly
#' used in spatial network analysis, and form a spatial extension to
#' \code{\link[tidygraph:node_types]{node type queries}} in tidygraph.
#'
#' @return A logical vector of the same length as the number of nodes in the
#' network, indicating if each node is of the type in question.
#'
#' @details Just as with all query functions in tidygraph, these functions
#' are meant to be called inside tidygraph verbs such as
#' \code{\link[tidygraph]{mutate}} or \code{\link[tidygraph]{filter}}, where
#' the network that is currently being worked on is known and thus not needed
#' as an argument to the function. If you want to use an algorithm outside of
#' the tidygraph framework you can use \code{\link[tidygraph]{with_graph}} to
#' set the context temporarily while the algorithm is being evaluated.
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' # Create a network.
#' net = as_sfnetwork(mozart, "mst", directed = FALSE)
#'
#' # Use query function in a filter call.
#' pseudos = net |>
#'   activate(nodes) |>
#'   filter(node_is_pseudo())
#'
#' danglers = net |>
#'   activate(nodes) |>
#'   filter(node_is_dangling())
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#' plot(net, main = "Pseudo nodes")
#' plot(st_geometry(pseudos), pch = 20, cex = 1.2, col = "orange", add = TRUE)
#' plot(net, main = "Dangling nodes")
#' plot(st_geometry(danglers), pch = 20, cex = 1.2, col = "orange", add = TRUE)
#' par(oldpar)
#'
#' # Use query function in a mutate call.
#' net |>
#'   activate(nodes) |>
#'   mutate(pseudo = node_is_pseudo(), dangling = node_is_dangling())
#'
#' # Use query function directly.
#' danglers = with_graph(net, node_is_dangling())
#' head(danglers)
#'
#' @name spatial_node_types
NULL

#' @describeIn spatial_node_types Pseudo nodes in directed networks are those
#' nodes with only one incoming and one outgoing edge. In undirected networks
#' pseudo nodes are those nodes with only two incident edges, i.e. nodes of
#' degree 2.
#' @importFrom tidygraph .G
#' @export
node_is_pseudo = function() {
  require_active_nodes()
  x = .G()
  is_pseudo = is_pseudo_node(x)
  if (is_focused(x)) is_pseudo[node_ids(x, focused = TRUE)] else is_pseudo
}

#' @importFrom igraph degree is_directed
is_pseudo_node = function(x) {
  if (is_directed(x)) {
    pseudo = degree(x, mode = "in") == 1 & degree(x, mode = "out") == 1
  } else {
    pseudo = degree(x) == 2
  }
}

#' @describeIn spatial_node_types Dangling nodes are nodes with only one
#' incident edge, i.e. nodes of degree 1.
#' @importFrom tidygraph .G
#' @export
node_is_dangling = function() {
  require_active_nodes()
  x = .G()
  is_dangling = is_dangling_node(x)
  if (is_focused(x)) is_dangling[node_ids(x, focused = TRUE)] else is_dangling
}

#' @importFrom igraph degree
is_dangling_node = function(x) {
  degree(x) == 1
}

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
