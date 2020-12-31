#' Query spatial edge measures
#'
#' These functions are a collection of specific spatial edge measures, that
#' form a spatial extension to edge measures in
#' \code{\link[tidygraph:tidygraph-package]{tidygraph}}.
#'
#' @details Just as with all query functions in tidygraph, spatial edge
#' measures are meant to be called inside tidygraph verbs such as
#' \code{\link[tidygraph]{mutate}} or \code{\link[tidygraph]{filter}}, where
#' the network that is currently being worked on is known and thus not needed
#' as an argument to the function. If you want to use an algorithm outside of
#' the tidygraph framework you can use \code{\link[tidygraph]{with_graph}} to
#' set the context temporarily while the algorithm is being evaluated.
#'
#' @return A numeric vector of the same length as the number of edges in the
#' graph.
#'
#' @name spatial_edge_measures
NULL

#' @describeIn spatial_edge_measures The angle in radians between a straight 
#' line from the edge startpoint pointing north, and the straight line from
#' the edge startpoint and the edge endpoint. Calculated with
#' \code{\link[lwgeom]{st_geod_azimuth}}. Requires a geographic CRS.
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel)
#'
#' net %>%
#'   activate("edges") %>%
#'   mutate(azimuth = edge_azimuth())
#'
#' @importFrom lwgeom st_geod_azimuth
#' @importFrom tidygraph .G
#' @export
edge_azimuth = function() {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  bounds = edge_boundary_nodes(x)
  st_geod_azimuth(bounds)[seq(1, length(bounds), 2)]
}

#' @describeIn spatial_edge_measures The ratio of the length of an edge
#' linestring geometry versus the straight-line distance between its boundary
#' nodes, as described in
#' \href{https://journals.sagepub.com/doi/10.1068/b130131p}{Giacomin & Levinson,
#' 2015}.
#' @examples
#' net %>%
#'   activate("edges") %>%
#'   mutate(circuity = edge_circuity())
#'
#' @importFrom sf st_length
#' @importFrom tidygraph .G
#' @export
edge_circuity = function() {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  st_length(x) / straight_line_distance(x)
}

#' @describeIn spatial_edge_measures The length of an edge linestring geometry
#' as calculated by \code{\link[sf]{st_length}}.
#' @examples
#' net %>%
#'   activate("edges") %>%
#'   mutate(length = edge_length())
#'
#' @importFrom sf st_length
#' @importFrom tidygraph .G
#' @export
edge_length = function() {
  x = .G()
  require_active_edges(x)
  if (has_spatially_explicit_edges(x)) {
    st_length(x)
  } else {
    straight_line_distance(x)
  }
}

#' @describeIn spatial_edge_measures The straight-line distance between the two
#' boundary nodes of an edge, as calculated by \code{\link[sf]{st_distance}}.
#' @examples
#' net %>%
#'   activate("edges") %>%
#'   mutate(displacement = edge_displacement())
#'
#' @importFrom tidygraph .G
#' @export
edge_displacement = function() {
  x = .G()
  require_active_edges(x)
  straight_line_distance(x)
}

#' @importFrom sf st_distance
straight_line_distance = function(x) {
  # Extract the nodes from the network.
  nodes = nodes_as_sf(x)
  # Get the indices of the boundary nodes of each edge.
  # Returns a matrix with source ids in column 1 and target ids in column 2.
  ids = edge_boundary_node_indices(x, matrix = TRUE)
  # Get the boundary node geometries of each edge.
  from = nodes[ids[, 1], ]
  to = nodes[ids[, 2], ]
  # Calculate distances pairwise.
  st_distance(from, to, by_element = TRUE)
}

#' Query edges with spatial predicates
#'
#' These functions allow to interpret spatial relations between edges and
#' other geospatial features directly inside \code{\link[tidygraph]{filter}}
#' and \code{\link[tidygraph]{mutate}} calls. All functions return a logical
#' vector of the same length as the number of edges in the network. Element i
#' in that vector is \code{TRUE} whenever \code{any(predicate(x[i], y[j]))} is
#' \code{TRUE}. Hence, in the case of using \code{edge_intersects}, element i
#' in the returned vector is \code{TRUE} when edge i intersects with any of
#' the features given in y.
#'
#' @param y The geospatial features to test the edges against, either as an
#' object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @param ... Arguments passed on to the corresponding spatial predicate
#' function of sf. See \code{\link[sf]{geos_binary_pred}}.
#'
#' @return A logical vector of the same length as the number of edges in the
#' network.
#'
#' @details See \code{\link[sf]{geos_binary_pred}} for details on each spatial
#' predicate. Just as with all query functions in tidygraph, these functions
#' are meant to be called inside tidygraph verbs such as
#' \code{\link[tidygraph]{mutate}} or \code{\link[tidygraph]{filter}}, where
#' the network that is currently being worked on is known and thus not needed
#' as an argument to the function. If you want to use an algorithm outside of
#' the tidygraph framework you can use \code{\link[tidygraph]{with_graph}} to
#' set the context temporarily while the algorithm is being evaluated.
#'
#' @note Note that \code{edge_is_within_distance} is a wrapper around the
#' \code{st_is_within_distance} predicate from sf. Hence, it is based on
#' 'as-the-crow-flies' distance, and not on distances over the network.
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
#' intersects = net %>%
#'   activate(edges) %>%
#'   filter(edge_intersects(poly))
#'
#' par(mar = c(1,1,1,1))
#' plot(st_geometry(net, "edges"))
#' plot(st_geometry(intersects, "edges"), col = "red", lwd = 2, add = TRUE)
#'
#' # Use predicate query function in a mutate call.
#' net %>%
#'   activate(edges) %>%
#'   mutate(disjoint = edge_is_disjoint(poly)) %>%
#'   select(disjoint)
#'
#' @name spatial_edge_predicates
NULL

#' @name spatial_edge_predicates
#' @importFrom sf st_intersects
#' @export
edge_intersects = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_intersects(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_disjoint
#' @export
edge_is_disjoint = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_disjoint(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_touches
#' @export
edge_touches = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_touches(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_crosses
#' @export
edge_crosses = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_crosses(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_within
#' @export
edge_is_within = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_within(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_contains
#' @export
edge_contains = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_contains(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_contains_properly
#' @export
edge_contains_properly = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_contains_properly(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_overlaps
#' @export
edge_overlaps = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_overlaps(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_equals
#' @export
edge_equals = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_equals(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_covers
#' @export
edge_covers = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_covers(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_covered_by
#' @export
edge_is_covered_by = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_covered_by(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_is_within_distance
#' @export
edge_is_within_distance = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_is_within_distance(x, y, ...)) > 0
}
