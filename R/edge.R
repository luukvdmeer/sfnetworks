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

#' @describeIn spatial_edge_measures The ratio of the length of an edge
#' linestring geometry versus the straight-line distance between its boundary
#' nodes, as described in
#' \href{https://journals.sagepub.com/doi/10.1068/b130131p}{Giacomin & Levinson,
#' 2015}.
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
#' @importFrom tidygraph .G
#' @export
edge_straight_length = function() {
  x = .G()
  require_active_edges(x)
  straight_line_distance(x)
}

#' @importFrom sf st_as_sf st_distance
straight_line_distance = function(x) {
  # Extract the nodes from the network.
  nodes = st_as_sf(x, "nodes")
  # Get the indices of the boundary nodes of each edge.
  # Returns a matrix with source ids in column 1 and target ids in column 2.
  ids = edge_boundary_node_indices(x)
  # Get the boundary node geometries of each edge.
  from = nodes[ids[, 1], ]
  to = nodes[ids[, 2], ]
  # Calculate distances pairwise.
  st_distance(from, to, by_element = TRUE)
}

#' Query edges with spatial predicates
#'
#' These functions allow to interpretate spatial relations between edges and
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
#' predicate. Just as with all query functions in tidygraph, spatial edge 
#' measures are meant to be called inside tidygraph verbs such as 
#' \code{\link[tidygraph]{mutate}} or \code{\link[tidygraph]{filter}}, where 
#' the network that is currently being worked on is known and thus not needed 
#' as an argument to the function. If you want to use an algorithm outside of
#' the tidygraph framework you can use \code{\link[tidygraph]{with_graph}} to 
#' set the context temporarily while the algorithm is being evaluated.
#'
#' @name spatial_edge_predicates
NULL

#' @name spatial_edge_predicates
#' @importFrom sf st_intersects
edge_intersects = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_intersects(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_disjoint
edge_is_disjoint = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_disjoint(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_touches
edge_touches = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_touches(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_crosses
edge_crosses = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_crosses(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_within
edge_is_within = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_within(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_contains
edge_contains = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_contains(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_contains_properly
edge_contains_properly = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_contains_properly(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_overlaps
edge_overlaps = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_overlaps(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_equals
edge_equals = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_equals(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_covers
edge_covers = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_covers(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_covered_by
edge_is_covered_by = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_covered_by(x, y, ...)) > 0
}

#' @name spatial_edge_predicates
#' @importFrom sf st_is_within_distance
edge_is_within_distance = function(y, ...) {
  x = .G()
  require_active_edges(x)
  require_spatially_explicit_edges(x)
  lengths(st_is_within_distance(x, y, ...)) > 0
}
