#' Querying spatial edge measures
#'
#' These functions are a collection of specific spatial edge measures, that
#' form a spatial extension to edge measures in 
#' \code{\link[tidygraph:tidygraph-package]{tidygraph}}.
#'
#' @details Just as with all tidygraph algorithms, spatial edge measures are 
#' meant to be called inside tidygraph verbs such as \code{mutate()}, where 
#' the graph that is currently being worked on is known and thus not needed 
#' as an argument to the function. If you want to use an algorithm outside of
#' the tidygraph framework you can use \code{\link[tidygraph]{with_graph}} to 
#' set the graph context temporarily while the algorithm is being evaluated.
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