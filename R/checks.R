#' Check if the output of an st_join operation has multiple matches
#'
#' @param x The output of an \code{st_join(a,b)} where the original row
#' indices of \code{a} are stored in a column named \code{.sfnetwork_index}.
#'
#' @return \code{TRUE} when there where multiple matches, \code{FALSE}
#' otherwise.
#'
#' @noRd
has_multiple_matches = function(x) {
  any(table(x$.sfnetwork_index) > 1)
}

#' Check if an sfnetwork has spatially explicit edges
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return \code{TRUE} if the network has spatially explicit edges, \code{FALSE}
#' otherwise.
#'
#' @importFrom igraph edge_attr
#' @noRd
has_spatially_explicit_edges = function(x) {
  any(sapply(igraph::edge_attr(x), is.sfc), na.rm = TRUE)
}

#' Check if a graph is directed.
#'
#' @param x An object of \code{\link{sfnetwork}}, \code{tbl_graph} 
#' or \code{igraph}.
#'
#' @return \code{TRUE} when the given graph is directed, \code{FALSE} otherwise.
#'
#' @importFrom igraph is_directed
#' @noRd
is_directed = function(x) {
  igraph::is_directed(x)
}

#' Check if a table has spatial information stored in a geometry list column
#'
#' @param x Object to check for spatial explicitness.
#'
#' @return \code{TRUE} if the table has a geometry list column, \code{FALSE}
#' otherwise.
#'
#' @noRd
is_spatially_explicit = function(x) {
  any(sapply(x, is.sfc), na.rm = TRUE)
}

#' Check if the CRS of two objects are the same
#'
#' @param x An object of class \code{\link{sfnetwork}}, \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @param y An object of class \code{\link{sfnetwork}}, \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @return \code{TRUE} when the CRS of x and y are the same, \code{FALSE}
#' otherwise.
#'
#' @importFrom sf st_crs
#' @noRd
same_crs = function(x, y) {
  sf::st_crs(x) == sf::st_crs(y)
}

#' Check if two sf objects have the same LINESTRING boundary points
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @param y An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @return \code{TRUE} when the boundary points are the same, \code{FALSE}
#' otherwise.
#'
#' @details This is a pairwise check. Each row in x is compared to its
#' corresponding row in y. Hence, x and y should be of the same length.
#'
#' @noRd
same_boundary_points = function(x, y) {
  all(same_geometries(get_boundary_points(x), get_boundary_points(y)))
}

#' Check if two sf objects have the same geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @param y An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return A vector of booleans, one element for each (x[i], y[i]) pair.
#'
#' @details This is a pairwise check. Each row in x is compared to its
#' corresponding row in y. Hence, x and y should be of the same length.
#'
#' @importFrom sf st_equals
#' @noRd
same_geometries = function(x, y) {
  diag(sf::st_equals(x, y, sparse = FALSE))
}

#' Check if any of the edge boundary points is equal to any of its boundary nodes
#'
#' @param nodes Nodes element of an \code{\link{sfnetwork}}.
#'
#' @param edges Edges element of an \code{\link{sfnetwork}}.
#'
#' @importFrom sf st_equals
#' @noRd
nodes_in_edge_boundaries = function(nodes, edges) {
  # Get geometries of all edge boundary points.
  edge_boundary_geoms = get_boundary_points(edges)
  # Get geometries of all edge boundary nodes.
  edge_boundary_nodes = get_boundary_nodes(nodes, edges)
  # Test for each edge :
  # Does one of the boundary points equals at least one of the boundary nodes.
  mat = sf::st_equals(edge_boundary_geoms, edge_boundary_nodes, sparse = FALSE)
  all(
    sapply(
      seq(1, nrow(mat), by = 2),
      function(x) sum(mat[x:(x + 1), x:(x + 1)]) > 1
    )
  )
}

#' Check if edge boundary points of are equal to their corresponding nodes
#'
#' @param nodes Nodes element of an \code{\link{sfnetwork}}.
#'
#' @param edges Edges element of an \code{\link{sfnetwork}}.
#'
#' @noRd
nodes_match_edge_boundaries = function(nodes, edges) {
  # Get geometries of all edge boundary points.
  edge_boundary_geoms = get_boundary_points(edges)
  # Get geometries of all edge boundary nodes.
  edge_boundary_nodes = get_boundary_nodes(nodes, edges)
  # Test if the boundary geometries are equal to their corresponding nodes.
  all(same_geometries(edge_boundary_geoms, edge_boundary_nodes))
}

#' Check if geometries are all of a specific type
#'
#' @param x An object of class \code{\link{sfnetwork}} or \code{\link[sf]{sf}}.
#'
#' @param type The geometry type to check for, as a string.
#'
#' @return \code{TRUE} when all geometries are of the given type, \code{FALSE}
#' otherwise.
#'
#' @importFrom sf st_is
#' @noRd
st_is_all = function(x, type) {
  all(sf::st_is(x, type))
}