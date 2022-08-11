#' Check if a table has spatial information stored in a geometry list column
#'
#' @param x A flat table, such as an sf object, data.frame or tibble.
#'
#' @return \code{TRUE} if the table has a geometry list column, \code{FALSE}
#' otherwise.
#'
#' @noRd
has_sfc = function(x) {
  any(vapply(x, is.sfc, FUN.VALUE = logical(1)), na.rm = TRUE)
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
has_single_geom_type = function(x, type) {
  all(st_is(x, type))
}

#' Check if a tbl_graph has nodes with a geometry list column
#'
#' @param x An object of class \code{\link[tidygraph]{tbl_graph}}.
#'
#' @return \code{TRUE} if the nodes table of the tbl_graph has a geometry list
#' column, \code{FALSE} otherwise.
#'
#' @noRd
has_spatial_nodes = function(x) {
  any(vapply(vertex_attr(x), is.sfc, FUN.VALUE = logical(1)), na.rm = TRUE)
}

#' Check if a sfnetwork has spatially explicit edges
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return \code{TRUE} if the network has spatially explicit edges,
#' \code{FALSE} otherwise.
#'
#' @importFrom igraph edge_attr
#' @noRd
has_explicit_edges = function(x) {
  any(vapply(edge_attr(x), is.sfc, FUN.VALUE = logical(1)), na.rm = TRUE)
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
have_equal_crs = function(x, y) {
  st_crs(x) == st_crs(y)
}

#' Check if the precision of two objects is the same
#'
#' @param x An object of class \code{\link{sfnetwork}}, \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @param y An object of class \code{\link{sfnetwork}}, \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @return \code{TRUE} when the precision of x and y is the same, \code{FALSE}
#' otherwise.
#'
#' @importFrom sf st_precision
#' @noRd
have_equal_precision = function(x, y) {
  st_precision(x) == st_precision(y)
}

#' Check if two sfnetworks have the same type of edges
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y An object of class \code{\link{sfnetwork}}.
#'
#' @return \code{TRUE} when the two networks both have spatially explicit edges
#' OR both have spatially implicit edges, \code{FALSE} otherwise.
#'
#' @noRd
have_equal_edge_type = function(x, y) {
  both_explicit = function(x, y) {
    has_explicit_edges(x) && has_explicit_edges(y)
  }
  both_implicit = function(x, y) {
    !has_explicit_edges(x) && !has_explicit_edges(y)
  }
  both_explicit(x, y) || both_implicit(x, y)
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
have_equal_geometries = function(x, y) {
  diag(st_equals(x, y, sparse = FALSE))
}

#' Check if any boundary point of an edge is equal to any of its boundary nodes
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @importFrom sf st_equals
#' @noRd
nodes_in_edge_boundaries = function(x) {
  boundary_points = edge_boundary_points(x)
  boundary_nodes = edge_boundary_nodes(x)
  # Test for each edge :
  # Does one of the boundary points equals at least one of the boundary nodes.
  M = st_equals(boundary_points, boundary_nodes, sparse = FALSE)
  f = function(x) sum(M[x:(x + 1), x:(x + 1)]) > 1
  vapply(seq(1, nrow(M), by = 2), f, FUN.VALUE = logical(1))
}

#' Check if edge boundary points are equal to their corresponding nodes
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @noRd
nodes_match_edge_boundaries = function(x) {
  boundary_points = edge_boundary_points(x)
  boundary_nodes = edge_boundary_nodes(x)
  # Test if the boundary geometries are equal to their corresponding nodes.
  have_equal_geometries(boundary_points, boundary_nodes)
}

#' Check if constant edge attributes will be assumed for a network
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return \code{TRUE} when the attribute-geometry relationship of at least
#' one edge attribute of x is not constant, but sf will for some operations
#' assume that it is, \code{FALSE} otherwise.
#'
#' @noRd
will_assume_constant = function(x) {
  ignore = c(
    "from",
    "to",
    ".tidygraph_edge_index",
    ".tidygraph_index",
    ".sfnetwork_edge_index",
    ".sfnetwork_index"
  )
  agr = edge_agr(x)
  real_agr = agr[!names(agr) %in% ignore]
  any(is.na(real_agr)) || any(real_agr != "constant")
}

#' Check if a planar coordinates will be assumed for a network
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return \code{TRUE} when the coordinates of x are longitude-latitude, but sf
#' will for some operations assume they are planar, \code{FALSE} otherwise.
#'
#' @importFrom sf sf_use_s2 st_crs st_is_longlat
#' @noRd
will_assume_planar = function(x) {
  (!is.na(st_crs(x)) && st_is_longlat(x)) && !sf_use_s2()
}
