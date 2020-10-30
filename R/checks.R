#' Check if a given vector contains duplicates
#'
#' @param x A vector.
#'
#' @return \code{TRUE} when the vector contains duplicated values, 
#' \code{FALSE} otherwise.
#'
#' @noRd
has_duplicates = function(x) {
  any(duplicated(x))
}

#' Check if a table has spatial information stored in a geometry list column
#'
#' @param x A flat table, such as an sf object, data.frame or tibble.
#'
#' @return \code{TRUE} if the table has a geometry list column, \code{FALSE}
#' otherwise.
#'
#' @noRd
has_sfc = function(x) {
  any(sapply(x, is.sfc), na.rm = TRUE)
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
  any(sapply(vertex_attr(x), is.sfc), na.rm = TRUE)
}

#' Check if a sfnetwork has spatially explicit edges
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return \code{TRUE} if the network has spatially explicit edges, \code{FALSE}
#' otherwise.
#'
#' @importFrom igraph edge_attr
#' @noRd
has_spatially_explicit_edges = function(x) {
  any(sapply(edge_attr(x), is.sfc), na.rm = TRUE)
}

#' Check if features in a table have varying attribute values
#'
#' @param x A flat table, such as an sf object, data.frame or tibble.
#'
#' @return \code{TRUE} when the attributes of the features in x are not all
#' the same, \code{FALSE} otherwise.
#'
#' @importFrom sf st_drop_geometry
#' @noRd
has_varying_feature_attributes = function(x) {
  if (is.sf(x)) x = st_drop_geometry(x)
  !all(duplicated(x)[-1])
}

#' Check for empty geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return A boolean vector of the same length as the number of features in x.
#' 
#' @importFrom sf st_dimension
#' @noRd
is_empty = function(x) {
  is.na(st_dimension(x))
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
have_equal_boundary_points = function(x, y) {
  all(
    have_equal_geometries(
      linestring_boundary_points(x), 
      linestring_boundary_points(y)
    )
  )
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
  both_ex = has_spatially_explicit_edges(x) && has_spatially_explicit_edges(y)
  both_im = !has_spatially_explicit_edges(x) && !has_spatially_explicit_edges(y)
  both_ex || both_im
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

#' Check if any of the edge boundary points is equal to any of its boundary nodes
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
  mat = st_equals(boundary_points, boundary_nodes, sparse = FALSE)
  all(
    sapply(
      seq(1, nrow(mat), by = 2),
      function(x) sum(mat[x:(x + 1), x:(x + 1)]) > 1
    )
  )
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
  all(have_equal_geometries(boundary_points, boundary_nodes))
}

#' Check if sf will assume planar coordinates for some operations on an object
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return \code{TRUE} when the coordinates of x are longitude-latitude, but sf
#' will for some operations assume they are planar, \code{FALSE} otherwise.
#'
#' @importFrom sf sf_use_s2 st_is_longlat
#' @noRd
will_assume_planar = function(x) {
  st_is_longlat(x) && !sf_use_s2()
}