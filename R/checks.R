#' Check if an object is a sfnetwork
#'
#' @param x Object to be checked.
#'
#' @return \code{TRUE} if the given object is an object of class
#' \code{\link{sfnetwork}}, \code{FALSE} otherwise.
#'
#' @examples
#' library(tidygraph, quietly = TRUE, warn.conflicts = FALSE)
#'
#' net = as_sfnetwork(roxel)
#' is_sfnetwork(net)
#' is_sfnetwork(as_tbl_graph(net))
#'
#' @export
is_sfnetwork = function(x) {
  inherits(x, "sfnetwork")
}

#' @name is_sfnetwork
#' @export
is.sfnetwork = function(x) {
  is_sfnetwork(x)
}

#' Check if a network is focused
#'
#' @param x An object of class \code{\link{sfnetwork}} or
#' \code{\link[tidygraph]{tbl_graph}}.
#'
#' @return \code{TRUE} if the given network is focused on nodes or edges,
#' \code{FALSE} otherwise.
#'
#' @details See \code{\link[tidygraph]{focus}} for more information on focused
#' networks.
#'
#' @noRd
is_focused = function(x) {
  inherits(x, "focused_tbl_graph")
}

#' Check if an object is an sf object
#'
#' @param x Object to be checked.
#'
#' @return \code{TRUE} if the given object is an object of class
#' \code{\link[sf]{sf}}, \code{FALSE} otherwise.
#'
#' @noRd
is_sf = function(x) {
  inherits(x, "sf")
}

#' Check if an object is an sfc object
#'
#' @param x Object to be checked.
#'
#' @return \code{TRUE} if the given object is an object of class
#' \code{\link[sf]{sfc}}, \code{FALSE} otherwise.
#'
#' @noRd
is_sfc = function(x) {
  inherits(x, "sfc")
}

#' Check if an object is an sfc object with linestring geometries
#'
#' @param x Object to be checked.
#'
#' @return \code{TRUE} if the given object is an object of class
#' \code{\link[sf]{sfc}} with geometries of type \code{LINESTRING},
#' \code{FALSE} otherwise.
#'
#' @noRd
is_sfc_linestring = function(x) {
  inherits(x, "sfc_LINESTRING")
}

#' Check if an object is an sfc object with point geometries
#'
#' @param x Object to be checked.
#'
#' @return \code{TRUE} if the given object is an object of class
#' \code{\link[sf]{sfc}} with geometries of type \code{POINT},
#' \code{FALSE} otherwise.
#'
#' @noRd
is_sfc_point = function(x) {
  inherits(x, "sfc_POINT")
}

#' Check if an object is an sfg object
#'
#' @param x Object to be checked.
#'
#' @return \code{TRUE} if the given object is an object of class
#' \code{\link[sf:st]{sfg}}, \code{FALSE} otherwise.
#'
#' @noRd
is_sfg = function(x) {
  inherits(x, "sfg")
}

#' Check if an object has only linestring geometries
#'
#' @param x An object of class \code{\link{sfnetwork}}, \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @return \code{TRUE} if the geometries of the given object are all of type
#' \code{LINESTRING}, \code{FALSE} otherwise.
#'
#' @noRd
are_linestrings = function(x) {
  is_sfc_linestring(st_geometry(x))
}

#' Check if an object has only point geometries
#'
#' @param x An object of class \code{\link{sfnetwork}}, \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @return \code{TRUE} if the geometries of the given object are all of type
#' \code{POINT}, \code{FALSE} otherwise.
#'
#' @noRd
are_points = function(x) {
  is_sfc_point(st_geometry(x))
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
  any(vapply(x, is_sfc, FUN.VALUE = logical(1)), na.rm = TRUE)
}

#' Check if a tbl_graph has nodes with a geometry list column
#'
#' @param x An object of class \code{\link[tidygraph]{tbl_graph}}.
#'
#' @return \code{TRUE} if the nodes table of the tbl_graph has a geometry list
#' column, \code{FALSE} otherwise.
#'
#' @importFrom igraph vertex_attr
#' @noRd
has_spatial_nodes = function(x) {
  cols = vertex_attr(x)
  any(vapply(cols, is_sfc_point, FUN.VALUE = logical(1)), na.rm = TRUE)
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
  cols = edge_attr(x)
  any(vapply(cols, is_sfc_linestring, FUN.VALUE = logical(1)), na.rm = TRUE)
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
  x_crs = if (is_sfnetwork(x)) st_crs(pull_node_geom(x)) else st_crs(x)
  y_crs = if (is_sfnetwork(y)) st_crs(pull_node_geom(y)) else st_crs(y)
  x_crs == y_crs
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
  xp = if (is_sfnetwork(x)) st_precision(pull_node_geom(x)) else st_precision(x)
  yp = if (is_sfnetwork(y)) st_precision(pull_node_geom(y)) else st_precision(y)
  xp == yp
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
  x_is_explicit = has_explicit_edges(x)
  y_is_explicit = has_explicit_edges(y)
  (x_is_explicit && y_is_explicit) || (!x_is_explicit && !y_is_explicit)
}

#' Check if two sf objects have the same geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @param y An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return A logical vector with one element for each (x[i], y[i]) pair. An
#' element is \code{TRUE} if the geometry of x[i] is equal to the geometry of
#' y[i], and \code{FALSE} otherwise.
#'
#' @details This is a pairwise check. Each row in x is compared to its
#' corresponding row in y. Hence, x and y should be of the same length.
#'
#' @importFrom sf st_equals
#' @noRd
have_equal_geometries = function(x, y) {
  equals = st_equals(x, y)
  do.call("c", lapply(seq_along(equals), \(i) i %in% equals[[i]]))
}

#' Check if an object is a single string
#'
#' @param x The object to be checked.
#'
#' @return \code{TRUE} if \code{x} is a single string, \code{FALSE} otherwise.
#'
#' @noRd
is_single_string = function(x) {
  is.character(x) && length(x) == 1
}

#' Check if any boundary point of an edge is equal to any of its incident nodes
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return A logical vector of the same length as the number of edges in the
#' network, holding a \code{TRUE} value if the boundary of the edge geometry
#' contains the geometries of both its incident nodes.
#'
#' @importFrom sf st_equals
#' @noRd
nodes_in_edge_boundaries = function(x) {
  boundary_geoms = edge_boundary_geoms(x)
  incident_geoms = edge_incident_geoms(x)
  # Test for each edge:
  # Does one of the boundary points equals at least one of the incident nodes.
  equals = st_equals(boundary_geoms, incident_geoms)
  is_in = function(i) {
    pool = c(equals[[i]], equals[[i + 1]])
    i %in% pool && i + 1 %in% pool
  }
  do.call("c", lapply(seq(1, length(equals), by = 2), is_in))
}

#' Check if edge boundary points are equal to their corresponding nodes
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return A logical vector of twice the length as the number of edges in the
#' network, with per edge one element for its startpoint and one for its
#' endpoint, holding a \code{TRUE} value if the point is equal to the geometry
#' of the corresponding node.
#'
#' @noRd
nodes_equal_edge_boundaries = function(x) {
  boundary_geoms = edge_boundary_geoms(x)
  incident_geoms = edge_incident_geoms(x)
  # Test if the boundary geometries are equal to their corresponding nodes.
  have_equal_geometries(boundary_geoms, incident_geoms)
}

#' Check if constant attributes will be assumed for a network
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param agr The attribute-geometry relationship values to check against.
#' Defaults to the agr factor of the edges.
#'
#' @param ignore_ids Should known index columns be ignored by the check?
#' Defaults to \code{TRUE}.
#'
#' @return \code{TRUE} when the attribute-geometry relationship of at least
#' one attribute of x is not constant, but sf will for some operations
#' assume that it is, \code{FALSE} otherwise.
#'
#' @noRd
will_assume_constant = function(x, agr = edge_agr(x), ignore_ids = TRUE) {
  if (ignore_ids) {
    ignore = c(
      "from",
      "to",
      ".tidygraph_node_index",
      ".tidygraph_edge_index",
      ".tidygraph_index",
      ".tbl_graph_index",
      ".sfnetwork_index"
    )
    agr = agr[!names(agr) %in% ignore]
  }
  any(is.na(agr)) || any(agr != "constant")
}

#' Check if projected coordinates will be assumed for a network
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return \code{TRUE} when the coordinates of x are longitude-latitude, but sf
#' will for some operations assume they are projected, \code{FALSE} otherwise.
#'
#' @importFrom sf sf_use_s2 st_crs st_is_longlat
#' @noRd
will_assume_projected = function(x) {
  (!is.na(st_crs(x)) && st_is_longlat(x)) && !sf_use_s2()
}
