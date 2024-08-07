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
  any(vapply(vertex_attr(x), is_sfc, FUN.VALUE = logical(1)), na.rm = TRUE)
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
  any(vapply(edge_attr(x), is_sfc, FUN.VALUE = logical(1)), na.rm = TRUE)
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

#' Proceed only when a given network element is active
#'
#' @details These function are meant to be called in the context of an
#' operation in which the network that is currently being worked on is known
#' and thus not needed as an argument to the function.
#'
#' @return Nothing when the expected network element is active, an error
#' message otherwise.
#'
#' @name require_active
#' @importFrom cli cli_abort
#' @importFrom tidygraph .graph_context
#' @noRd
require_active_nodes <- function() {
  if (!.graph_context$free() && .graph_context$active() != "nodes") {
    cli_abort("This call requires nodes to be active.")
  }
}

#' @name require_active
#' @importFrom cli cli_abort
#' @importFrom tidygraph .graph_context
#' @noRd
require_active_edges <- function() {
  if (!.graph_context$free() && .graph_context$active() != "edges") {
    cli_abort("This call requires edges to be active.")
  }
}

#' Proceed only when edges are spatially explicit
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return Nothing when the edges of x are spatially explicit, an error message
#' otherwise.
#'
#' @importFrom cli cli_abort
#' @noRd
require_explicit_edges = function(x) {
  if (! has_explicit_edges(x)) {
    cli_abort(c(
      "This call requires spatially explicit edges.",
      "i" = "If you meant to call it on the nodes, activate nodes first.",
      "i" = "Call {.code convert(x, to_spatial_explicit} to explicitize edges."
    ))
  }
}


#' Proceed only if the given object is a valid adjacency matrix
#'
#' Adjacency matrices of networks are n x n matrices with n being the number of
#' nodes, and element Aij holding a \code{TRUE} value if node i is adjacent to
#' node j, and a \code{FALSE} value otherwise.
#'
#' @param x Object to be checked.
#'
#' @param nodes The nodes that are referenced in the matrix as an object
#' of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} with \code{POINT}
#' geometries.
#'
#' @return Nothing if the given object is a valid adjacency matrix
#' referencing the given nodes, an error message otherwise.
#'
#' @importFrom cli cli_abort
#' @importFrom sf st_geometry
#' @noRd
require_valid_adjacency_matrix = function(x, nodes) {
  n_nodes = length(st_geometry(nodes))
  if (! (nrow(x) == n_nodes && ncol(x) == n_nodes)) {
    cli_abort(
      c(
        "The dimensions of the matrix should match the number of nodes.",
        "x" = paste(
          "The provided matrix has dimensions {nrow(x)} x {ncol(x)},",
          "while there are {n_nodes} nodes."
        )
      )
    )
  }
}

#' Proceed only if the given object is a valid neighbor list
#'
#' Neighbor lists are sparse adjacency matrices in list format that specify for
#' each node to which other nodes it is adjacent.
#'
#' @param x Object to be checked.
#'
#' @param nodes The nodes that are referenced in the neighbor list as an object
#' of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} with \code{POINT}
#' geometries.
#'
#' @return Nothing if the given object is a valid neighbor list referencing
#' the given nodes, and error message afterwards.
#'
#' @importFrom cli cli_abort
#' @importFrom sf st_geometry
#' @noRd
require_valid_neighbor_list = function(x, nodes) {
  n_nodes = length(st_geometry(nodes))
  if (! length(x) == n_nodes) {
    cli_abort(
      c(
        "The length of the sparse matrix should match the number of nodes.",
        "x" = paste(
          "The provided matrix has length {length(x)},",
          "while there are {n_nodes} nodes."
        )
      )
    )
  }
  if (! all(vapply(x, is.integer, FUN.VALUE = logical(1)))) {
    cli_abort("The sparse matrix should contain integer node indices.")
  }
}
