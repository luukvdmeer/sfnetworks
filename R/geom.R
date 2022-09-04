#' Get or set the sf_column attribute of the active element of a sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param value A character.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently
#' active element of x will be used.
#'
#' @return For the getter, a character. The setter only modifies x.
#'
#' @noRd
geom_colname = function(x, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = node_geom_colname(x),
    edges = edge_geom_colname(x),
    raise_unknown_input(active)
  )
}

#' @importFrom igraph vertex_attr vertex_attr_names
node_geom_colname = function(x) {
  col = attr(vertex_attr(x), "sf_column")
  if (is.null(col)) {
    # Take the name of the first sfc column.
    sfc_idx = which(vapply(vertex_attr(x), is.sfc, FUN.VALUE = logical(1)))[1]
    col = vertex_attr_names(x)[sfc_idx]
  }
  col
}

#' @importFrom igraph edge_attr edge_attr_names
edge_geom_colname = function(x) {
  col = attr(edge_attr(x), "sf_column")
  if (is.null(col) && has_explicit_edges(x)) {
    # Take the name of the first sfc column.
    sfc_idx = which(vapply(edge_attr(x), is.sfc, FUN.VALUE = logical(1)))[1]
    col = edge_attr_names(x)[sfc_idx]
  }
  col
}

#' @name geom_colname
#' @noRd
`geom_colname<-` = function(x, active = NULL, value) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = `node_geom_colname<-`(x, value),
    edges = `edge_geom_colname<-`(x, value),
    raise_unknown_input(active)
  )
}

#' @importFrom igraph vertex_attr<-
`node_geom_colname<-` = function(x, value) {
  attr(vertex_attr(x), "sf_column") = value
  x
}

#' @importFrom igraph edge_attr<-
`edge_geom_colname<-` = function(x, value) {
  attr(edge_attr(x), "sf_column") = value
  x
}

#' Pull the geometry column from the active element of a sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently
#' active element of x will be used.
#'
#' @return An object of class \code{\link[sf]{sfc}}.
#'
#' @noRd
pull_geom = function(x, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = pull_node_geom(x),
    edges = pull_edge_geom(x),
    raise_unknown_input(active)
  )
}

#' @importFrom igraph vertex_attr
pull_node_geom = function(x) {
  geom = vertex_attr(x, node_geom_colname(x))
  if (! is.sfc(geom)) raise_invalid_sf_column()
  geom
}

#' @importFrom igraph edge_attr
pull_edge_geom = function(x) {
  require_explicit_edges(x)
  geom = edge_attr(x, edge_geom_colname(x))
  if (! is.sfc(geom)) raise_invalid_sf_column()
  geom
}

#' Mutate the geometry column of the active element of a sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y An object of class \code{\link[sf]{sfc}}, or character.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently
#' active element of x will be used.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @details Note that the returned network will not be checked for a valid
#' sfnetwork structure. This happens only in the exported `st_geometry<-`
#' method for sfnetwork object.
#'
#' @noRd
mutate_geom = function(x, y, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = mutate_node_geom(x, y),
    edges = mutate_edge_geom(x, y),
    raise_unknown_input(active)
  )
}

#' @importFrom igraph vertex_attr<-
#' @importFrom sf st_geometry
mutate_node_geom = function(x, y) {
  nodes = nodes_as_sf(x)
  st_geometry(nodes) = y
  node_attribute_values(x) = nodes
  x
}

#' @importFrom igraph edge_attr<-
#' @importFrom sf st_geometry
mutate_edge_geom = function(x, y) {
  edges = edges_as_table(x)
  st_geometry(edges) = y
  edge_attribute_values(x) = edges
  x
}

#' Drop the geometry column of the active element of a sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently
#' active element of x will be used.
#'
#' @return An object of class \code{\link{sfnetwork}} when active = 'edges', an
#' object of class \code{\link[tidygraph]{tbl_graph}} when active = 'nodes'.
#'
#' @noRd
drop_geom = function(x, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = drop_node_geom(x),
    edges = drop_edge_geom(x),
    raise_unknown_input(active)
  )
}

#' @importFrom igraph delete_vertex_attr
#' @importFrom tidygraph as_tbl_graph
drop_node_geom = function(x) {
  x_new = delete_vertex_attr(x, node_geom_colname(x))
  node_geom_colname(x_new) = NULL
  node_agr(x_new) = NULL
  as_tbl_graph(x_new)
}

#' @importFrom igraph delete_edge_attr
drop_edge_geom = function(x) {
  geom_col = edge_geom_colname(x)
  if (is.null(geom_col)) {
    stop("Edges are already spatially implicit", call. = FALSE)
  }
  x_new = delete_edge_attr(x, edge_geom_colname(x))
  edge_geom_colname(x_new) = NULL
  edge_agr(x_new) = NULL
  x_new
}
