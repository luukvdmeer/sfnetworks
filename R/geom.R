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
    raise_invalid_active(active)
  )
}

#' @name geom_colname
#' @importFrom igraph vertex_attr vertex_attr_names
#' @noRd
node_geom_colname = function(x) {
  col = attr(vertex_attr(x), "sf_column")
  if (is.null(col)) {
    # Take the name of the first sfc column with point geometries.
    is_sfc = vapply(vertex_attr(x), is_sfc_point, FUN.VALUE = logical(1))
    sfc_idx = which(is_sfc)[1]
    if (! is.na(sfc_idx)) col = vertex_attr_names(x)[sfc_idx]
  }
  col
}

#' @name geom_colname
#' @importFrom igraph edge_attr edge_attr_names
#' @noRd
edge_geom_colname = function(x) {
  col = attr(edge_attr(x), "sf_column")
  if (is.null(col)) {
    # Take the name of the first sfc column with linestring geometries.
    # If this does not exist (implicit edges) col stays NULL.
    is_sfc = vapply(edge_attr(x), is_sfc_linestring, FUN.VALUE = logical(1))
    sfc_idx = which(is_sfc)[1]
    if (! is.na(sfc_idx)) col = edge_attr_names(x)[sfc_idx]
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
    raise_invalid_active(active)
  )
}

#' @name geom_colname
#' @importFrom igraph vertex_attr<-
#' @noRd
`node_geom_colname<-` = function(x, value) {
  attr(vertex_attr(x), "sf_column") = value
  x
}

#' @name geom_colname
#' @importFrom igraph edge_attr<-
#' @noRd
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
    raise_invalid_active(active)
  )
}

#' @name pull_geom
#' @importFrom igraph vertex_attr
#' @noRd
pull_node_geom = function(x) {
  geom = vertex_attr(x, node_geom_colname(x))
  if (! is_sfc(geom)) raise_invalid_sf_column()
  geom
}

#' @name pull_geom
#' @importFrom igraph edge_attr
#' @noRd
pull_edge_geom = function(x) {
  geom_colname = edge_geom_colname(x)
  if (is.null(geom_colname)) raise_require_explicit()
  geom = edge_attr(x, geom_colname)
  if (! is_sfc(geom)) raise_invalid_sf_column()
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
    raise_invalid_active(active)
  )
}

#' @name mutate_geom
#' @importFrom sf st_geometry
#' @noRd
mutate_node_geom = function(x, y) {
  nodes = nodes_as_sf(x)
  st_geometry(nodes) = y
  node_attribute_values(x) = nodes
  x
}

#' @name mutate_geom
#' @importFrom sf st_geometry
#' @noRd
mutate_edge_geom = function(x, y) {
  edges = edge_data(x)
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
    raise_invalid_active(active)
  )
}

#' @name drop_geom
#' @importFrom igraph delete_vertex_attr
#' @importFrom tidygraph as_tbl_graph
#' @noRd
drop_node_geom = function(x) {
  x_new = delete_vertex_attr(x, node_geom_colname(x))
  node_geom_colname(x_new) = NULL
  node_agr(x_new) = NULL
  as_tbl_graph(x_new)
}

#' @name drop_geom
#' @importFrom igraph delete_edge_attr
#' @noRd
drop_edge_geom = function(x) {
  geom_col = edge_geom_colname(x)
  if (is.null(geom_col)) return(x)
  x_new = delete_edge_attr(x, edge_geom_colname(x))
  edge_geom_colname(x_new) = NULL
  x_new
}
