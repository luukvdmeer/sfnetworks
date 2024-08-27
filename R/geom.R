#' Get or set the geometry column name of the active element of a sfnetwork
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
#' @param focused Should only features that are in focus be pulled? Defaults
#' to \code{FALSE}. See \code{\link[tidygraph]{focus}} for more information on
#' focused networks.
#'
#' @return An object of class \code{\link[sf]{sfc}}.
#'
#' @noRd
pull_geom = function(x, active = NULL, focused = FALSE) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = pull_node_geom(x, focused = focused),
    edges = pull_edge_geom(x, focused = focused),
    raise_invalid_active(active)
  )
}

#' @name pull_geom
#' @importFrom igraph vertex_attr
#' @noRd
pull_node_geom = function(x, focused = FALSE) {
  geom = vertex_attr(x, node_geom_colname(x))
  if (! is_sfc(geom)) raise_invalid_sf_column()
  if (focused && is_focused(x)) geom = geom[node_ids(x, focused = TRUE)]
  geom
}

#' @name pull_geom
#' @importFrom igraph edge_attr
#' @noRd
pull_edge_geom = function(x, focused = FALSE) {
  geom_colname = edge_geom_colname(x)
  if (is.null(geom_colname)) raise_require_explicit()
  geom = edge_attr(x, geom_colname)
  if (! is_sfc(geom)) raise_invalid_sf_column()
  if (focused && is_focused(x)) geom = geom[edge_ids(x, focused = TRUE)]
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
#' @param focused Should only features that are in focus be mutated? Defaults
#' to \code{FALSE}. See \code{\link[tidygraph]{focus}} for more information on
#' focused networks.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @details Note that the returned network will not be checked for a valid
#' sfnetwork structure. This happens only in the exported `st_geometry<-`
#' method for sfnetwork object.
#'
#' @noRd
mutate_geom = function(x, y, active = NULL, focused = FALSE) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = mutate_node_geom(x, y, focused = focused),
    edges = mutate_edge_geom(x, y, focused = focused),
    raise_invalid_active(active)
  )
}

#' @name mutate_geom
#' @importFrom sf st_geometry<-
#' @noRd
mutate_node_geom = function(x, y, focused = FALSE) {
  nodes = nodes_as_sf(x)
  if (focused && is_focused(x)) {
    st_geometry(nodes[node_ids(x, focused = TRUE), ]) = y
  } else {
    st_geometry(nodes) = y
  }
  node_data(x) = nodes
  x
}

#' @name mutate_geom
#' @importFrom sf st_geometry<-
#' @noRd
mutate_edge_geom = function(x, y, focused = FALSE) {
  edges = edge_data(x, focused = FALSE)
  if (focused && is_focused(x)) {
    st_geometry(edges[edge_ids(x, focused = TRUE), ]) = y
  } else {
    st_geometry(edges) = y
  }
  edge_data(x) = edges
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

#' Extract for each edge in a spatial network the geometries of incident nodes
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only edges that are in focus be considered? Defaults
#' to \code{FALSE}. See \code{\link[tidygraph]{focus}} for more information on
#' focused networks.
#'
#' @param list Should te result be returned as a two-element list? Defaults
#' to \code{FALSE}.
#'
#' @return When extracting both source and target node geometries, an object of
#' class \code{\link[sf]{sfc}} with \code{POINT} geometries of length equal to
#' twice the number of edges in x, and ordered as [source of edge 1, target of
#' edge 1, source of edge 2, target of edge 2, ...]. If \code{list = TRUE}, a
#' list of length two is returned instead. The first element contains the
#' source node geometries and the second element the target node geometries.
#'
#' When only extracting source or target node geometries, an object of class
#' \code{\link[sf]{sfc}} with \code{POINT} geometries, of length equal to the
#' number of edges in x.
#'
#' @details \code{edge_incident_geoms} obtains the geometries of incident nodes
#' using the *from* and *to* columns in the edges table.
#' \code{edge_boundary_geoms} instead obtains the boundary points of the edge
#' linestring geometries, and check which node geometries are equal to those
#' points. In a valid spatial network structure, the incident geometries should
#' be equal to the boundary geometries (in directed networks) or the incident
#' geometries of each edge should contain the boundary geometries of that edge
#' (in undirected networks).
#'
#' @importFrom igraph ends
#' @noRd
edge_incident_geoms = function(x, focused = FALSE, list = FALSE) {
  nodes = pull_node_geom(x)
  ids = ends(x, edge_ids(x, focused = focused), names = FALSE)
  if (list) {
    list(nodes[ids[, 1]], nodes[ids[, 2]])
  } else {
    nodes[as.vector(t(ids))]
  }
}

#' @name edge_incident_geoms
#' @importFrom igraph ends
#' @noRd
edge_source_geoms = function(x, focused = FALSE) {
  nodes = pull_node_geom(x)
  id_mat = ends(x, edge_ids(x, focused = focused), names = FALSE)
  nodes[id_mat[, 1]]
}

#' @name edge_incident_geoms
#' @importFrom igraph ends
#' @noRd
edge_target_geoms = function(x, focused = FALSE) {
  nodes = pull_node_geom(x)
  id_mat = ends(x, edge_ids(x, focused = focused), names = FALSE)
  nodes[id_mat[, 2]]
}

#' @name edge_incident_geoms
#' @noRd
edge_boundary_geoms = function(x, focused = FALSE, list = FALSE) {
  edges = pull_edge_geom(x, focused = focused)
  points = linestring_boundary_points(edges)
  if (list) {
    starts = points[seq(1, length(points), 2)]
    ends = points[seq(2, length(points), 2)]
    list(starts, ends)
  } else {
    points
  }
}
