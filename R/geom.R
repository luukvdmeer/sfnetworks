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
    sfc_idx = which(sapply(vertex_attr(x), is.sfc))[1]
    col = vertex_attr_names(x)[sfc_idx]
  }
  col
}

#' @importFrom igraph edge_attr edge_attr_names
edge_geom_colname = function(x) {
  col = attr(edge_attr(x), "sf_column")
  if (has_spatially_explicit_edges(x) && is.null(col)) {
    # Take the name of the first sfc column.
    sfc_idx = which(sapply(edge_attr(x), is.sfc))[1]
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

#' @importFrom igraph vertex_attr
#' @importFrom rlang !! :=
#' @importFrom tidygraph mutate
mutate_node_geom = function(x, y) {
  if (is.character(y)) {
    # Set another column to be the new geometry column.
    stopifnot(is.sfc(vertex_attr(x, y)))
    node_geom_colname(x) = y
  } else {
    # Replace the geometries of the current geometry column with new values.
    geom_col = node_geom_colname(x)
    x = mutate(activate(x, "nodes"), !!geom_col := y) %preserve_active% x
  }
  x
}

#' @importFrom igraph edge_attr edge_attr_names
#' @importFrom rlang !! :=
#' @importFrom tidygraph mutate
mutate_edge_geom = function(x, y) {
  if (is.character(y)) {
    # Set another column to be the new geometry column.
    stopifnot(is.sfc(edge_attr(x, y)))
    edge_geom_colname(x) = y
  } else {
    stopifnot(is.sfc(y))
    # Replace the geometries in the current geometry column with y.
    geom_col = edge_geom_colname(x)
    # What if there is currently no column marked as geometry column?
    # --> This means a new geometry column is created.
    # --> Use the same geometry column name as the one in the nodes.
    # --> If that is an existing column, create a column named 'geometry'.
    if (is.null(geom_col)) {
      geom_col = node_geom_colname(x)
      if (geom_col %in% edge_attr_names(x)) {
        geom_col = "geometry"
        if (geom_col %in% edge_attr_names(x)) raise_overwrite(geom_col)
      }
    }
    # Replace.
    x = mutate(activate(x, "edges"), !!geom_col := y) %preserve_active% x
    edge_geom_colname(x) = geom_col
  }
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
