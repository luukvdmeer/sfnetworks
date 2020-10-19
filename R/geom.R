#' Get or set the sf_column attribute of the active element of an sfnetwork
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
    throw_unknown_active_exception(active)
  )
}

node_geom_colname = function(x) {
  attr(x, "sf")[["nodes"]][["sf_column"]]
}

edge_geom_colname = function(x) {
  attr(x, "sf")[["edges"]][["sf_column"]]
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
    throw_unknown_active_exception(active)
  )
}

`node_geom_colname<-` = function(x, value) {
  attr(x, "sf")[["nodes"]][["sf_column"]] = value
  x
}

`edge_geom_colname<-` = function(x, value) {
  attr(x, "sf")[["edges"]][["sf_column"]] = value
  x
}

#' Mutate the geometry column of the active element of an sfnetwork
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
    throw_unknown_active_exception(active)
  )
}

#' @importFrom igraph vertex_attr
#' @importFrom rlang !! :=
#' @importFrom tidygraph mutate
mutate_node_geom = function(x, y) {
  if (is.character(y)) {
    # Set another column to be the new geometry column.
    stopifnot(is.sfc(igraph::vertex_attr(x, y)))
    x_new = x
    node_geom_colname(x_new) = y
  } else {
    # Replace the geometries of the current geometry column with new values.
    geom_col = node_geom_colname(x)
    x_new = tidygraph::mutate(activate(x, "nodes"), !!geom_col := y)
  }
  # Update agr.
  node_agr(x_new) = updated_node_agr(x_new)
  x_new %preserve_active% x
}

#' @importFrom igraph edge_attr edge_attr_names
#' @importFrom rlang !! :=
#' @importFrom tidygraph mutate
mutate_edge_geom = function(x, y) {
  if (is.character(y)) {
    # Set another column to be the new geometry column.
    stopifnot(is.sfc(igraph::edge_attr(x, y)))
    x_new = x
    edge_geom_colname(x_new) = y
    edge_agr(x_new) = updated_edge_agr(x_new)
  } else {
    # Replace the geometries in the current geometry column with y.
    geom_col = edge_geom_colname(x)
    # What if there is currently no column marked as geometry column?
    # First: check if there are sfc columns and take the first of those.
    # Then: create a new column named "geometry".
    if (is.null(geom_col)) {
      if (has_sfc(igraph::edge_attr(x))) {
        sfc_col = which(sapply(igraph::edge_attr(x), function(x) is.sfc))[1]
        geom_col = igraph::edge_attr_names(x)[sfc_col]
        warning("Overwriting sfc column '", sfc_col, "'", call. = FALSE)
      } else {
        geom_col = "geometry"
        if ("geometry" %in% igraph::edge_attr_names(x)) {
          warning("Overwriting column 'geometry'", call. = FALSE)
        }
      }
    }
    # Replace.
    x_new = tidygraph::mutate(activate(x, "edges"), !!geom_col := y)
    # Update sf attributes.
    edge_geom_colname(x_new) = geom_col
    edge_agr(x_new) = updated_edge_agr(x_new)
  }
  x_new %preserve_active% x
}

#' Drop the geometry column of the active element of an sfnetwork
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
    throw_unknown_active_exception(active)
  )
}

#' @importFrom igraph delete_vertex_attr
drop_node_geom = function(x) {
  x_new = igraph::delete_vertex_attr(x, node_geom_colname(x))
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
  x_new = igraph::delete_edge_attr(x, edge_geom_colname(x))
  edge_geom_colname(x_new) = NULL
  edge_agr(x_new) = NULL
  x_new
}