#' Query sf attributes from the active element of a sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param name Name of the attribute to query. Either \code{'sf_column'} or
#' \code{'agr'}.
#'
#' @param active Which network element (i.e. nodes or edges) to activate before
#' extracting. If \code{NULL}, it will be set to the current active element of
#' the given network. Defaults to \code{NULL}.
#'
#' @return The value of the attribute matched, or \code{NULL} if no exact 
#' match is found.
#'
#' @details sf attributes include \code{sf_column} (the name of the sf column)
#' and \code{agr} (the attribute-geometry-relationships).
#'
#' @examples
#' net = as_sfnetwork(roxel)
#' sf_attr(net, "agr", active = "edges")
#' sf_attr(net, "sf_column", active = "nodes")
#'
#' @export
sf_attr = function(x, name, active = NULL) {
  switch(
    name,
    agr = agr(x, active),
    sf_column = geom_colname(x, active),
    raise_unknown_input(name)
  )
}

#' Preserve the value of the 'active' attribute of the original network
#'
#' @param new An object of class \code{\link{sfnetwork}}.
#'
#' @param orig An object of class \code{\link{sfnetwork}}.
#'
#' @noRd
`%preserve_active%` = function(new, orig) {
  switch(
    attr(orig, "active"),
    nodes = activate(new, "nodes"),
    edges = activate(new, "edges")
  )
}

#' Get attribute column names from the active element of a sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently
#' active element of x will be used.
#'
#' @return A character vector.
#'
#' @details From the graph point of view, the geometry is considered an
#' attribute of a node or edge, and the indices of the start and end nodes
#' of an edge are not considered attributes of that edge. From the spatial
#' point of view, the geometry is never considered an attribute, but the
#' indices of start and end nodes of an edge are. Hence, the function
#' \code{graph_attribute_names} will return a vector of names that includes
#' the name of the geometry column, but - when active = 'edges' - not the
#' names of the to and from columns. The function \code{spatial_attribute_names}
#' will return a vector of names that does not include the name of the
#' geometry column, but - when active = 'edges' - does include the names of
#' the to and from colums.
#'
#' @name attr_names
#' @noRd
graph_attribute_names = function(x, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = node_graph_attribute_names(x),
    edges = edge_graph_attribute_names(x),
    raise_unknown_input(active)
  )
}

#' @importFrom igraph vertex_attr_names
node_graph_attribute_names = function(x) {
  vertex_attr_names(x)
}

#' @importFrom igraph edge_attr_names
edge_graph_attribute_names = function(x) {
  edge_attr_names(x)
}

#' @name attr_names
#' @noRd
spatial_attribute_names = function(x, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = node_spatial_attribute_names(x),
    edges = edge_spatial_attribute_names(x),
    raise_unknown_input(active)
  )
}

node_spatial_attribute_names = function(x) {
  g_attrs = node_graph_attribute_names(x)
  g_attrs[g_attrs != node_geom_colname(x)]
}

edge_spatial_attribute_names = function(x) {
  g_attrs = edge_graph_attribute_names(x)
  geom_colname = edge_geom_colname(x)
  if (is.null(geom_colname)) {
    character(0)
  } else {
    c("from", "to", g_attrs[g_attrs != geom_colname])
  }
}
