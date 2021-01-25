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

#' Preserve the attributes of the original network and its elements
#'
#' @param new An object of class \code{\link{sfnetwork}}.
#'
#' @param orig An object of class \code{\link{sfnetwork}}.
#'
#' @details All attributes include the graph attributes *and* the attributes
#' of its element objects (i.e. nodes and edges). Graph attributes always 
#' contain the class of the object and the name of the active element. Users 
#' can also add their own attributes to the network. Element attributes 
#' contain the name of the geometry list column and the agr factor of the
#' element. Note that we talk about the attributes of the element *objects*. 
#' Hence, attributes attached to the object that stores the elements data. 
#' This is *not* the same as the attribute columns *in* the element data.
#'
#' @importFrom igraph graph_attr graph_attr<-
#' @noRd
`%preserve_all_attrs%` = function(new, orig) {
  graph_attr(new) = graph_attr(orig)
  attributes(new) = attributes(orig)
  attributes(vertex_attr(new)) = attributes(vertex_attr(orig))
  attributes(edge_attr(new)) = attributes(edge_attr(orig))
  new
}

#' Preserve the attributes of the original network
#'
#' @param new An object of class \code{\link{sfnetwork}}.
#'
#' @param orig An object of class \code{\link{sfnetwork}}.
#'
#' @details The graph attributes are the attributes directly attached to
#' the network object as a whole. Hence, this does *not* include attributes 
#' belonging to the element objects (i.e. the nodes table or the edges table). 
#' Graph attributes always includes the class of the object and the name of the
#' active element. Users can also add their own attributes to the network.
#'
#' @importFrom igraph graph_attr graph_attr<-
#' @noRd
`%preserve_graph_attrs%` = function(new, orig) {
  graph_attr(new) = graph_attr(orig)
  attributes(new) = attributes(orig)
  new
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
#' names of the to and from columns. The function 
#' \code{spatial_attribute_names} will return a vector of names that does not 
#' include the name of the geometry column, but - when active = 'edges' - does 
#' include the names of the to and from colums.
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

#' Set the attributes of the active element of a sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently
#' active element of x will be used.
#'
#' @param value A table in which each column is an attribute to be set. If the
#' nodes are active, this table has to be of class \code{\link[sf]{sf}}. For
#' the edges, it can also be a \code{data.frame} or 
#' \code{\link[tibble]{tibble}}.
#'
#' @return An object of class \code{\link{sfnetwork}} with updated attributes.
#'
#' @details From the graph point of view, the geometry is considered an
#' attribute of a node or edge, and the indices of the start and end nodes
#' of an edge are not considered attributes of that edge.
#'
#' @name graph_attributes
#' @noRd
`graph_attributes<-` = function(x, active = NULL, value) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = `node_graph_attributes<-`(x, value),
    edges = `edge_graph_attributes<-`(x, value),
    raise_unknown_input(active)
  )
}

#' @importFrom igraph vertex_attr<-
`node_graph_attributes<-` = function(x, value) {
  vertex_attr(x) = as.list(value)
  x
}

#' @importFrom igraph edge_attr<-
`edge_graph_attributes<-` = function(x, value) {
  edge_attr(x) = as.list(value[, !names(value) %in% c("from", "to")])
  x
}
