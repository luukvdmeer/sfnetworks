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
#' @details All attributes include the network attributes *and* the sf specific
#' attributes of its element objects (i.e. the nodes and edges tables).
#'
#' The network attributes always contain the class of the network and the name
#' of the active element. Users can also add their own attributes to the
#' network.
#'
#' The sf specific element attributes contain the name of the geometry list
#' column and the agr factor of the element. In a spatially implicit network
#' these attributes will be \code{NULL} for the edges table. Note that we talk
#' about the attributes of the element *objects*. Hence, attributes attached to
#' the table that stores the elements data. This is *not* the same as the
#' attribute columns *in* the element table.
#'
#' @importFrom igraph graph_attr graph_attr<-
#' @noRd
`%preserve_all_attrs%` = function(new, orig) {
  graph_attr(new) = graph_attr(orig)
  attributes(new) = attributes(orig)
  node_geom_colname(new) = node_geom_colname(orig)
  node_agr(new) = node_agr(orig)
  edge_geom_colname(new) = edge_geom_colname(orig)
  edge_agr(new) = edge_agr(orig)
  new
}

#' Preserve the attributes of the original network
#'
#' @param new An object of class \code{\link{sfnetwork}}.
#'
#' @param orig An object of class \code{\link{sfnetwork}}.
#'
#' @details The network attributes are the attributes directly attached to
#' the network object as a whole. Hence, this does *not* include attributes
#' belonging to the element objects (i.e. the nodes and the edges tables). The
#' network attributes always contain the class of the network and the name of
#' the active element. Users can also add their own attributes to the network.
#'
#' @importFrom igraph graph_attr graph_attr<-
#' @noRd
`%preserve_network_attrs%` = function(new, orig) {
  graph_attr(new) = graph_attr(orig)
  attributes(new) = attributes(orig)
  new
}

#' Preserve the sf specific attributes of the nodes and edges tables
#'
#' @param new An object of class \code{\link{sfnetwork}}.
#'
#' @param orig An object of class \code{\link{sfnetwork}}.
#'
#' @details The sf specific attributes of the network elements (i.e. the nodes
#' and edges tables) contain the name of the geometry list column and the agr
#' factor of the element. In a spatially implicit network these attributes will
#' be \code{NULL} for the edges table. Note that we talk about the attributes
#' of the element *objects*. Hence, attributes attached to the table that
#' stores the elements data. This is *not* the same as the attribute columns
#' *in* the element table.
#'
#' @noRd
`%preserve_sf_attrs%` = function(new, orig) {
  node_geom_colname(new) = node_geom_colname(orig)
  node_agr(new) = node_agr(orig)
  edge_geom_colname(new) = edge_geom_colname(orig)
  edge_agr(new) = edge_agr(orig)
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
#' @details Which columns in the nodes or edges table of the network are
#' considered attribute columns can be different depending on our perspective.
#'
#' From the graph-centric point of view, the geometry is considered an
#' attribute of a node or edge. Edges are defined by the nodes they connect,
#' and hence the from and to columns in the edges table define the edges,
#' rather than being attributes of them. Therefore, the function
#' \code{attribute_names} will return a vector of names that includes the name
#' of the geometry column, but - when \code{active = "edges"} - not the names
#' of the to and from columns.
#'
#' However, when we take a geometry-centric point of view, the geometries are
#' spatial features that contain attributes. Such a feature is defined by its
#' geometry, and hence the geometry list-column is not considered an attribute
#' column. The indices of the start and end nodes, however, are considered
#' attributes of the edge linestring features. Therefore, the function
#' \code{feature_attribute_names} will return a vector of names that does not
#' include the name of the geometry column, but - when \code{active = "edges"}
#' - does include the names of the to and from columns.
#'
#' @name attr_names
#' @noRd
attribute_names = function(x, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = node_attribute_names(x),
    edges = edge_attribute_names(x),
    raise_unknown_input(active)
  )
}

#' @name attr_names
#' @noRd
#' @importFrom igraph vertex_attr_names
node_attribute_names = function(x) {
  vertex_attr_names(x)
}

#' @name attr_names
#' @noRd
#' @importFrom igraph edge_attr_names
edge_attribute_names = function(x) {
  edge_attr_names(x)
}

#' @name attr_names
#' @noRd
feature_attribute_names = function(x, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = node_feature_attribute_names(x),
    edges = edge_feature_attribute_names(x),
    raise_unknown_input(active)
  )
}

#' @name attr_names
#' @noRd
node_feature_attribute_names = function(x) {
  g_attrs = node_attribute_names(x)
  g_attrs[g_attrs != node_geom_colname(x)]
}

#' @name attr_names
#' @noRd
edge_feature_attribute_names = function(x) {
  g_attrs = edge_attribute_names(x)
  geom_colname = edge_geom_colname(x)
  if (is.null(geom_colname)) {
    character(0)
  } else {
    c("from", "to", g_attrs[g_attrs != geom_colname])
  }
}

#' Set or replace attribute column values of the active element of a sfnetwork
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
#' @details From the network-centric point of view, the geometry is considered
#' an attribute of a node or edge, and the indices of the start and end nodes
#' of an edge are not considered attributes of that edge.
#'
#' @name attr_values
#' @noRd
`attribute_values<-` = function(x, active = NULL, value) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = `node_attribute_values<-`(x, value),
    edges = `edge_attribute_values<-`(x, value),
    raise_unknown_input(active)
  )
}

#' @name attr_values
#' @noRd
#' @importFrom igraph vertex_attr<-
`node_attribute_values<-` = function(x, value) {
  vertex_attr(x) = as.list(value)
  x
}

#' @name attr_values
#' @noRd
#' @importFrom igraph edge_attr<-
`edge_attribute_values<-` = function(x, value) {
  edge_attr(x) = as.list(value[, !names(value) %in% c("from", "to")])
  x
}

#' Get an attribute summary function
#'
#' @param label A character string referring to the summary function.
#'
#' @return Definition of a function that takes a vector of attribute values as
#' input and returns a single value.
#'
#' @importFrom stats median
#' @importFrom utils head tail
#'
#' @noRd
attribute_summary_function = function(label) {
  if (is.function(label)) {
    label
  } else {
    switch(
      label,
      ignore = function(x) NA,
      sum = function(x) sum(x),
      prod = function(x) prod(x),
      min = function(x) min(x),
      max = function(x) max(x),
      random = function(x) sample(x, 1),
      first = function(x) head(x, 1),
      last = function(x) tail(x, 1),
      mean = function(x) mean(x),
      median = function(x) median(x),
      concat = function(x) c(x),
      raise_unknown_input(label)
    )
  }
}