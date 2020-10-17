#' Query sf attributes from the active element of an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param name Name of the attribute to query. If \code{NULL}, then all sf 
#' attributes are returned in a list. Defaults to \code{NULL}.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently 
#' active element of x will be used.
#'
#' @return A named list of attributes if \code{name} is \code{NULL},
#' otherwise the value of the attribute matched, or NULL if no exact match is 
#' found and no or more than one partial match is found.
#'
#' @details sf attributes include \code{sf_column} (the name of the sf column)
#' and \code{agr} (the attribute-geometry-relationships).
#'
#' @export
sf_attr = function(x, name = NULL, active = NULL) {
  switch(
    name,
    agr = agr(x, active),
    sf_column = geom_colname(x, active),
    stop("Unknown sf attribute: ", name)
  )
}

#' Preserve the value 'active' attribute of the original network
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

#' Throw an error when an unknown value for the active attribute is provided
#'
#' @param x The provided value for the active attribute.
#'
#' @noRd
throw_unknown_active_exception = function(x) {
  stop(
    "Unknown active element: ", x, ". Only nodes and edges supported",
    call. = FALSE
  )
}

#' Query sf attributes from an sf object
#'
#' @param x An object of class \code{\link[sf]{sf}}.
#'
#' @return A named list of attributes.
#'
#' @details sf attributes include \code{sf_column} (the name of the sf column)
#' and \code{agr} (the attribute-geometry-relationships).
#'
#' @noRd
attrs_from_sf = function(x) {
  list(sf_column = attr(x, "sf_column"), agr = attr(x, "agr"))
}

#' Get attribute column names from the active element of an sfnetwork
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
    throw_unknown_active_exception(active)
  )
}

#' @importFrom igraph vertex_attr_names
node_graph_attribute_names = function(x) {
  igraph::vertex_attr_names(x)
}

#' @importFrom igraph edge_attr_names
edge_graph_attribute_names = function(x) {
  igraph::edge_attr_names(x)
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
    throw_unknown_active_exception(active)
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