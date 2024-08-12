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
    raise_unknown_input("name", name, c("agr", "sf_column"))
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
  `%preserve_sf_attrs%`(`%preserve_network_attrs%`(new, orig), orig)
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
  node_geom_colname = node_geom_colname(orig)
  if (! is.null(node_geom_colname)) {
    node_geom_colname(new) = node_geom_colname
    node_agr(new) = node_agr(orig)
  }
  edge_geom_colname = edge_geom_colname(orig)
  if (! is.null(edge_geom_colname)) {
    edge_geom_colname(new) = edge_geom_colname
    edge_agr(new) = edge_agr(orig)
  }
  new
}

#' Get attribute column names from the active element of a sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently
#' active element of x will be used.
#'
#' @param idxs Should the columns storing indices of start and end nodes in the
#' edges table (i.e. the from and to columns) be considered attribute columns?
#' Defaults to \code{FALSE}.
#'
#' @param geom Should the geometry column be considered an attribute column?
#' Defaults to \code{TRUE}.
#'
#' @return A character vector.
#'
#' @name attr_names
#' @noRd
attribute_names = function(x, active = NULL, idxs = FALSE, geom = TRUE) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = node_attribute_names(x, geom = geom),
    edges = edge_attribute_names(x, idxs = idxs, geom = geom),
    raise_invalid_active(active)
  )
}

#' @name attr_names
#' @noRd
#' @importFrom igraph vertex_attr_names
node_attribute_names = function(x, geom = TRUE) {
  attrs = vertex_attr_names(x)
  if (! geom) {
    attrs = attrs[attrs != node_geom_colname(x)]
  }
  attrs
}

#' @name attr_names
#' @noRd
#' @importFrom igraph edge_attr_names
edge_attribute_names = function(x, idxs = FALSE, geom = TRUE) {
  attrs = edge_attr_names(x)
  if (idxs) {
    attrs = c("from", "to", attrs)
  }
  if (! geom) {
    geom_colname = edge_geom_colname(x)
    if (! is.null(geom_colname)) {
      attrs = attrs[attrs != geom_colname]
    }
  }
  attrs
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
#' @details For these functions, the geometry is considered an attribute of a
#' node or edge, and the indices of the start and end nodes of an edge are not
#' considered attributes of that edge.
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
    raise_invalid_active(active)
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

#' Get the specified summary function for an attribute column.
#'
#' @param attr Name of the attribute.
#'
#' @param spec Specification of the summary function belonging to each
#' attribute.
#'
#' @return A function that takes a vector of attribute values as input and
#' returns a single value.
#'
#' @noRd
get_summary_function = function(attr, spec) {
  if (!is.list(spec)) {
    func = spec
  } else {
    names = names(spec)
    if (is.null(names)) {
      func = spec[[1]]
    } else {
      func = spec[[attr]]
      if (is.null(func)) {
        default = which(names == "")
        if (length(default) > 0) {
          func = spec[[default[1]]]
        } else {
          func = "ignore"
        }
      }
    }
  }
  if (is.function(func)) {
    func
  } else {
    summariser(func)
  }
}

#' @importFrom stats median
#' @importFrom utils head tail
summariser = function(name) {
  switch(
    name,
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
    raise_unknown_summariser(name)
  )
}
