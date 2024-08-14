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

#' Get or set the agr attribute of the active element of a sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param value A named factor with appropriate levels. Names should
#' correspond to the attribute columns of the targeted element of x. Attribute
#' columns do not involve the geometry list column, but do involve the from and
#' to columns.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently
#' active element of x will be used.
#'
#' @return For the getter, a named agr factor. The setter only modifies x.
#'
#' @noRd
agr = function(x, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = node_agr(x),
    edges = edge_agr(x),
    raise_invalid_active(active)
  )
}

#' @name agr
#' @importFrom igraph vertex_attr
#' @noRd
node_agr = function(x) {
  agr = attr(vertex_attr(x), "agr")
  colnames = node_colnames(x, geom = FALSE)
  make_agr_valid(agr, names = colnames)
}

#' @name agr
#' @importFrom igraph edge_attr
#' @noRd
edge_agr = function(x) {
  agr = attr(edge_attr(x), "agr")
  colnames = edge_colnames(x, idxs = TRUE, geom = FALSE)
  make_agr_valid(agr, names = colnames)
}

#' @name agr
#' @noRd
`agr<-` = function(x, active = NULL, value) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = `node_agr<-`(x, value),
    edges = `edge_agr<-`(x, value),
    raise_invalid_active(active)
  )
}

#' @name agr
#' @importFrom igraph vertex_attr<-
#' @noRd
`node_agr<-` = function(x, value) {
  attr(vertex_attr(x), "agr") = value
  x
}

#' @name agr
#' @importFrom igraph edge_attr<-
#' @noRd
`edge_agr<-` = function(x, value) {
  attr(edge_attr(x), "agr") = value
  x
}

#' Create an empty agr factor
#'
#' @param names A character vector containing the names that should be present
#' in the agr factor.
#'
#' @return A named factor with appropriate levels. Values are all equal to
#' \code{\link[sf]{NA_agr_}}. Names correspond to the  attribute columns of the
#' targeted element of x. Attribute columns do not  involve the geometry list
#' column, but do involve the from and to columns.
#'
#' @noRd
empty_agr = function(names) {
  structure(rep(sf::NA_agr_, length(names)), names = names)
}

#' Make an agr factor valid
#'
#' @param agr The agr factor to be made valid.
#'
#' @param names A character vector containing the names that should be present
#' in the agr factor.
#'
#' @return A named factor with appropriate levels. Names are guaranteed to
#' correspond to the attribute columns of the targeted element of x and are
#' guaranteed to be sorted in the same order as those attribute columns.
#' Attribute columns do not involve the geometry list column, but do involve
#' the from and to columns.
#'
#' @noRd
make_agr_valid = function(agr, names) {
  levels = c("constant", "aggregate", "identity")
  if (is.null(agr)) {
    valid_agr = empty_agr(names)
  } else {
    valid_agr = structure(agr[names], names = names, levels = levels)
  }
  valid_agr
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
