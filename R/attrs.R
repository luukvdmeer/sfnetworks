attrs_from_sf = function(x) {
  list(sf_column = attr(x, "sf_column"), agr = attr(x, "agr"))
}

empty_agr = function(attr_names) {
  structure(rep(sf::NA_agr_, length(attr_names)), names = attr_names)
}

order_agr = function(x) {
  agr = sf_attr(x, "agr", "edges")
  ordered_agr = unlist(
    list(agr["from"], agr["to"], agr[setdiff(names(agr), c("from", "to"))])
  )
  sf_attr(x, "agr", "edges") = ordered_agr
  x
}

#' Query sf attributes from the active element of an sfnetwork object
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param name Name of the attribute to query. If \code{NULL}, then all sf 
#' attributes are returned in a list. Defaults to \code{NULL}.
#'
#' @param active Which network element (i.e. nodes or edges) to activate before
#' extracting. If \code{NULL}, it will be set to the current active element of
#' the given network. Defaults to \code{NULL}.
#'
#' @param value The new value of the attribute, or \code{NULL} to remove the 
#' attribute.
#'
#' @return For the extractor: a list of attributes if \code{name} is \code{NULL},
#' otherwise the value of the attribute matched, or NULL if no exact match is 
#' found and no or more than one partial match is found.
#'
#' @details sf attributes include \code{sf_column} (the name of the sf column)
#' and \code{agr} (the attribute-geometry-relationships).
#'
#' @name sf_attr
#' @importFrom igraph edge_attr vertex_attr
#' @export
sf_attr = function(x, name = NULL, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  if (is.null(name)) {
    switch(
      active,
      nodes = attr(x, "sf")[["nodes"]],
      edges = attr(x, "sf")[["edges"]]
    )
  } else {
    switch(
      active,
      nodes = attr(x, "sf")[["nodes"]][[name]],
      edges = attr(x, "sf")[["edges"]][[name]]
    )
  }
}

#' @name sf_attr
#' @export
`sf_attr<-` = function(x, name, active = NULL, value) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = set_node_sf_attr(x, name, value),
    edges = set_edge_sf_attr(x, name, value)
  )
}

set_node_sf_attr = function(x, name, value) {
  attr(x, "sf")[["nodes"]][[name]] = value
  x
}

set_edge_sf_attr = function(x, name, value) {
  attr(x, "sf")[["edges"]][[name]] = value
  x
}