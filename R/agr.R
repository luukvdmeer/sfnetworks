#' Get or set the agr attribute of the active element of an sfnetwork
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
    throw_unknown_active_exception(active)
  )
}

node_agr = function(x) {
  attr(x, "sf")[["nodes"]][["agr"]]
}

edge_agr = function(x) {
  attr(x, "sf")[["edges"]][["agr"]]
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
    throw_unknown_active_exception(active)
  )
}

`node_agr<-` = function(x, value) {
  attr(x, "sf")[["nodes"]][["agr"]] = value
  x
}

`edge_agr<-` = function(x, value) {
  attr(x, "sf")[["edges"]][["agr"]] = value
  x
}

#' Create an updated agr factor for the active element of an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently 
#' active element of x will be used.
#'
#' @return A named factor with appropriate levels. Names are guaranteed to 
#' correspond to the attribute columns of the targeted element of x and are 
#' guaranteed to be sorted in the same order as those attribute columns. 
#' Attribute columns do not involve the geometry list column, but do involve 
#' the from and to columns.
#'
#' @noRd
updated_agr = function(x, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = updated_node_agr(x),
    edges = updated_edge_agr(x),
    throw_unknown_active_exception(active)
  )
}

updated_node_agr = function(x) {
  agr = node_agr(x)
  if (is.null(agr)) {
    new_agr = empty_node_agr(x)
  } else {
    cols = node_spatial_attribute_names(x)
    new_agr = structure(agr[cols], names = cols)
  } 
  new_agr
}

updated_edge_agr = function(x) {
  agr = edge_agr(x)
  if (is.null(agr)) {
    new_agr = empty_edge_agr(x)
  } else {
    cols = edge_spatial_attribute_names(x)
    new_agr = structure(agr[cols], names = cols)
  } 
  new_agr
}

#' Create an empty agr factor for the active element of an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently 
#' active element of x will be used.
#'
#' @return A named factor with appropriate levels. Values are all equal to
#' \code{\link[sf]{NA_agr_}}. Names correspond to the  attribute columns of the 
#' targeted element of x. Attribute columns do not  involve the geometry list 
#' column, but do involve the from and to columns.
#'
#' @noRd
empty_agr = function(x, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = empty_node_agr(x),
    edges = empty_edge_agr(x),
    throw_unknown_active_exception(active)
  )
}

empty_node_agr = function(x) {
  cols = node_spatial_attribute_names(x)
  structure(rep(sf::NA_agr_, length(cols)), names = cols)
}

empty_edge_agr = function(x) {
  cols = edge_spatial_attribute_names(x)
  structure(rep(sf::NA_agr_, length(cols)), names = cols)
}

#' Create a correctly sorted agr factor for the active element of an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the currently 
#' active element of x will be used.
#'
#' @return A named factor with appropriate levels. Names correspond to the 
#' attribute columns of the targeted element of x and are guaranteed to be 
#' sorted in the same order as those attribute columns. Attribute columns do not 
#' involve the geometry list column, but do involve the from and to columns.
#'
#' @noRd
sorted_agr = function(x, active = NULL) {
    if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = sorted_node_agr(x),
    edges = sorted_edge_agr(x),
    throw_unknown_active_exception(active)
  )
}

sorted_node_agr = function(x) {
  agr = node_agr(x)
  cols = node_spatial_attribute_names(x)
  agr[cols]
}

sorted_edge_agr = function(x) {
  agr = edge_agr(x)
  cols = edge_spatial_attribute_names(x)
  agr[cols]
}

#' Concatenate two agr factors
#'
#' @param x A named agr factor with appropriate levels.
#'
#' @param y A named agr factor with appropriate levels.
#'
#' @return A named agr factor with appropriate levels.
#'
#' @noRd
concat_agr = function(x, y) {
  unlist(list(x, y))
}