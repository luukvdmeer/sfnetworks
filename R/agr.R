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
    raise_unknown_input(active)
  )
}

#' @importFrom igraph vertex_attr
node_agr = function(x) {
  agr = attr(vertex_attr(x), "agr")
  valid_agr(agr, node_spatial_attribute_names(x))
}

#' @importFrom igraph edge_attr
edge_agr = function(x) {
  agr = attr(edge_attr(x), "agr")
  if (has_spatially_explicit_edges(x)) {
    agr = valid_agr(agr, edge_spatial_attribute_names(x))
  }
  agr
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
    raise_unknown_input(active)
  )
}

#' @importFrom igraph vertex_attr<-
`node_agr<-` = function(x, value) {
  attr(vertex_attr(x), "agr") = value
  x
}

#' @importFrom igraph edge_attr<-
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
#' @param levels A character vector containing the levels that should be 
#' present in the agr factor.
#'
#' @return A named factor with appropriate levels. Names are guaranteed to
#' correspond to the attribute columns of the targeted element of x and are
#' guaranteed to be sorted in the same order as those attribute columns.
#' Attribute columns do not involve the geometry list column, but do involve
#' the from and to columns.
#'
#' @noRd
valid_agr = function(agr, names, levels = sf:::agr_levels) {
  if (is.null(agr)) {
    new_agr = empty_agr(names)
  } else {
    new_agr = structure(agr[names], names = names, levels = levels)
  }
  new_agr
}
