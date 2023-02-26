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

#' @name agr
#' @importFrom igraph vertex_attr
#' @noRd
node_agr = function(x) {
  agr = attr(vertex_attr(x), "agr")
  make_agr_valid(agr, names = node_feature_attribute_names(x))
}

#' @name agr
#' @importFrom igraph edge_attr
#' @noRd
edge_agr = function(x) {
  agr = attr(edge_attr(x), "agr")
  if (has_explicit_edges(x)) {
    agr = make_agr_valid(agr, names = edge_feature_attribute_names(x))
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
