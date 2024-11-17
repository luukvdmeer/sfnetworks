#' Proceed only when a given network element is active
#'
#' @details These function are meant to be called in the context of an
#' operation in which the network that is currently being worked on is known
#' and thus not needed as an argument to the function.
#'
#' @return Nothing when the expected network element is active, an error
#' message otherwise.
#'
#' @name require_active
#' @importFrom cli cli_abort
#' @importFrom tidygraph .graph_context
#' @noRd
require_active_nodes <- function() {
  if (!.graph_context$free() && .graph_context$active() != "nodes") {
    cli_abort("This call requires nodes to be active.")
  }
}

#' @name require_active
#' @importFrom cli cli_abort
#' @importFrom tidygraph .graph_context
#' @noRd
require_active_edges <- function() {
  if (!.graph_context$free() && .graph_context$active() != "edges") {
    cli_abort("This call requires edges to be active.")
  }
}

#' Proceed only when edges are spatially explicit
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return Nothing when the edges of x are spatially explicit, an error message
#' otherwise.
#'
#' @importFrom cli cli_abort
#' @noRd
require_explicit_edges = function(x) {
  if (! has_explicit_edges(x)) raise_require_explicit()
}
