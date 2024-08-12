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


#' Proceed only if the given object is a valid adjacency matrix
#'
#' Adjacency matrices of networks are n x n matrices with n being the number of
#' nodes, and element Aij holding a \code{TRUE} value if node i is adjacent to
#' node j, and a \code{FALSE} value otherwise.
#'
#' @param x Object to be checked.
#'
#' @param nodes The nodes that are referenced in the matrix as an object
#' of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} with \code{POINT}
#' geometries.
#'
#' @return Nothing if the given object is a valid adjacency matrix
#' referencing the given nodes, an error message otherwise.
#'
#' @importFrom cli cli_abort
#' @importFrom sf st_geometry
#' @noRd
require_valid_adjacency_matrix = function(x, nodes) {
  n_nodes = length(st_geometry(nodes))
  if (! (nrow(x) == n_nodes && ncol(x) == n_nodes)) {
    cli_abort(
      c(
        "The dimensions of the matrix should match the number of nodes.",
        "x" = paste(
          "The provided matrix has dimensions {nrow(x)} x {ncol(x)},",
          "while there are {n_nodes} nodes."
        )
      )
    )
  }
}

#' Proceed only if the given object is a valid neighbor list
#'
#' Neighbor lists are sparse adjacency matrices in list format that specify for
#' each node to which other nodes it is adjacent.
#'
#' @param x Object to be checked.
#'
#' @param nodes The nodes that are referenced in the neighbor list as an object
#' of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} with \code{POINT}
#' geometries.
#'
#' @return Nothing if the given object is a valid neighbor list referencing
#' the given nodes, and error message afterwards.
#'
#' @importFrom cli cli_abort
#' @importFrom sf st_geometry
#' @noRd
require_valid_neighbor_list = function(x, nodes) {
  n_nodes = length(st_geometry(nodes))
  if (! length(x) == n_nodes) {
    cli_abort(
      c(
        "The length of the sparse matrix should match the number of nodes.",
        "x" = paste(
          "The provided matrix has length {length(x)},",
          "while there are {n_nodes} nodes."
        )
      )
    )
  }
  if (! all(vapply(x, is.integer, FUN.VALUE = logical(1)))) {
    cli_abort("The sparse matrix should contain integer node indices.")
  }
}
