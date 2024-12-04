#' Get the indices of the nodes adjacent to a given node
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param node The integer index of the node for which adjacent nodes should be
#' queried.
#'
#' @param direction The direction of travel. Defaults to \code{'out'}, meaning
#' that the direction given by the network is followed and a node is adjacent
#' if it can be reached by an outgoing edge. May be set to \code{'in'}, meaning
#' that the opposite direction is followed. May also be set to \code{'all'},
#' meaning that the network is considered to be undirected. This argument is
#' ignored for undirected networks.
#'
#' @returns A vector of integer indices specifying the adjacent nodes to the
#' given node.
#'
#' @importFrom igraph adjacent_vertices igraph_opt igraph_options
#' @noRd
node_adjacencies = function(x, node, direction = "out") {
  # Change default igraph options.
  # This prevents igraph returns node or edge indices as formatted sequences.
  # We only need the "raw" integer indices.
  # Changing this option can lead to quite a performance improvement.
  default_igraph_opt = igraph_opt("return.vs.es")
  if (default_igraph_opt) {
    igraph_options(return.vs.es = FALSE)
    on.exit(igraph_options(return.vs.es = default_igraph_opt))
  }
  # Query adjacent nodes and correct for zero-based indexing if needed.
  adjacent_vertices(x, node, mode = direction)[[1]] + get_igraph_offset()
}

#' Get the indices of the edges incident to given nodes
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param nodes A vector of integer indices specifying the nodes for which
#' incident edges should be queried.
#'
#' @returns A list in which each element is a vector of integer indices
#' specifying the incident edges to one of the given nodes.
#'
#' @importFrom igraph incident_edges igraph_opt igraph_options
#' @noRd
node_incidents = function(x, nodes) {
  # Change default igraph options.
  # This prevents igraph returns node or edge indices as formatted sequences.
  # We only need the "raw" integer indices.
  # Changing this option can lead to quite a performance improvement.
  default_igraph_opt = igraph_opt("return.vs.es")
  if (default_igraph_opt) {
    igraph_options(return.vs.es = FALSE)
    on.exit(igraph_options(return.vs.es = default_igraph_opt))
  }
  # Query incident edges and correct for zero-based indexing if needed.
  ids = incident_edges(x, nodes, mode = "all")
  ids = lapply(ids, `+`, get_igraph_offset())
  ids
}

#' Get the offset of node and edge indices returned by igraph
#'
#' The functions \code{\link[igraph]{adjacent_vertices}} and
#' \code{\link[igraph]{incident_edges}} used to return zero-based indices.
#' Since v2.1.2, it returns one-based indices instead. To not fix the required
#' igraph version to the latest release, this utility function finds the offset
#' of returned indices compared to one-based indexing.
#'
#' @note This function assumes that the igraph option \code{return.vs.es} is
#' set to \code{FALSE}!
#'
#' @returns An integer, 1 if zero-based indexing is used, and 0 if one-based
#' indexing is used.
#'
#' @importFrom igraph adjacent_vertices make_graph
#' @noRd
get_igraph_offset = function() {
  if (! is.null(igraph_offset$offset)) return(igraph_offset$offset)
  net = make_graph(edges = c(1L, 2L))
  idx = as.integer(adjacent_vertices(net, v = 1L, mode = "out"))
  off = 2L - idx
  igraph_offset$offset = off
  off
}

igraph_offset = new.env(parent = emptyenv())