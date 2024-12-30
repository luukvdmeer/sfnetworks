#' Run an igraph function on an sfnetwork object
#'
#' Since \code{\link{sfnetwork}} objects inherit \code{\link[igraph]{igraph}}
#' objects, any igraph function can be called on a sfnetwork. However, if this
#' function returns a network, it will be an igraph object rather than a
#' sfnetwork object. With \code{\link{wrap_igraph}}, such a function will
#' preserve the sfnetwork class, after checking if the network returned by
#' igraph still has a valid spatial network structure.
#'
#' @param .data An object of class \code{\link{sfnetwork}}.
#'
#' @param .f An function from the \code{\link[igraph]{igraph}} package that
#' accepts a graph as its first argument, and returns a graph.
#'
#' @param ... Arguments passed on to \code{.f}.
#'
#' @param .force Should network validity checks be skipped? Defaults to
#' \code{FALSE}, meaning that network validity checks are executed when
#' returning the new network. These checks guarantee a valid spatial network
#' structure. For the nodes, this means that they all should have \code{POINT}
#' geometries. In the case of spatially explicit edges, it is also checked that
#' all edges have \code{LINESTRING} geometries, nodes and edges have the same
#' CRS and boundary points of edges match their corresponding node coordinates.
#' These checks are important, but also time consuming. If you are already sure
#' your input data meet the requirements, the checks are unnecessary and can be
#' turned off to improve performance.
#'
#' @param .message Should informational messages (those messages that are
#' neither warnings nor errors) be printed when constructing the network?
#' Defaults to \code{TRUE}.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @examples
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#'
#' net = as_sfnetwork(mozart, "delaunay", directed = FALSE)
#' mst = wrap_igraph(net, igraph::mst, .message = FALSE)
#' mst
#'
#' plot(net)
#' plot(mst)
#'
#' par(oldpar)
#'
#' @export
wrap_igraph = function(.data, .f, ..., .force = FALSE, .message = TRUE) {
  out = .f(.data, ...) %preserve_all_attrs% .data
  if (! .force) validate_network(out, message = .message)
  out
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