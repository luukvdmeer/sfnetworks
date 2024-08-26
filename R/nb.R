#' Conversion between neighbor lists and sfnetworks
#'
#' Neighbor lists are sparse adjacency matrices in list format that specify for
#' each node to which other nodes it is adjacent. They occur for example in the
#' \code{\pkg{sf}} package as \code{\link[sf]{sgbp}} objects, and are also
#' frequently used in the \code{\pkg{spdep}} package.
#'
#' @param x For the conversion to sfnetwork: a neighbor list, which is a list
#  with one element per node that holds the integer indices of the nodes it is
#' adjacent to. For the conversion from sfnetwork: an object of class
#' \code{\link{sfnetwork}}.
#'
#' @param nodes The nodes themselves as an object of class \code{\link[sf]{sf}}
#' or \code{\link[sf]{sfc}} with \code{POINT} geometries.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param edges_as_lines Should the created edges be spatially explicit, i.e.
#' have \code{LINESTRING} geometries stored in a geometry list column? Defaults
#' to \code{TRUE}.
#'
#' @param compute_length Should the geographic length of the edges be stored in
#' a column named \code{length}? Defaults to \code{FALSE}.
#'
#' @param direction The direction that defines if two nodes are neighbors.
#' Defaults to \code{'out'}, meaning that the direction given by the network is
#' followed and node j is only a neighbor of node i if there exists an edge
#' i->j. May be set to \code{'in'}, meaning that the opposite direction is
#' followed and node j is only a neighbor of node i if there exists an edge
#' j->i. May also be set to \code{'all'}, meaning that the network is
#' considered to be undirected. This argument is ignored for undirected
#' networks.
#'
#' @return For the conversion to sfnetwork: An object of class
#' \code{\link{sfnetwork}}. For the conversion from sfnetwork: a neighbor list,
#' which is a list with one element per node that holds the integer indices of
#' the nodes it is adjacent to.
#'
#' @name nb
NULL

#' @name nb
#' @importFrom tibble tibble
#' @export
nb_to_sfnetwork = function(x, nodes, directed = TRUE, edges_as_lines = TRUE,
                           compute_length = FALSE) {
  # Define the edges by their from and to nodes.
  # An edge will be created between each neighboring node pair.
  edges = rbind(
    rep(c(1:length(x)), lengths(x)),
    do.call("c", x)
  )
  if (! directed && length(edges) > 0) {
    # If the network is undirected:
    # --> Edges i -> j and j -> i are the same.
    # --> We create the network only with unique edges.
    edges = unique(apply(edges, 2, sort), MARGIN = 2)
  }
  # Create the sfnetwork object.
  sfnetwork(
    nodes = nodes,
    edges = tibble(from = edges[1, ], to = edges[2, ]),
    directed = directed,
    edges_as_lines = edges_as_lines,
    compute_length = compute_length,
    force = TRUE
  )
}

#' @name nb
#' @importFrom igraph as_adj_list igraph_opt igraph_options
#' @export
sfnetwork_to_nb = function(x, direction = "out") {
  # Change default igraph options.
  # This prevents igraph returns node or edge indices as formatted sequences.
  # We only need the "raw" integer indices.
  # Changing this option improves performance especially on large networks.
  default_igraph_opt = igraph_opt("return.vs.es")
  igraph_options(return.vs.es = FALSE)
  on.exit(igraph_options(return.vs.es = default_igraph_opt))
  # Return the neighbor list, without node names.
  nb = as_adj_list(x, mode = direction, loops = "once", multiple = FALSE)
  names(nb) = NULL
  nb
}

#' Convert an adjacency matrix into a neighbor list
#'
#' Adjacency matrices of networks are n x n matrices with n being the number of
#' nodes, and element Aij holding a \code{TRUE} value if node i is adjacent to
#' node j, and a \code{FALSE} value otherwise. Neighbor lists are the sparse
#' version of these matrices, coming in the form of a list with one element per
#' node, holding the indices of the nodes it is adjacent to.
#'
#' @param x An adjacency matrix of class \code{\link{matrix}}. Non-logical
#' matrices are first converted into logical matrices using
#' \code{\link{as.logical}}.
#'
#' @return The sparse adjacency matrix as object of class \code{\link{list}}.
#'
#' @noRd
adj_to_nb = function(x) {
  if (! is.logical(x)) {
    apply(x, 1, \(x) which(as.logical(x)), simplify = FALSE)
  } else {
    apply(x, 1, which, simplify = FALSE)
  }
}