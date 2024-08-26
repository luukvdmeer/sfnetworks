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
#' @param force Should validity checks be skipped? Defaults to \code{FALSE},
#' meaning that network validity checks are executed when constructing the
#' network. These checks make sure that the provided neighbor list has a valid
#' structure, i.e. that its length is equal to the number of provided nodes and
#' that its values are all integers referring to one of the nodes.
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
                           compute_length = FALSE, force = FALSE) {
  if (! force) validate_nb(x, nodes)
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
adj_to_nb = function(x, force = FALSE) {
  if (! is.logical(x)) {
    apply(x, 1, \(x) which(as.logical(x)), simplify = FALSE)
  } else {
    apply(x, 1, which, simplify = FALSE)
  }
}

#' Validate the structure of a neighbor list
#'
#' Neighbor lists are sparse adjacency matrices in list format that specify for
#' each node to which other nodes it is adjacent.
#'
#' @param x Object to be validated.
#'
#' @param nodes The nodes that are referenced in the neighbor list as an object
#' of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} with \code{POINT}
#' geometries.
#'
#' @return Nothing if the given object is a valid neighbor list referencing
#' the given nodes. Otherwise, an error is thrown.
#'
#' @importFrom cli cli_abort
#' @importFrom rlang try_fetch
#' @importFrom sf st_geometry
#' @noRd
validate_nb = function(x, nodes) {
  n_nodes = length(st_geometry(nodes))
  # Check 1: Is the length of x equal to the number of provided nodes?
  if (! length(x) == n_nodes) {
    cli_abort(c(
      "The length of the sparse matrix should match the number of nodes.",
      "x" = paste(
        "The provided matrix has length {length(x)},",
        "while there are {n_nodes} nodes."
      )
    ))
  }
  # Check 2: Are all referenced node indices integers?
  if (! all(vapply(x, is.integer, FUN.VALUE = logical(1)))) {
    x = try_fetch(
      lapply(x, as.integer),
      error = function(e) {
        cli_abort("The sparse matrix should contain integer node indices.")
      }
    )
  }
  # Check 3: Are all referenced node indices referring to a provided node?
  ids_in_bounds = function(x) all(x > 0 & x <= n_nodes)
  if (! all(vapply(x, ids_in_bounds, FUN.VALUE = logical(1)))) {
    cli_abort(c(
      "The sparse matrix should contain valid node indices",
      "x" = "Some of the given indices are out of bounds"
    ))
  }
}
