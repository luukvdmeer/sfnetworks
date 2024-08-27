#' Extract the node or edge indices from a spatial network
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only the indices of features that are in focus be
#' extracted? Defaults to \code{TRUE}. See \code{\link[tidygraph]{focus}} for
#' more information on focused networks.
#'
#' @details The indices in these objects are always integers that correspond to
#' rownumbers in respectively the nodes or edges table.
#'
#' @return An vector of integers.
#'
#' @examples
#' net = as_sfnetwork(roxel[1:10, ])
#' node_ids(net)
#' edge_ids(net)
#'
#' @name ids
#' @importFrom rlang %||%
#' @export
node_ids = function(x, focused = TRUE) {
  if (focused) {
    attr(x, "nodes_focus_index") %||% seq_len(n_nodes(x))
  } else {
    seq_len(n_nodes(x))
  }
}

#' @name ids
#' @importFrom rlang %||%
#' @export
edge_ids = function(x, focused = TRUE) {
  if (focused) {
    attr(x, "edges_focus_index") %||% seq_len(n_edges(x))
  } else {
    seq_len(n_edges(x))
  }
}

#' Extract for each edge in a spatial network the indices of incident nodes
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only edges that are in focus be considered? Defaults
#' to \code{FALSE}. See \code{\link[tidygraph]{focus}} for more information on
#' focused networks.
#'
#' @param matrix Should te result be returned as a two-column matrix? Defaults
#' to \code{FALSE}.
#'
#' @return When extracting both source and target node indices, a numeric
#' vector of length equal to twice the number of edges in x, and ordered as
#' [source of edge 1, target of edge 1, source of edge 2, target of edge 2,
#' ...]. If \code{matrix = TRUE}, a two-column matrix is returned instead, with
#' the number of rows equal to the number of edges in the network. The first
#' column contains the indices of the source nodes and the second column the
#' indices of the target nodes.
#'
#' When only extracting source or target node indices, a numeric vector of
#' length equal to the number of edges in x.
#'
#' @details \code{edge_incident_ids} obtains the indices of incident nodes
#' using the *from* and *to* columns in the edges table.
#' \code{edge_boundary_ids} instead obtains the boundary points of the edge
#' linestring geometries, and check which node geometries are equal to those
#' points. In a valid spatial network structure, the incident indices should be
#' equal to the boundary indices (in directed networks) or the incident indices
#' of each edge should contain the boundary indices of that edge (in undirected
#' networks).
#'
#' @importFrom igraph ends
#' @noRd
edge_incident_ids = function(x, focused = FALSE, matrix = FALSE) {
  ends = ends(x, edge_ids(x, focused = focused), names = FALSE)
  if (matrix) ends else as.vector(t(ends))
}

#' @name edge_incident_ids
#' @importFrom igraph ends
#' @noRd
edge_source_ids = function(x, focused = FALSE, matrix = FALSE) {
  ends(x, edge_ids(x, focused = focused), names = FALSE)[, 1]
}

#' @name edge_incident_ids
#' @importFrom igraph ends
#' @noRd
edge_target_ids = function(x, focused = FALSE, matrix = FALSE) {
  ends(x, edge_ids(x, focused = focused), names = FALSE)[, 2]
}

#' @name edge_indicent_ids
#' @importFrom sf st_equals
#' @noRd
edge_boundary_ids = function(x, focused = FALSE, matrix = FALSE) {
    nodes = pull_node_geom(x)
    edges = edges_as_sf(x, focused = focused)
    idxs_lst = st_equals(linestring_boundary_points(edges), nodes)
    idxs_vct = do.call("c", idxs_lst)
    # In most networks the location of a node will be unique.
    # However, this is not a requirement.
    # There may be cases where multiple nodes share the same geometry.
    # Then some more processing is needed to find the correct indices.
    if (length(idxs_vct) != n_edges(x, focused = focused) * 2) {
      n = length(idxs_lst)
      from = idxs_lst[seq(1, n - 1, 2)]
      to = idxs_lst[seq(2, n, 2)]
      p_idxs = mapply(c, from, to, SIMPLIFY = FALSE)
      n_idxs = mapply(c, edges$from, edges$to, SIMPLIFY = FALSE)
      find_indices = function(a, b) {
        idxs = a[a %in% b]
        if (length(idxs) > 2) b else idxs
      }
      idxs_lst = mapply(find_indices, p_idxs, n_idxs, SIMPLIFY = FALSE)
      idxs_vct = do.call("c", idxs_lst)
    }
    if (matrix) t(matrix(idxs_vct, nrow = 2)) else idxs_vct
}
