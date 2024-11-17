#' Extract all node or edge indices from a spatial network
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
#' @return A vector of integers.
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

#' Query specific node indices from a spatial network
#'
#' This function is not meant to be called directly, but used inside other
#' functions that accept a node query.
#'
#' @param data An object of class \code{\link{sfnetwork}}.
#'
#' @param query The query that defines for which nodes to extract indices,
#' defused into a \code{\link[rlang:topic-quosure]{quosure}}. See Details for
#' the different ways in which node queries can be formulated.
#'
#' @details There are multiple ways in which node indices can be queried in
#' sfnetworks. The query can be formatted as follows:
#'
#' \itemize{
#'   \item As spatial features: Spatial features can be given as object of
#'   class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}. The nearest node to
#'   each feature is found by calling \code{\link[sf]{st_nearest_feature}}.
#'   \item As node type query function: A
#'   \link[tidygraph:node_types]{node type query function} defines for each
#'   node if it is of a given type or not. Nodes that meet the criterium are
#'   queried.
#'   \item As node predicate query function: A
#'   \link[=spatial_node_predicates]{node predicate query function} defines
#'   for each node if a given spatial predicate applies to the spatial relation
#'   between that node and other spatial features. Nodes that meet the
#'   criterium are queried.
#'   \item As column name: The referenced column is expected to have logical
#'   values defining for each node if it should be queried or not. Note that
#'   tidy evaluation is used and hence the column name should be unquoted.
#'   \item As integers: Integers are interpreted as node indices. A node index
#'   corresponds to a row-number in the nodes table of the network.
#'   \item As characters: Characters are interpreted as node names. A node name
#'   corresponds to a value in a column named "name" in the the nodes table of
#'   the network. Note that this column is expected to store unique names
#'   without any duplicated values.
#'   \item As logicals: Logicals should define for each node if it should be
#'   queried or not.
#' }
#'
#' Queries that can not be evaluated in any of the ways described above will be
#' forcefully converted to integers using \code{\link{as.integer}}.
#'
#' @return A vector of queried node indices.
#'
#' @importFrom cli cli_abort
#' @importFrom igraph vertex_attr
#' @importFrom rlang eval_tidy
#' @importFrom tidygraph .N .register_graph_context
#' @export
evaluate_node_query = function(data, query) {
  .register_graph_context(data, free = TRUE)
  nodes = eval_tidy(query, .N())
  if (is_sf(nodes) | is_sfc(nodes)) {
    nodes = nearest_node_ids(data, nodes)
  } else if (is.logical(nodes)) {
    nodes = which(nodes)
  } else if (is.character(nodes)) {
    names = vertex_attr(data, "name")
    if (is.null(names)) {
      cli_abort(c(
        "Failed to match node names.",
        "x" = "There is no node attribute {.field name}.",
        "i" = paste(
          "When querying nodes using names it is expected that these",
          "names are stored in a node attribute named {.field name}"
        )
      ))
    }
    nodes = match(nodes, names)
  }
  if (! is.integer(nodes)) nodes = as.integer(nodes)
  nodes
}

#' Query specific edge indices from a spatial network
#'
#' This function is not meant to be called directly, but used inside other
#' functions that accept an edge query.
#'
#' @param data An object of class \code{\link{sfnetwork}}.
#'
#' @param query The query that defines for which edges to extract indices,
#' defused into a \code{\link[rlang:topic-quosure]{quosure}}. See Details for
#' the different ways in which edge queries can be formulated.
#'
#' @details There are multiple ways in which edge indices can be queried in
#' sfnetworks. The query can be formatted as follows:
#'
#' \itemize{
#'   \item As spatial features: Spatial features can be given as object of
#'   class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}. The nearest edge to
#'   each feature is found by calling \code{\link[sf]{st_nearest_feature}}.
#'   \item As edge type query function: A
#'   \link[tidygraph:edge_types]{edge type query function} defines for each
#'   edge if it is of a given type or not. Nodes that meet the criterium are
#'   queried.
#'   \item As edge predicate query function: A
#'   \link[=spatial_edge_predicates]{edge predicate query function} defines
#'   for each edge if a given spatial predicate applies to the spatial relation
#'   between that edge and other spatial features. Nodes that meet the
#'   criterium are queried.
#'   \item As column name: The referenced column is expected to have logical
#'   values defining for each edge if it should be queried or not. Note that
#'   tidy evaluation is used and hence the column name should be unquoted.
#'   \item As integers: Integers are interpreted as edge indices. A edge index
#'   corresponds to a row-number in the edges table of the network.
#'   \item As characters: Characters are interpreted as edge names. A edge name
#'   corresponds to a value in a column named "name" in the the edges table of
#'   the network. Note that this column is expected to store unique names
#'   without any duplicated values.
#'   \item As logicals: Logicals should define for each edge if it should be
#'   queried or not.
#' }
#'
#' Queries that can not be evaluated in any of the ways described above will be
#' forcefully converted to integers using \code{\link{as.integer}}.
#'
#' @return A vector of queried edge indices.
#'
#' @importFrom cli cli_abort
#' @importFrom igraph edge_attr
#' @importFrom rlang eval_tidy
#' @importFrom tidygraph .E .register_graph_context
#' @export
evaluate_edge_query = function(data, query) {
  .register_graph_context(data, free = TRUE)
  edges = eval_tidy(query, .E())
  if (is_sf(edges) | is_sfc(edges)) {
    edges = nearest_edge_ids(data, edges)
  } else if (is.logical(edges)) {
    edges = which(edges)
  } else if (is.character(edges)) {
    names = edge_attr(data, "name")
    if (is.null(names)) {
      cli_abort(c(
        "Failed to match edge names.",
        "x" = "There is no edge attribute {.field name}.",
        "i" = paste(
          "When querying edges using names it is expected that these",
          "names are stored in a edge attribute named {.field name}"
        )
      ))
    }
    edges = match(edges, names)
  }
  if (! is.integer(edges)) edges = as.integer(edges)
  edges
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
  idlist = st_equals(linestring_boundary_points(edges), nodes)
  idvect = do.call("c", idlist)
  # In most networks the location of a node will be unique.
  # However, this is not a requirement.
  # There may be cases where multiple nodes share the same geometry.
  # Then some more processing is needed to find the correct indices.
  if (length(idvect) != n_edges(x, focused = focused) * 2) {
    n = length(idlist)
    from = idlist[seq(1, n - 1, 2)]
    to = idlist[seq(2, n, 2)]
    pids = mapply(c, from, to, SIMPLIFY = FALSE)
    nids = mapply(c, edges$from, edges$to, SIMPLIFY = FALSE)
    find_indices = function(a, b) {
      ids = a[a %in% b]
      if (length(ids) > 2) b else ids
    }
    idlist = mapply(find_indices, pids, nids, SIMPLIFY = FALSE)
    idvect = do.call("c", idlist)
  }
  if (matrix) t(matrix(idvect, nrow = 2)) else idvect
}

#' Add or drop original feature index columns
#'
#' When morphing networks into a different structure, groups of nodes or edges
#' may be merged into a single feature, or individual nodes or edges may be
#' split into multiple features. In those cases, tidygraph and sfnetworks keep
#' track of the original node and edge indices by creating columns named
#' \code{.tidygraph_node_index} and \code{.tidygraph_edge_index}.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @returns An object of class \code{\link{sfnetwork}}.
#'
#' @name original_ids
#' @importFrom igraph edge_attr<- edge_attr_names vertex_attr<-
#' vertex_attr_names
#' @noRd
add_original_ids = function(x) {
  if (! ".tidygraph_node_index" %in% vertex_attr_names(x)) {
    vertex_attr(x, ".tidygraph_node_index") = seq_len(n_nodes(x))
  }
  if (! ".tidygraph_edge_index" %in% edge_attr_names(x)) {
    edge_attr(x, ".tidygraph_edge_index") = seq_len(n_edges(x))
  }
  x
}

#' @name original_ids
#' @importFrom igraph delete_edge_attr delete_vertex_attr
#' @noRd
drop_original_ids = function(x) {
  x = delete_vertex_attr(x, ".tidygraph_node_index")
  x = delete_edge_attr(x, ".tidygraph_edge_index")
  x
}
