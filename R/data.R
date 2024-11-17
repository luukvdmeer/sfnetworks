#' Extract the node or edge data from a spatial network
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only features that are in focus be extracted? Defaults
#' to \code{TRUE}. See \code{\link[tidygraph]{focus}} for more information on
#' focused networks.
#'
#' @return For the nodes, always an object of class \code{\link[sf]{sf}}. For
#' the edges, an object of class \code{\link[sf]{sf}} if the edges are
#' spatially explicit, and an object of class \code{\link[tibble]{tibble}}
#' if the edges are spatially implicity and \code{require_sf = FALSE}.
#'
#' @examples
#' net = as_sfnetwork(roxel[1:10, ])
#' node_data(net)
#' edge_data(net)
#'
#' @name data
#' @export
node_data = function(x, focused = TRUE) {
  nodes_as_sf(x, focused = focused)
}

#' @name data
#'
#' @param require_sf Is an \code{\link[sf]{sf}} object required? This will make
#' extraction of edge data fail if the edges are spatially implicit. Defaults
#' to \code{FALSE}.
#'
#' @export
edge_data = function(x, focused = TRUE, require_sf = FALSE) {
  if (require_sf) {
    edges_as_sf(x, focused = focused)
  } else {
    edges_as_spatial_tibble(x, focused = focused)
  }
}

#' Count the number of nodes or edges in a network
#'
#' @param x An object of class \code{\link{sfnetwork}}, or any other network
#' object inheriting from \code{\link[igraph]{igraph}}.
#'
#' @param focused Should only features that are in focus be counted? Defaults
#' to \code{FALSE}. See \code{\link[tidygraph]{focus}} for more information on
#' focused networks.
#'
#' @return An integer.
#'
#' @examples
#' net = as_sfnetwork(roxel)
#' n_nodes(net)
#' n_edges(net)
#'
#' @name n
#' @importFrom igraph vcount
#' @export
n_nodes = function(x, focused = FALSE) {
  if (focused) {
    fids = attr(x, "nodes_focus_index")
    if (is.null(fids)) vcount(x) else length(fids)
  } else {
    vcount(x)
  }
}

#' @name n
#' @importFrom igraph ecount
#' @export
n_edges = function(x, focused = FALSE) {
  if (focused) {
    fids = attr(x, "edges_focus_index")
    if (is.null(fids)) ecount(x) else length(fids)
  } else {
    ecount(x)
  }
}

#' Get the column names of the node or edge data
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param idxs Should the names of the columns storing indices of source and
#' target nodes in the edges table (i.e. the from and to columns) be included?
#' Defaults to \code{FALSE}.
#'
#' @param geom Should the geometry column be included? Defaults to \code{TRUE}.
#'
#' @return A character vector.
#'
#' @name colnames
#' @importFrom igraph vertex_attr_names
#' @noRd
node_colnames = function(x, geom = TRUE) {
  attrs = vertex_attr_names(x)
  if (! geom) {
    attrs = attrs[attrs != node_geom_colname(x)]
  }
  attrs
}

#' @name colnames
#' @importFrom igraph edge_attr_names
#' @noRd
edge_colnames = function(x, idxs = FALSE, geom = TRUE) {
  attrs = edge_attr_names(x)
  if (idxs) {
    attrs = c("from", "to", attrs)
  }
  if (! geom) {
    geom_colname = edge_geom_colname(x)
    if (! is.null(geom_colname)) {
      attrs = attrs[attrs != geom_colname]
    }
  }
  attrs
}

#' Query specific attribute column names in the node or edge data
#'
#' This function is not meant to be called directly, but used inside other
#' functions that accept a attribute column query.
#'
#' @param data An object of class \code{\link{sfnetwork}}.
#'
#' @param query The query that defines for which attribute column names to
#' extract, defused into a \code{\link[dplyr:topic-quosure]{quosure}}. The
#' query is evaluated as a \code{\link[dplyr]{dplyr_tidy_select}} argument.
#'
#' @note The geometry column and any index column (e.g. from, to, or the
#' tidygraph index columns added during morphing) are not considered attribute
#' columns.
#'
#' @returns A character vector of queried attribute column names.
#'
#' @name evaluate_attribute_query
#' @importFrom sf st_drop_geometry
#' @importFrom tidyselect eval_select
#' @noRd
evaluate_node_attribute_query = function(x, query) {
  nodes = st_drop_geometry(nodes_as_sf(x))
  exclude = c(
    ".tidygraph_node_index",
    ".tidygraph_edge_index",
    ".tidygraph_index",
    ".tbl_graph_index",
    ".sfnetwork_index"
  )
  node_attrs = nodes[, !(names(nodes) %in% exclude)]
  names(node_attrs)[eval_select(query, node_attrs)]
}

#' @name evaluate_attribute_query
#' @importFrom sf st_drop_geometry
#' @importFrom tidyselect eval_select
#' @noRd
evaluate_edge_attribute_query = function(x, query) {
  edges = st_drop_geometry(edge_data(x))
  exclude = c(
    "from",
    "to",
    ".tidygraph_node_index",
    ".tidygraph_edge_index",
    ".tidygraph_index",
    ".tbl_graph_index",
    ".sfnetwork_index"
  )
  edge_attrs = edges[, !(names(edges) %in% exclude)]
  names(edge_attrs)[eval_select(query, edge_attrs)]
}

#' Set or replace node or edge data in a spatial network
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param value A table in which each column is an attribute to be set. For the
#' nodes this table has to be of class \code{\link[sf]{sf}}. For the edges it
#' can also be a \code{\link{data.frame}} or \code{\link[tibble]{tibble}}.
#'
#' @return An object of class \code{\link{sfnetwork}} with updated attributes.
#'
#' @details This function is only meant to update attributes of nodes or edges
#' and not to change the graph morphology. This means that when setting edge
#' data the columns storing indices of start and end nodes (i.e. the from and
#' to columns) should not be included. The geometry column, however, should be.
#'
#' @name set_data
#' @importFrom igraph vertex_attr<-
#' @noRd
`node_data<-` = function(x, value) {
  vertex_attr(x) = as.list(value)
  x
}

#' @name set_data
#' @importFrom igraph edge_attr<-
#' @noRd
`edge_data<-` = function(x, value) {
  edge_attr(x) = as.list(value[, !names(value) %in% c("from", "to")])
  x
}

#' Add original data to merged features
#'
#' When morphing networks into a different structure, groups of nodes or edges
#' may be merged into a single feature. In those cases, there is always the
#' option to store the data of the original features in a column named
#' \code{.orig_data}.
#'
#' @param x The new network as object of class \code{\link{sfnetwork}}.
#'
#' @param orig The original features as object of class \code{\link[sf]{sf}}.
#'
#' @return The new network with the original data stored in a column named
#' \code{.orig_data}.
#'
#' @name add_original_data
#' @noRd
add_original_node_data = function(x, orig) {
  # Store the original node data in a .orig_data column.
  new_nodes = node_data(x, focused = FALSE)
  orig$.tidygraph_node_index = NULL
  copy_data = function(i) orig[i, , drop = FALSE]
  new_nodes$.orig_data = lapply(new_nodes$.tidygraph_node_index, copy_data)
  node_data(x) = new_nodes
  x
}

#' @name add_original_data
#' @noRd
add_original_edge_data = function(x, orig) {
  # Store the original edge data in a .orig_data column.
  new_edges = edge_data(x, focused = FALSE)
  orig$.tidygraph_edge_index = NULL
  copy_data = function(i) orig[i, , drop = FALSE]
  new_edges$.orig_data = lapply(new_edges$.tidygraph_edge_index, copy_data)
  edge_data(x) = new_edges
  x
}
