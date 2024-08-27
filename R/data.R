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

#' Get column names of the nodes or edges table of of a sfnetwork
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
