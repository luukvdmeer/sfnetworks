#' Join two spatial networks based on equality of node geometries
#'
#' A spatial network specific join function which makes a spatial full join on 
#' the geometries of the nodes data, based on the \code{st_equals} spatial 
#' predicate. Edge data are combined using a \code{bind_rows} semantic, meaning 
#' that data are matched by column name and values are filled with `NA` if 
#' missing in either of the networks. The \code{from} and \code{to} columns in 
#' the edge data are updated such that they match the new node indices of the 
#' resulting network. If requested, edges can be splitted at locations where 
#' they either intersect with nodes of the other network, or get crossed by 
#' edges of the other network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y An object of class \code{\link{sfnetwork}}, or directly convertible 
#' to it using \code{\link{as_sfnetwork}}.
#'
#' @param blend_nodes Should nodes be blended? If \code{TRUE}, edges in x will
#' be splitted at each location where they intersect a node in y, and edges in
#' y will be splitted at each location where they intersect a node in x. This
#' requires edges in both networks to be spatially explicit. Defaults to 
#' \code{FALSE}.
#'
#' @param blend_crossings Should edge crossings be blended? If \code{TRUE},
#' edges in x will be splitted at each location where they cross an edge in y, 
#' and edges in y will be splitted at each location where they cross and edge
#' in x. This requires edges in both networks to be spatially explicit.
#' Defaults to \code{FALSE}.
#'
#' @param sort If any of the blend options was set to \code{TRUE}, should the
#' nodes in the resulting network be sorted in the same order as those in x, 
#' followed by the new nodes joined in from y? Defaults to \code{TRUE}. If set 
#' to \code{FALSE}, node order might be changed. However, sorting
#' might influence performance.
#'
#' @param ... Arguments passed on to \code{\link[tidygraph]{graph_join}}.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @export
#'
st_network_join = function(x, y, blend_nodes = FALSE, 
                           blend_crossings = FALSE, sort = TRUE, ...) {
  UseMethod("st_network_join")
}

#' @importFrom sf st_geometry
#' @importFrom tidygraph as_tbl_graph graph_join
#' @export
st_network_join.sfnetwork = function(x, y, blend_nodes = FALSE, 
                           blend_crossings = FALSE, sort = TRUE, ...) {
  if (! is.sfnetwork(y)) y = as_sfnetwork(y)
  stopifnot(have_equal_crs(x, y))
  stopifnot(have_equal_edge_type(x, y))
  x_geom_colname = node_geom_colname(x)
  y_geom_colname = node_geom_colname(y)
  # Blend if requested.
  if (any(c(blend_nodes, blend_crossings))) {
    if (will_assume_planar(x)) raise_assume_planar("st_blend")
    raise_assume_constant("st_blend")
    if (blend_nodes) {
      x_nodes = st_geometry(x, "nodes")
      y_nodes = st_geometry(y, "nodes")
      x = suppressWarnings(blend_(x, y_nodes, tolerance = 0, sort = sort))
      y = suppressWarnings(blend_(y, x_nodes, tolerance = 0, sort = sort))
    }
    if (blend_crossings) {
      x_edges = st_geometry(x, "edges")
      y_edges = st_geometry(y, "edges")
      crossings = linestring_crossings(x_edges, y_edges)
      x = suppressWarnings(blend_(x, crossings, tolerance = 0, sort = sort))
      y = suppressWarnings(blend_(y, crossings, tolerance = 0, sort = sort))
    }
  }
  # Regular graph join based on geometry columns.
  g_tbg = graph_join(
    x = as_tbl_graph(x),
    y = as_tbl_graph(y),
    by = structure(names = x_geom_colname, .Data = y_geom_colname),
    ...
  )
  # Convert back to sfnetwork.
  g_sfn = tbg_to_sfn(g_tbg)
  if (has_duplicates(st_geometry(g_sfn, "nodes"))) raise_multiple_matches()
  g_sfn %preserve_active% x
}