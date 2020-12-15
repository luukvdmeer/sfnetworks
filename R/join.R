#' Join two spatial networks based on equality of node geometries
#'
#' A spatial network specific join function which makes a spatial full join on
#' the geometries of the nodes data, based on the \code{\link[sf]{st_equals}} 
#' spatial predicate. Edge data are combined using a 
#' \code{\link[dplyr]{bind_rows}} semantic, meaning that data are matched by 
#' column name and values are filled with \code{NA} if missing in either of the 
#' networks. The \code{from} and \code{to} columns in the edge data are updated 
#' such that they match the new node indices of the resulting network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y An object of class \code{\link{sfnetwork}}, or directly convertible
#' to it using \code{\link{as_sfnetwork}}.
#'
#' @param ... Arguments passed on to \code{\link[tidygraph]{graph_join}}.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' node1 = st_point(c(0, 0))
#' node2 = st_point(c(1, 0))
#' node3 = st_point(c(1,1))
#' node4 = st_point(c(0,1))
#' edge1 = st_sfc(st_linestring(c(node1, node2)))
#' edge2 = st_sfc(st_linestring(c(node2, node3)))
#' edge3 = st_sfc(st_linestring(c(node3, node4)))
#'
#' net = as_sfnetwork(c(edge1, edge2))
#' other_net = as_sfnetwork(c(edge2, edge3))
#'
#' joined = st_network_join(net, other_net)
#' joined
#'
#' ## Plot results.
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#' plot(net, pch = 15, cex = 2, lwd = 4)
#' plot(other_net, col = "red", pch = 18, cex = 2, lty = 3, lwd = 4, add = TRUE)
#' plot(joined, cex = 2, lwd = 4)
#'
#' @export
st_network_join = function(x, y, ...) {
  UseMethod("st_network_join")
}

#' @importFrom tidygraph as_tbl_graph graph_join
#' @importFrom units set_units
#' @export
st_network_join.sfnetwork = function(x, y, ...) {
  if (! is.sfnetwork(y)) y = as_sfnetwork(y)
  stopifnot(have_equal_crs(x, y))
  stopifnot(have_equal_edge_type(x, y))
  x_geom_colname = node_geom_colname(x)
  y_geom_colname = node_geom_colname(y)
  # Regular graph join based on geometry columns.
  g_tbg = graph_join(
    x = as_tbl_graph(x),
    y = as_tbl_graph(y),
    by = structure(names = x_geom_colname, .Data = y_geom_colname),
    ...
  )
  # Convert back to sfnetwork.
  g_sfn = tbg_to_sfn(g_tbg)
  g_sfn %preserve_active% x
}
