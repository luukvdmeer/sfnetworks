#' Join two spatial networks based on equality of node geometries
#'
#' A spatial network specific join function which makes a spatial full join on
#' the geometries of the nodes data, based on the \code{\link[sf]{st_equals}}
#' spatial predicate. Edge data are combined using a
#' \code{\link[dplyr]{bind_rows}} semantic, meaning that data are matched by
#' column name and values are filled with \code{NA} if missing in either of
#' the networks. The \code{from} and \code{to} columns in the edge data are
#' updated such that they match the new node indices of the resulting network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y An object of class \code{\link{sfnetwork}}, or directly convertible
#' to it using \code{\link{as_sfnetwork}}.
#'
#' @param ... Arguments passed on to \code{\link[tidygraph]{graph_join}}.
#'
#' @return The joined networks as an object of class \code{\link{sfnetwork}}.
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
#' net1 = as_sfnetwork(c(edge1, edge2))
#' net2 = as_sfnetwork(c(edge2, edge3))
#'
#' joined = st_network_join(net1, net2)
#' joined
#'
#' ## Plot results.
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#' plot(net1, pch = 15, cex = 2, lwd = 4)
#' plot(net2, col = "red", pch = 18, cex = 2, lty = 3, lwd = 4, add = TRUE)
#' plot(joined, cex = 2, lwd = 4)
#' par(oldpar)
#'
#' @export
st_network_join = function(x, y, ...) {
  UseMethod("st_network_join")
}

#' @importFrom cli cli_abort
#' @importFrom tidygraph unfocus
#' @export
st_network_join.sfnetwork = function(x, y, ...) {
  if (! is_sfnetwork(y)) y = as_sfnetwork(y)
  x = unfocus(x)
  y = unfocus(y)
  if (! have_equal_edge_type(x, y)) {
    cli_abort(c(
      paste(
        "{.arg x} and {.arg y} should have the same type of edges",
        "(spatially explicit or spatially implicit)"
      ),
      "i" = "Call {.fn sfnetworks::to_spatial_explicit} to explicitize edges.",
      "i" = "Call {.fn sf::st_drop_geometry} to drop edge geometries."
    ))
  }
  if (! have_equal_crs(x, y)) {
    cli_abort(c(
      "{.arg x} and {.arg y} should have the same CRS.",
      "i" = "Call {.fn sf::st_transform} to transform to a different CRS."
    ))
  }
  spatial_join_network(x, y, ...)
}

#' @importFrom tidygraph as_tbl_graph graph_join
spatial_join_network = function(x, y, ...) {
  # Retrieve names of node geometry columns of x and y.
  x_geom_colname = node_geom_colname(x)
  y_geom_colname = node_geom_colname(y)
  # Regular graph join based on geometry columns.
  x_new = graph_join(
    x = as_tbl_graph(x),
    y = as_tbl_graph(y),
    by = structure(names = x_geom_colname, .Data = y_geom_colname),
    ...
  )
  x_new %preserve_network_attrs% x
}
