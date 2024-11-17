#' Join two spatial networks based on equality of node geometries
#'
#' A spatial network specific join function which makes a spatial full join on
#' the geometries of the nodes data. Edge data are combined using a
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
#' @note By default sfnetworks rounds coordinates to 12 decimal places to
#' determine spatial equality. You can influence this behavior by explicitly
#' setting the precision of the networks using
#' \code{\link[sf]{st_set_precision}}.
#'
#' @return The joined networks as an object of class \code{\link{sfnetwork}}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#'
#' # Create two networks.
#' n1 = st_point(c(0, 0))
#' n2 = st_point(c(1, 0))
#' n3 = st_point(c(1,1))
#' n4 = st_point(c(0,1))
#'
#' e1 = st_sfc(st_linestring(c(n1, n2)))
#' e2 = st_sfc(st_linestring(c(n2, n3)))
#' e3 = st_sfc(st_linestring(c(n3, n4)))
#'
#' neta = as_sfnetwork(c(e1, e2))
#' netb = as_sfnetwork(c(e2, e3))
#'
#' # Join the networks based on spatial equality of nodes.
#' net = st_network_join(neta, netb)
#' net
#'
#' # Plot.
#' plot(neta, pch = 15, cex = 2, lwd = 4)
#' plot(netb, col = "orange", pch = 18, cex = 2, lty = 3, lwd = 4, add = TRUE)
#
#' plot(net, cex = 2, lwd = 4)
#'
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

#' @importFrom dplyr join_by
#' @importFrom igraph delete_vertex_attr vertex_attr vertex_attr<-
#' vertex_attr_names
#' @importFrom tidygraph as_tbl_graph graph_join
spatial_join_network = function(x, y, ...) {
  # Extract node geometry column names from x and y.
  x_geomcol = node_geom_colname(x)
  y_geomcol = node_geom_colname(y)
  # Assess which node geometries in the union of x and y are equal.
  # This will create a vertex of unique node indices in the union of x and y.
  N_x = vertex_attr(x, x_geomcol)
  N_y = vertex_attr(y, y_geomcol)
  N = c(N_x, N_y)
  uid = st_match_points(N)
  # Store the unique node indices as node attributes in both x and y.
  if (".sfnetwork_index" %in% c(vertex_attr_names(x), vertex_attr_names(y))) {
    raise_reserved_attr(".sfnetwork_index")
  }
  vertex_attr(x, ".sfnetwork_index") = uid[1:length(N_x)]
  vertex_attr(y, ".sfnetwork_index") = uid[(length(N_x) + 1):length(uid)]
  # Join x and y based on the unique node indices using tidygraphs graph_join.
  # Perform this join without the geometry column.
  # Otherwise the geometry columns of x and y are seen as regular attributes.
  # Meaning that they get stored separately in the joined network.
  x_tbg = as_tbl_graph(delete_vertex_attr(x, x_geomcol))
  y_tbg = as_tbl_graph(delete_vertex_attr(y, y_geomcol))
  x_new = graph_join(x_tbg, y_tbg, by = join_by(.sfnetwork_index), ...)
  # Add the corresponding node geometries to the joined network.
  N_new = N[!duplicated(uid)][vertex_attr(x_new, ".sfnetwork_index")]
  vertex_attr(x_new, x_geomcol) = N_new
  # Return after removing the unique node index attribute.
  delete_vertex_attr(x_new, ".sfnetwork_index") %preserve_network_attrs% x
}
