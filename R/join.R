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
#' @note Due to internal rounding of rational numbers, it may occur that the
#' intersection point between a line and a point is not evaluated as 
#' actually intersecting that line by the designated algorithm. Instead, the
#' intersection point lies a tiny-bit away from the edge. Therefore, when any
#' of \code{blend_nodes} or \code{blend_crossings} is set to \code{TRUE}, a
#' small tolerance of 1e-5 meters will be used for blending, instead of a 
#' tolerance of 0.
#'
#' @examples
#' library(sfnetworks)
#' library(sf)
#'
#' # Create two networks.
#' n11 = st_point(c(0,0))
#' n12 = st_point(c(1,1))
#' e1 = st_sfc(st_linestring(c(n11, n12)), crs = 4326)
#'
#' n21 = n12
#' n22 = st_point(c(0,2))
#' e2 = st_sfc(st_linestring(c(n21, n22)), crs = 4326)
#'
#' n31 = n22
#' n32 = st_point(c(-1,1))
#' e3 = st_sfc(st_linestring(c(n31, n32)), crs = 4326)
#'
#' n41 = st_point(c(0.5,0.5))
#' n42 = st_point(c(-1,2))
#' e4 = st_sfc(st_linestring(c(n41, n42)), crs = 4326)
#'
#' n51 = n42
#' n52 = n22
#' e5 = st_sfc(st_linestring(c(n51, n52)), crs = 4326)
#'
#' net1 = as_sfnetwork(st_as_sf(c(e1,e2,e3)))
#' net2 = as_sfnetwork(st_as_sf(c(e4, e5)))
#'
#' # The basic st_network_join does the following:
#' # --> Full spatial join on the nodes, based on st_equals
#' # --> Bind edges
#' # --> Update from and to columns of edges accordingly
#' (j1 = st_network_join(net1, net2))
#'
#' # Set blend_nodes to TRUE. This will:
#' # --> Split edges in x that intersect with a node in y
#' # --> Split edges in y that intersect with a node in x
#' (j2 = st_network_join(net1, net2, blend_nodes = TRUE))
#'
#' # Set blend_crossings to TRUE. This will:
#' # --> Split edges in x where they cross an edge in y
#' # --> Split edges in y where they cross an edge in x
#' (j3 = st_network_join(net1, net2, blend_crossings = TRUE))
#'
#' # Of course we can also blend nodes AND crossings.
#' (j4 = st_network_join(net1, net2, blend_nodes = TRUE, blend_crossings = TRUE))
#'
#' # Notice that nodes are always sorted just as in x.
#' # This takes a little extra processing.
#' # If you dont care about this you can also set sort to FALSE.
#' (j5 = st_network_join(net1, net2, blend_nodes = TRUE, sort = FALSE))
#'
#' # Plot results
#' par(mfrow = c(2,2))
#' plot(net1, col = "blue", cex = 2, main = "Two networks")
#' plot(net2, cex = 1.5, col = "red", add = TRUE)
#' plot(j1, cex = 2, main = "Joined networks...")
#' mtext("6 nodes and 5 edges", side = 1)
#' plot(j2, cex = 2, main = "...with blended nodes")
#' mtext("6 nodes and 6 edges", side = 1)
#' plot(j3, cex = 2, main = "...with blended crossings")
#' mtext("7 nodes and 7 edges", side = 1)
#'
#' @export
st_network_join = function(x, y, blend_nodes = FALSE,
                           blend_crossings = FALSE, sort = TRUE, ...) {
  UseMethod("st_network_join")
}

#' @importFrom tidygraph as_tbl_graph graph_join
#' @importFrom units set_units
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
    if (will_assume_planar(x)) raise_assume_planar("st_network_blend")
    raise_assume_constant("st_network_blend")
    tol = set_units(1e-5, "m")
    if (blend_nodes) {
      x_nodes = node_geom(x)
      y_nodes = node_geom(y)
      x = suppressWarnings(blend_(x, y_nodes, tolerance = tol, sort = sort))
      y = suppressWarnings(blend_(y, x_nodes, tolerance = tol, sort = sort))
    }
    if (blend_crossings) {
      x_edges = edge_geom(x)
      y_edges = edge_geom(y)
      crossings = linestring_crossings(x_edges, y_edges)
      x = suppressWarnings(blend_(x, crossings, tolerance = tol, sort = sort))
      y = suppressWarnings(blend_(y, crossings, tolerance = tol, sort = sort))
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
  g_sfn %preserve_active% x
}
