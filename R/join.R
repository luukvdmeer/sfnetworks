#' Join sfnetworks based on equality of node geometries
#'
#' This spatial network specific join function makes a spatial full join on the 
#' geometries of the nodes data and updates the edges in y such that they
#' match the new indices of the nodes in the resulting network. Edge data is 
#' combined using a \code{bind_rows} semantic, meaning that data are matched 
#' by column name and filled with `NA` if it is missing in either of the
#' networks. Also, this means that two edges with equal geometries will both
#' be present in the resulting network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments passed on to \code{\link[tidygraph]{graph_join}}.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @importFrom tidygraph graph_join
#' @export
st_network_join = function(x, y, ...) {
  stopifnot(is.sfnetwork(x) & is.sfnetwork(y))
  stopifnot(have_equal_crs(x, y))
  x_geom_colname = node_geom_colname(x)
  y_geom_colname = node_geom_colname(y)
  # Regular graph join based on geometry columns.
  g_tbg = tidygraph::graph_join(
    x = as_tbl_graph(x),
    y = as_tbl_graph(y),
    by = structure(names = x_geom_colname, .Data = y_geom_colname),
    ...
  )
  # Convert back to sfnetwork.
  g_sfn = tbg_to_sfn(g_tbg)
  if (has_duplicates(st_geometry(g_sfn, "nodes"))) {
    stop("One or more nodes have multiple matches", call. = FALSE)
  }
  # Update agr.
  node_agr(g_sfn) = structure(
    concat_agr(node_agr(x), node_agr(y)),
    names = node_spatial_attribute_names(g_sfn)
  )
  edge_agr(g_sfn) = empty_edge_agr(g_sfn)
  g_sfn %preserve_active% x
}