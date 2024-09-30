#' Compute the bounding box of a spatial network
#'
#' A spatial network specific bounding box creator, returning the combined
#' bounding box of the nodes and edges in the network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments passed on to \code{\link[sf]{st_bbox}}.
#'
#' @return The bounding box of the network as an object of class
#' \code{\link[sf:st_bbox]{bbox}}.
#'
#' @details See \code{\link[sf]{st_bbox}} for details.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#'
#' # Create a network.
#' n1 = st_point(c(8, 51))
#' n2 = st_point(c(7, 51.5))
#' n3 = st_point(c(8, 52))
#' n4 = st_point(c(9, 51))
#' e1 = st_sfc(st_linestring(c(n1, n2, n3)))
#'
#' nodes = st_as_sf(c(st_sfc(n1), st_sfc(n3), st_sfc(n4)))
#'
#' edges = st_as_sf(e1)
#' edges$from = 1
#' edges$to = 2
#'
#' net = sfnetwork(nodes, edges)
#'
#' # Create bounding boxes for nodes, edges and the whole network.
#' node_bbox = st_bbox(activate(net, "nodes"))
#' node_bbox
#' edge_bbox = st_bbox(activate(net, "edges"))
#' edge_bbox
#' net_bbox = st_network_bbox(net)
#' net_bbox
#'
#' # Plot.
#' plot(net, lwd = 2, cex = 4, main = "Element bounding boxes")
#' plot(st_as_sfc(node_bbox), border = "orange", lty = 2, lwd = 4, add = TRUE)
#' plot(st_as_sfc(edge_bbox), border = "skyblue", lty = 2, lwd = 4, add = TRUE)
#'
#' plot(net, lwd = 2, cex = 4, main = "Network bounding box")
#' plot(st_as_sfc(net_bbox), border = "orange", lty = 2, lwd = 4, add = TRUE)
#'
#' par(oldpar)
#'
#' @export
st_network_bbox = function(x, ...) {
  UseMethod("st_network_bbox")
}

#' @importFrom sf st_bbox
#' @export
st_network_bbox.sfnetwork = function(x, ...) {
  # If the network is spatially implicit:
  # --> The network bbox is equal to the node bbox.
  # If the network is spatially explicit:
  # --> Get most extreme coordinates among node and edge bboxes.
  nodes_bbox = st_bbox(pull_node_geom(x), ...)
  if (has_explicit_edges(x)) {
    edges_bbox = st_bbox(pull_edge_geom(x), ...)
    net_bbox = merge_bboxes(nodes_bbox, edges_bbox)
  } else {
    net_bbox = nodes_bbox
  }
  net_bbox
}

#' @importFrom sf st_as_sfc st_bbox st_buffer st_crs st_distance st_point st_sfc
extended_network_bbox = function(x, ratio = 0.1) {
  crs = st_crs(x)
  bbox = st_network_bbox(x)
  lowleft = st_sfc(st_point(c(bbox["xmin"], bbox["ymin"])), crs = crs)
  upright = st_sfc(st_point(c(bbox["xmax"], bbox["ymax"])), crs = crs)
  diameter = st_distance(lowleft, upright)
  buffer = st_buffer(st_as_sfc(bbox), dist = diameter * ratio)
  st_bbox(buffer)
}
