#' Get the bounding box of a spatial network
#'
#' A spatial network specific bounding box extractor, returning the combined
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
#' library(sf)
#'
#' # Create a network.
#' node1 = st_point(c(8, 51))
#' node2 = st_point(c(7, 51.5))
#' node3 = st_point(c(8, 52))
#' node4 = st_point(c(9, 51))
#' edge1 = st_sfc(st_linestring(c(node1, node2, node3)))
#'
#' nodes = st_as_sf(c(st_sfc(node1), st_sfc(node3), st_sfc(node4)))
#' edges = st_as_sf(edge1)
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
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#' plot(net, lwd = 2, cex = 4, main = "Element bounding boxes")
#' plot(st_as_sfc(node_bbox), border = "red", lty = 2, lwd = 4, add = TRUE)
#' plot(st_as_sfc(edge_bbox), border = "blue", lty = 2, lwd = 4, add = TRUE)
#' plot(net, lwd = 2, cex = 4, main = "Network bounding box")
#' plot(st_as_sfc(net_bbox), border = "red", lty = 2, lwd = 4, add = TRUE)
#' par(oldpar)
#'
#' @export
st_network_bbox = function(x, ...) {
  UseMethod("st_network_bbox")
}

#' @importFrom sf st_bbox st_geometry
#' @export
st_network_bbox.sfnetwork = function(x, ...) {
  # Extract bbox from nodes and edges.
  nodes_bbox = st_bbox(pull_node_geom(x), ...)
  edges_bbox = st_bbox(pull_edge_geom(x), ...)
  # Take most extreme coordinates to form the network bbox.
  merge_bboxes(nodes_bbox, edges_bbox)
}


