#' Get the bounding box of a spatial networks
#'
#' A spatial network specific bounding box extractor, returning the combined
#' bounding box of the nodes and edges in the network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments passed on to \code{\link[sf]{st_bbox}}.
#'
#' @return An object of class \code{\link[sf:st_bbox]{bbox}}.
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
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#' plot(net, lwd = 2, cex = 4, main = "Element bounding boxes")
#' plot(st_as_sfc(node_bbox), border = "red", lty = 2, lwd = 4, add = TRUE)
#' plot(st_as_sfc(edge_bbox), border = "blue", lty = 2, lwd = 4, add = TRUE)
#' plot(net, lwd = 2, cex = 4, main = "Network bounding box")
#' plot(st_as_sfc(net_bbox), border = "red", lty = 2, lwd = 4, add = TRUE)
#'
#' @export
st_network_bbox = function(x, ...) {
  UseMethod("st_network_bbox")
}

#' @importFrom sf st_bbox st_geometry
#' @export
st_network_bbox.sfnetwork = function(x, ...) {
  # Extract bbox from nodes and edges.
  nodes_bbox = st_bbox(st_geometry(x, "nodes"), ...)
  edges_bbox = st_bbox(st_geometry(x, "edges"), ...)
  # Take most extreme coordinates to form the network bbox.
  x_bbox = nodes_bbox
  x_bbox["xmin"] = min(nodes_bbox["xmin"], edges_bbox["xmin"])
  x_bbox["ymin"] = min(nodes_bbox["ymin"], edges_bbox["ymin"])
  x_bbox["xmax"] = max(nodes_bbox["xmax"], edges_bbox["xmax"])
  x_bbox["ymax"] = max(nodes_bbox["ymax"], edges_bbox["ymax"])
  x_bbox
}