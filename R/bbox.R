#' Get the bounding box of a sfnetwork
#'
#' A spatial network specific bounding box extractor, returning the combined
#' bounding box of the nodes and edges in the network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments passed on to \code{\link[sf]{st_bbox}}.
#'
#' @return An object of class \code{bbox}.
#'
#' @details See \code{\link[sf]{st_bbox}} for details.
#'
#' @examples
#' library(sf)
#'
#' node1 = st_point(c(8, 51))
#' node2 = st_point(c(7, 51.5))
#' node3 = st_point(c(8, 52))
#' node4 = st_point(c(9, 51))
#' edges = st_sfc(st_linestring(c(p1, p2, p3)))
#'
#' net = as_sfnetwork(edges)
#'
#' node_bbox = st_bbox(activate(net, "nodes"))
#' edge_bbox = st_bbox(activate(net, "edges"))
#' net_bbox = st_network_bbox(net)
#'
#' ## Plot results.
#' plot(net)
#' plot(st_as_sfc(node_bbox), border = "red", lty = 2, add = TRUE)
#' plot(st_as_sfc(edge_bbox), border = "blue", lty = 2, add = TRUE)
#' plot(net)
#' plot(st_as_sfc(net_bbox), border = "red", lty = 2, add = TRUE)
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