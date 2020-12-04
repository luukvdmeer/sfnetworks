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