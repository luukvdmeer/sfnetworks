#' Extract the nearest nodes or edges to given spatial features
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y Spatial features as object of class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @param focused Should only features that are in focus be extracted? Defaults
#' to \code{TRUE}. See \code{\link[tidygraph]{focus}} for more information on
#' focused networks.
#'
#' @details To determine the nearest node or edge to each feature in \code{y}
#' the function \code{\link[sf]{st_nearest_feature}} is used. When extracting
#' nearest edges, spatially explicit edges are required, i.e. the edges table
#' should have a geometry column.
#'
#' @return An object of class \code{\link[sf]{sf}} with each row containing
#' the nearest node or edge to the corresponding spatial features in \code{y}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel)
#' pts = st_sample(st_bbox(roxel), 6)
#'
#' nodes = nearest_nodes(net, pts)
#' edges = nearest_edges(net, pts)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#'
#' plot(net, main = "Nearest nodes")
#' plot(pts, cex = 2, col = "red", pch = 20, add = TRUE)
#' plot(st_geometry(nodes), cex = 2, col = "orange", pch = 20, add = TRUE)
#'
#' plot(net, main = "Nearest edges")
#' plot(pts, cex = 2, col = "red", pch = 20, add = TRUE)
#' plot(st_geometry(edges), lwd = 2, col = "orange", pch = 20, add = TRUE)
#'
#' par(oldpar)
#'
#' @name nearest
#' @importFrom sf st_geometry st_nearest_feature
#' @export
nearest_nodes = function(x, y, focused = TRUE) {
  nodes = nodes_as_sf(x, focused = focused)
  nodes[st_nearest_feature(st_geometry(y), nodes), ]
}

#' @name nearest
#' @importFrom sf st_geometry st_nearest_feature
#' @export
nearest_edges = function(x, y, focused = TRUE) {
  edges = edges_as_sf(x, focused = focused)
  edges[st_nearest_feature(st_geometry(y), edges), ]
}

#' Extract the indices of nearest nodes or edges to given spatial features
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y Spatial features as object of class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @param focused Should only the indices of features that are in focus be
#' extracted? Defaults to \code{TRUE}. See \code{\link[tidygraph]{focus}} for
#' more information on focused networks.
#'
#' @details To determine the nearest node or edge to each feature in \code{y}
#' the function \code{\link[sf]{st_nearest_feature}} is used. When extracting
#' nearest edges, spatially explicit edges are required, i.e. the edges table
#' should have a geometry column.
#'
#' @return An integer vector with each element containing the index of the
#' nearest node or edge to the corresponding spatial features in \code{y}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel)
#' pts = st_sample(st_bbox(roxel), 6)
#'
#' nearest_node_ids(net, pts)
#' nearest_edge_ids(net, pts)
#'
#' @name nearest_ids
#' @importFrom sf st_geometry st_nearest_feature
#' @export
nearest_node_ids = function(x, y, focused = TRUE) {
  st_nearest_feature(st_geometry(y), pull_node_geom(x, focused = focused))
}

#' @name nearest_ids
#' @importFrom sf st_geometry st_nearest_feature
#' @export
nearest_edge_ids = function(x, y, focused = TRUE) {
  st_nearest_feature(st_geometry(y), pull_edge_geom(x, focused = focused))
}
