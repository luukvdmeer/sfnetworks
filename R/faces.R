#' Extract the faces of a spatial network
#'
#' The faces of a spatial network are the areas bounded by edges, without any
#' other edge crossing it. A special face is the outer face, which is the area
#' not bounded by any set of edges.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param boundary The boundary used for the outer face, as an object of class
#' \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} containing a single
#' \code{POLYGON} geometry. Note that this boundary should always be larger
#' than the bounding box of the network. If \code{NULL} (the default) the
#' network bounding box extended by 0.1 times its diameter is used.
#'
#' @returns An object of class \code{\link[sf]{sfc}} with \code{POLYGON}
#' geometries, in which each feature represents one face of the network.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#'
#' pts = st_transform(mozart, 3035)
#' net = as_sfnetwork(pts, "delaunay")
#'
#' faces = st_network_faces(net)
#'
#' plot(faces, col = sf.colors(length(faces), categorical = TRUE))
#' plot(net, add = TRUE)
#'
#' par(oldpar)
#'
#' @export
st_network_faces = function(x, boundary = NULL) {
  UseMethod("st_network_faces")
}

#' @importFrom lwgeom st_split
#' @importFrom sf st_as_sfc st_collection_extract st_geometry
#' @export
st_network_faces.sfnetwork = function(x, boundary = NULL) {
  if (is.null(boundary)) boundary = st_as_sfc(extended_network_bbox(x, 0.1))
  splits = st_split(st_geometry(boundary), pull_edge_geom(x))
  st_collection_extract(splits, "POLYGON")
}