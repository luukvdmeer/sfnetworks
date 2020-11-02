#' Snap geospatial points to their nearest node on the network
#'
#' Wrapper around \code{\link[sf]{st_nearest_feature}} to find the nearest
#' node in a spatial network to a given set of input geometries. In theory, 
#' the input geometries can be of any geometry type, but it is recommended 
#' to only provide geometries of type \code{POINT}, for example by first
#' calculating the centroid of other geometry types.
#'
#' @param x The spatial features to be snapped, either as object of class
#' \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @param y The network to be snapped to as object object of class
#' \code{\link{sfnetwork}}.
#'
#' @param tolerance The tolerance distance to be used. Only features that are
#' at least as close to the network as the tolerance distance will be snapped.
#' For all other features, an empty geometry will be returned. Should be a
#' non-negative number preferably given as an object of class 
#' \code{\link[units]{units}}. Otherwise, it will be assumed that the unit is 
#' meters. If set to \code{Inf} all features will be snapped. Defaults to 
#' \code{Inf}.
#'
#' @param geometries Should the geometries of nearest nodes be returned? If
#' set to \code{FALSE}, indices of nearest nodes are returned instead. 
#' Defaults to \code{TRUE}.
#'
#' @return If \code{geometries=TRUE}: an object of class \code{\link[sf]{sfc}}, 
#' containing the snapped geometries of x. Empty geometries are returned for
#' all features in x that are outside the given tolerance distance.
#' If \code{geometries=FALSE}: an numeric vector containing the indices of the 
#' nearest node to each feature in x. NA's are returned for all geometries in x 
#' outside the given tolerance distance.
#'
#' @details For snapping to the nearest point on the nearest edge instead, 
#' first blend the features in x into the network using \code{\link{st_blend}}, 
#' and then call \code{st_nearest_node}.
#' 
#' @export
st_nearest_node = function(x, y, tolerance = Inf, geometries = TRUE) {
  UseMethod("st_nearest_node")
}

#' @name st_nearest_node
#' @importFrom sf st_crs st_geometry st_is_within_distance st_nearest_feature
#' @importFrom units set_units
#' @export
st_nearest_node.sf = function(x, y, tolerance = Inf, 
                                     geometries = TRUE) {
  stopifnot(have_equal_crs(x, y))
  stopifnot(as.numeric(tolerance) >= 0)
  if (will_assume_planar(x)) raise_assume_planar("st_nearest_node")
  # Extract node geometries from the given network.
  nodes = st_geometry(y, "nodes")
  # For each feature p in x:
  # --> Find the index and geometries of the nearest node q to p.
  Qi = suppressMessages(st_nearest_feature(x, nodes))
  Qg = nodes[Qi]
  # If tolerance is not infinite:
  # --> Return nearest nodes only for features within tolerance.
  # --> Return empty geometries or NA for all other features.
  if (! is.infinite(tolerance)) {
    if (! (inherits(tolerance, "units"))) tolerance = set_units(tolerance, "m")
    within = lengths(st_is_within_distance(x, Qg, tolerance)) > 0
    Qi[!within] = NA
    Qg[!within] = empty_point(crs = st_crs(x))
  }
  if (geometries) Qg else Qi
}

#' @name st_nearest_node
#' @export
st_nearest_node.sfc = function(x, y, tolerance = Inf, 
                                     geometries = TRUE) {
  st_nearest_node.sf(x, y, tolerance, geometries)
}