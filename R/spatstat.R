#' Convert a sfnetwork into a linnet
#'
#' A method to convert an object of class \code{\link{sfnetwork}} into
#' \code{\link[spatstat.linnet]{linnet}} format and enhance the
#' interoperability between \code{sfnetworks} and \code{spatstat}. Use
#' this method without the .sfnetwork suffix and after loading the
#' \pkg{spatstat} package.
#'
#' @param X An object of class \code{\link{sfnetwork}} with a projected CRS.
#'
#' @param ... Arguments passed to \code{\link[spatstat.linnet]{linnet}}.
#'
#' @return An object of class \code{\link[spatstat.linnet]{linnet}}.
#'
#' @seealso \code{\link{as_sfnetwork}} to convert objects of class
#' \code{\link[spatstat.linnet]{linnet}} into objects of class
#' \code{\link{sfnetwork}}.
#'
#' @importFrom rlang check_installed is_installed
#' @name as.linnet
as.linnet.sfnetwork = function(X, ...) {
  # Check the presence and the version of spatstat.geom and spatstat.linnet
  check_installed("spatstat.geom")
  check_installed("spatstat.linnet")
  check_installed("sf (>= 1.0)")
  if (is_installed("spatstat")) check_installed("spatstat (>= 2.0)")
  # Convert nodes to ppp.
  V = spatstat.geom::as.ppp(pull_node_geom(X))
  # Extract the edge list.
  E = as.matrix(edges_as_regular_tibble(X)[, c("from", "to")])
  # Build linnet.
  spatstat.linnet::linnet(vertices = V, edges = E, ...)
}
