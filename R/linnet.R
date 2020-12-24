#' Convert an sfnetwork object into linnet format
#'
#' A method to convert \code{sfnetwork} object into
#' \code{\link[spatstat]{linnet}} format and enhance the interoperability
#' between \code{sfnetworks} and \code{spatstat}. Use this method without the
#' .sfnetwork suffix and after loading the \code{spatstat} package.
#'
#' Check also \code{\link[sfnetworks]{as_sfnetwork}} for other methods to
#' convert \code{\link[spatstat]{linnet}} objects into \code{sfnetwork} format.
#'
#' @param X An \code{sfnetwork} object with implicit or explicit edges and
#'   projected CRS.
#' @param ... Arguments passed to \code{\link[spatstat]{linnet}} (only
#'   \code{sparse} and \code{warn}).
#'
#' @return An object of class \code{linnet}.
#'
#' @name as.linnet
#' @export
#' @importFrom sf st_as_sf
#' @examples
#' if (require("spatstat", quietly = TRUE)) {
#'   roxel_sfn = as_sfnetwork(roxel) %>%
#'     sf::st_transform(3035)
#'   (roxel_linnet = as.linnet(roxel_sfn, sparse = TRUE))
#'   plot(roxel_linnet)
#' }
as.linnet.sfnetwork <- function(X, ...) {
  # Check the presence of spatstat
  if (!requireNamespace("spatstat", quietly = TRUE)) {
    stop("Package spatstat required, please install it first.", call. = FALSE)
  }

  # Extract the vertices of the sfnetwork
  X_vertices_ppp <- spatstat::as.ppp(
    sf::st_as_sf(X, "nodes")
  )

  # Extract the edge list
  X_edge_list <- as.matrix(
    (as.data.frame(activate(X, "edges")))[, c("from", "to")]
  )

  # build linnet
  spatstat::linnet(
    vertices = X_vertices_ppp,
    edges = X_edge_list,
    ...
  )
}
