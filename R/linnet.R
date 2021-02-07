#' Convert a sfnetwork into a linnet
#'
#' A method to convert an object of class \code{\link{sfnetwork}} into
#' \code{\link[spatstat]{linnet}} format and enhance the interoperability
#' between \code{sfnetworks} and \code{spatstat}. Use this method without the
#' .sfnetwork suffix and after loading the \code{spatstat} package.
#'
#' @param X An object of class \code{\link{sfnetwork}} with a projected CRS.
#'
#' @param ... Arguments passed to \code{\link[spatstat]{linnet}}.
#'
#' @return An object of class \code{\link[spatstat]{linnet}}.
#'
#' @seealso \code{\link{as_sfnetwork}} to convert objects of class
#' \code{\link[spatstat]{linnet}} into objects of class 
#' \code{\link{sfnetwork}}.
#'
#' @name as.linnet
as.linnet.sfnetwork <- function(X, ...) {
  # Check the presence of spatstat.
  if (!requireNamespace("spatstat", quietly = TRUE)) {
    stop("Package spatstat required, please install it first.", call. = FALSE)
  }
  # Extract the vertices of the sfnetwork.
  X_vertices_ppp = spatstat::as.ppp(nodes_as_sf(X))
  # Extract the edge list.
  X_edge_list = as.matrix(
    (as.data.frame(activate(X, "edges")))[, c("from", "to")]
  )
  # Build linnet.
  spatstat::linnet(
    vertices = X_vertices_ppp,
    edges = X_edge_list,
    ...
  )
}
