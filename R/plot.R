#' Plot sfnetwork object
#'
#' Plot the geometries of an object of class \code{\link{sfnetwork}}.
#'
#' @param x Object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments passed on to \code{\link[sf]{plot}}
#'
#' @importFrom graphics plot
#' @importFrom sf st_geometry
#' @export
plot.sfnetwork = function(x, ...) {
  dots = list(...)
  nsf = sf::st_geometry(activate(x, "nodes")) # Nodes.
  if (! has_spatially_explicit_edges(x)) {
    x = to_spatially_explicit_edges(x) # Draw edges if not spatially explicit.
  }
  esf = sf::st_geometry(activate(x, "edges")) # Edges.
  gsf = c(nsf, esf) # Full graph.
  dots$x = gsf
  # Use pch of 20 by default.
  pch_missing = is.null(dots$pch)
  dots$pch = if (pch_missing) 20 else dots$pch
  do.call(plot, dots)
}
