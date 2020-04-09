#' Plot sfnetwork object
#'
#' Plot spatially explicit graph components (nodes and edges) of an sfnetwork
#' on a map.
#'
#' @param x Object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments passed on to \code{\link[sf]{plot}}
#'
#' @importFrom graphics plot
#' @importFrom sf st_as_sf st_geometry
#' @export
plot.sfnetwork = function(x, ...) {
  dots = list(...)
  nsf = sf::st_geometry(sf::st_as_sf(activate(x, "nodes"))) # Nodes
  # Bind nodes and edges into one sf object if edges are spatially explicit.
  if (has_spatially_explicit_edges(x)) {
    esf = sf::st_geometry(sf::st_as_sf(activate(x, "edges"))) # Edges
    gsf = c(nsf, esf) # Full graph
    dots$x = gsf
  } else {
    dots$x = nsf
  }
  # Use pch of 20 by default.
  pch_missing = is.null(dots$pch)
  dots$pch = if (pch_missing) 20 else dots$pch
  do.call(plot, dots)
}
