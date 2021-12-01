#' Plot sfnetwork geometries
#'
#' Plot the geometries of an object of class \code{\link{sfnetwork}}.
#'
#' @param x Object of class \code{\link{sfnetwork}}.
#'
#' @param draw_lines If the edges of the network are spatially implicit, should
#' straight lines be drawn between connected nodes? Defaults to \code{TRUE}.
#' Ignored when the edges of the network are spatially explicit.
#'
#' @param ... Arguments passed on to \code{\link[sf:plot]{plot.sf}}
#'
#' @details This is a basic plotting functionality. For more advanced plotting,
#' it is recommended to extract the nodes and edges from the network, and plot
#' them separately with one of the many available spatial plotting functions
#' as can be found in \code{sf}, \code{tmap}, \code{ggplot2}, \code{ggspatial},
#' and others.
#'
#' @examples
#' ## This line sets roxel CRS again
#' ## to comply with different PROJ versions
#' sf::st_crs(roxel) = "EPSG:4326"
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,1))
#' net = as_sfnetwork(roxel)
#' plot(net)
#'
#' # When lines are spatially implicit.
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#' net = as_sfnetwork(roxel, edges_as_lines = FALSE)
#' plot(net)
#' plot(net, draw_lines = FALSE)
#'
#' # Changing default settings.
#' par(mar = c(1,1,1,1), mfrow = c(1,1))
#' plot(net, col = 'blue', pch = 18, lwd = 1, cex = 2)
#'
#' # Add grid and axis
#' par(mar = c(2.5,2.5,1,1))
#' plot(net, graticule = TRUE, axes = TRUE)
#'
#' par(oldpar)
#'
#' @importFrom graphics plot
#' @importFrom sf st_geometry
#' @export
plot.sfnetwork = function(x, draw_lines = TRUE, ...) {
  dots = list(...)
  # Get geometries of nodes.
  nsf = node_geom(x)
  # Combine node geometries with edge geometries if needed.
  use_edges = TRUE
  if (! has_spatially_explicit_edges(x)) {
    if (draw_lines) {
      x = explicitize_edges(x)
    } else {
      use_edges = FALSE
    }
  }
  dots$x = if (use_edges) c(nsf, edge_geom(x)) else nsf
  # Use pch of 20 by default.
  pch_missing = is.null(dots$pch)
  dots$pch = if (pch_missing) 20 else dots$pch
  # Plot.
  do.call(plot, dots)
}

#' Plot sfnetwork geometries with ggplot2
#'
#' Plot the geometries of an object of class \code{\link{sfnetwork}}
#' automatically as a \code{\link[ggplot2]{ggplot}} object. Use this method
#' without the .sfnetwork suffix and after loading the \code{ggplot2} package.
#'
#' @param object An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Ignored.
#'
#' @return An object of class \code{\link[ggplot2]{ggplot}}.
#'
#' @details See \code{\link[ggplot2]{autoplot}}.
#'
#' @name autoplot
autoplot.sfnetwork = function(object, ...) {
  g = ggplot2::ggplot() + ggplot2::geom_sf(data = nodes_as_sf(object))
  if (has_spatially_explicit_edges(object)) {
    g + ggplot2::geom_sf(data = edges_as_sf(object))
  } else {
    message("Spatially implicit edges are drawn as lines", call. = FALSE)
    object = explicitize_edges(object)
    g + ggplot2::geom_sf(data = edges_as_sf(object))
  }
}
