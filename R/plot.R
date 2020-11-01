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
#'
#' net = as_sfnetwork(roxel)
#' plot(net)
#'
#' # When lines are spatially implicit
#' net = as_sfnetwork(roxel, edges_as_lines = F)
#' plot(net)
#'
#' # Changing plot parameters like `col` will affect
#' both edges and nodes, while e.g. `lwd` only affects the edges
#' and `pch` and `cex` the nodes.
#'
#' plot(net, col = 'blue', pch = 18, lwd = 1, cex = 2)
#'
#' @importFrom graphics plot
#' @importFrom sf st_geometry
#' @export
plot.sfnetwork = function(x, draw_lines = TRUE, ...) {
  dots = list(...)
  # Get geometries of nodes.
  nsf = st_geometry(x, "nodes")
  # Combine node geometries with edge geometries if needed.
  use_edges = TRUE
  if (! has_spatially_explicit_edges(x)) {
    if (draw_lines) {
      x = explicitize_edges(x)
    } else {
      use_edges = FALSE
    }
  }
  dots$x = if (use_edges) c(nsf, st_geometry(x, "edges")) else nsf
  # Use pch of 20 by default.
  pch_missing = is.null(dots$pch)
  dots$pch = if (pch_missing) 20 else dots$pch
  # Plot.
  do.call(plot, dots)
}

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Autoplot sfnetwork object
#'
#' Autoplot an object of class \code{\link{sfnetwork}} as a ggplot object.
#'
#' @param x Object of class \code{\link{sfnetwork}}.
#'
#' @details This is a basic ggploting functionality, for a quick assessment of
#' the network but on a \code{ggplot2} format.
#'
#' @examples
#'
#' net = as_sfnetwork(roxel)
#'
#' # Quick overview of the network in `ggplot2` style
#' autoplot(net)
#'
#' # Other `ggplot2` elements can be added
#' points = net %>%
#'   st_bbox() %>%
#'   st_as_sfc() %>%
#'   st_sample(10, type = 'random') %>%
#'   st_set_crs(4326) %>%
#'   st_cast('POINT')
#'
#' autoplot(net) +
#'    # The theme can be customized
#'    ggplot2::theme_minimal() +
#'    # Labels cna be added
#'    ggplot2::labs(title = 'Nice ggplot') +
#'    # And extra `geom_sf` layers can be included
#'    ggplot2::geom_sf(data = points, color = 'red', size = 2)
#'
#' @importFrom sf st_as_sf
#' @export
autoplot.sfnetwork = function(x) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('The "ggplot2" package is needed for this functionality to work', call. = FALSE)
  }

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf::st_as_sf(x, 'nodes')) +
    ggplot2::geom_sf(data = sf::st_as_sf(x, 'edges'))
}
