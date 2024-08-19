#' Plot the geometries of a sfnetwork
#'
#' Plot the geometries of an object of class \code{\link{sfnetwork}}.
#'
#' @param x Object of class \code{\link{sfnetwork}}.
#'
#' @param draw_lines If the edges of the network are spatially implicit, should
#' straight lines be drawn between connected nodes? Defaults to \code{TRUE}.
#' Ignored when the edges of the network are spatially explicit.
#'
#' @param node_args A named list of arguments that will be passed on to
#' \code{\link[sf:plot]{plot.sf}} only for plotting the nodes.
#'
#' @param edge_args A named list of arguments that will be passed on to
#' \code{\link[sf:plot]{plot.sf}} only for plotting the edges.
#'
#' @param ... Arguments passed on to \code{\link[sf:plot]{plot.sf}} that will
#' apply to the plot as a whole.
#'
#' @details Arguments passed to \code{...} will be used both for plotting the
#' nodes and for plotting the edges. Edges are always plotted first. Arguments
#' specified in \code{node_args} and \code{edge_args} should not be specified
#' in \code{...} as well, this will result in an error.
#'
#' @return Invisible.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,1))
#' net = as_sfnetwork(roxel)
#' plot(net)
#'
#' # When edges are spatially implicit.
#' # By default straight lines will be drawn between connected nodes.
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#' inet = st_drop_geometry(activate(net, "edges"))
#' plot(inet)
#' plot(inet, draw_lines = FALSE)
#'
#' # Changing plot settings.
#' par(mar = c(1,1,1,1), mfrow = c(1,1))
#' plot(net, main = "My network", col = "blue", pch = 18, lwd = 1, cex = 2)
#'
#' # Changing plot settings for nodes and edges separately.
#' plot(net, node_args = list(col = "red"), edge_args = list(col = "blue"))
#'
#' # Add grid and axis
#' par(mar = c(2.5,2.5,1,1))
#' plot(net, graticule = TRUE, axes = TRUE)
#'
#' # Plot two networks on top of each other.
#' par(mar = c(1,1,1,1), mfrow = c(1,1))
#' neta = as_sfnetwork(roxel[1:10, ])
#' netb = as_sfnetwork(roxel[50:60, ])
#' plot(neta)
#' plot(netb, node_args = list(col = "orange"), add = TRUE)
#'
#' par(oldpar)
#'
#' @importFrom graphics plot
#' @export
plot.sfnetwork = function(x, draw_lines = TRUE,
                          node_args = list(), edge_args = list(), ...) {
  # Extract geometries of nodes and edges.
  node_geoms = pull_node_geom(x)
  edge_geoms = if (has_explicit_edges(x)) pull_edge_geom(x) else NULL
  # Extract additional plot arguments.
  dots = list(...)
  # Plot the edges.
  if (draw_lines && is.null(edge_geoms)) {
    bids = edge_boundary_node_ids(x, matrix = TRUE)
    edge_geoms = draw_lines(node_geoms[bids[, 1]], node_geoms[bids[, 2]])
  }
  if (! is.null(edge_geoms)) {
    edge_args = c(edge_args, dots)
    do.call(plot, c(list(edge_geoms), edge_args))
  }
  # Plot the nodes.
  node_args = c(node_args, dots)
  if (is.null(node_args$pch)) node_args$pch = 20
  if (! is.null(edge_geoms) && ! isTRUE(node_args$add)) node_args$add = TRUE
  do.call(plot, c(list(node_geoms), node_args))
  # Return invisibly.
  invisible()
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
  object = make_edges_explicit(object) # Make sure edges are explicit.
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = nodes_as_sf(object)) +
    ggplot2::geom_sf(data = edges_as_sf(object))
}
