#' Plot a spatial network
#'
#' Plot an object of class \code{sfn_network} or \code{sfn_route}.
#'
#' @param x object of class \code{sfn_network} or \code{sfn_route}
#' @param y ignored
#' @param ... further specifications, passed on to \link{plot_sf}
#' @param col_edges color of the edges
#' @param lwd linewidth of the edges
#' @param col_nodes color of the nodes
#' @param pch plotting symbol of the nodes
#' @param cex symbol size of the nodes
#' @param network object of class \code{sfn_network};
#' when plotting an object of class \code{sfn_route}, this parameter defines the network that should be plotted on the background;
#' by default set to \code{NULL}, meaning no network is plotted on the background
#' @param attribute name of a column in the edges element of the input object, that should be used to color the edges;
#' by default set to \code{NULL};
#' if an attribute column is specified, only the edges will be plotted, and col_edges will be ignored
#' @param pal the color palette that is be used to color the edges by attribute;
#' if an attribute column is given but pal is omitted, \link{sf.colors} is used
#' @param add logical; if \code{TRUE}, plot will be added on top of the current plot;
#' by default set to \code{FALSE}
#' @method plot sfn_network
#' @name plot
#' @details The plot methods for objects of class \code{sfn_network} and \code{sfn_route}
#' uses the plot method for \code{sf} objects to plot the edges and noted.
#' See \link{plot_sf} for a more detailed description.
#' @importFrom magrittr %>%
#' @importFrom purrr pluck
#' @importFrom sf st_geometry
#' @importFrom graphics plot par
#' @export
plot.sfn_network = function(x, y, ..., col_edges = 'black', lwd = 1, col_nodes = 'black', pch = 16, cex = 0.5, attribute = NULL, pal = NULL, add = FALSE) {
  # The plot method for sfn_network only takes an x input
  # Stop when user gives a value for y
  stopifnot(missing(y))

  # If attribute is set to NULL, plot only geometry of the network
  # Plot the edges, with the nodes added on top
  # Set margins such that the network is clearly vissible
  if(is.null(attribute)) {
    par(mar = c(1,1,1.2,1))

    edges = x %>%
      pluck('edges') %>%
      st_geometry()

    nodes = x %>%
      pluck('nodes') %>%
      st_geometry

    plot(edges, col = col_edges, lwd = lwd, add = add, ...)

    plot(nodes, col = col_nodes, pch = pch, cex = cex, add = TRUE, ...)
  }

  # If attribute is set to a column name, retrieve this column
  # Make an sf plot with only this attribute column
  else {
    edges = x %>%
      pluck('edges') %>%
      select(attribute)

    plot(edges, lwd = lwd, add = add, ...)
  }
}

#' @name plot
#' @method plot sfn_route
#' @export
plot.sfn_route = function(x, y, ..., col_edges = 'black', lwd = 1, col_nodes = 'black', pch = 16, cex = 0.5, network = NULL, attribute = NULL, pal = NULL, add = FALSE) {
  # The plot method for sfn_route only takes an x input
  # Stop when user gives a value for y
  stopifnot(missing(y))

  # Convert sfn_route object to sfn_network object
  x = x %>% sfn_asnetwork()

  # If network is not set to NULL, first plot the given network as background
  # Plot the network in grayscale
  # The shortest path will then be plotted on top of the network
  # Therefore, set add to NULL when network is NULL and to TRUE when network is not NULL
  if(!is.null(network)) {
    # Plot network basemap
    plot(
      x = network,
      col_edges = "gray80",
      lwd = 1,
      col_nodes = "gray50",
      pch = pch,
      cex = 0.1,
      attribute = NULL,
      ...
    )

    # Plot shortest path on top
    plot(
      x = x,
      col_edges = col_edges,
      lwd = lwd,
      col_nodes = col_nodes,
      pch = pch,
      cex = cex,
      attribute = attribute,
      add = TRUE,
      ...
    )
  }

  # If network is NULL, only plot the shortest path
  else{
    plot(
      x = x,
      col_edges = col_edges,
      lwd = lwd,
      col_nodes = col_nodes,
      pch = pch,
      cex = cex,
      attribute = attribute,
      ...
    )
  }
}
