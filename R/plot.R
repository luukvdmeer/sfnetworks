#' Plot sfnetwork object
#'
#' Plot spatially explicit graph components (nodes and edges) of an sfnetwork on a map
#'
#' @param x object of class sfnetwork
#' @param ... further parameters passed to \code{\link[sf]{plot}}
#' @export
plot.sfnetwork = function(x, ...){
  dots = list(...)
  # print(dots)
  pch_missing = is.null(dots$pch)

  sfn = sf::st_geometry(sf::st_as_sf(activate(x, 'nodes')))
  sfe = sf::st_geometry(sf::st_as_sf(activate(x, 'edges')))
  sfgraph = c(sfn, sfe)

  dots$x = sfgraph
  dots$pch = if (pch_missing) 20 else dots$pch
  do.call(plot, dots)
}
