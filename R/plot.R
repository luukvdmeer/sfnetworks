#' Plot sfnetwork object
#'
#' Plot spatially explicit graph components (nodes and edges) of an sfnetwork on a map
#'
#' @param x object of class sfnetwork
#' @param ... further parameters passed to \code{\link[sf]{plot}}
#' @importFrom graphics plot
#' @export
plot.sfnetwork = function(x, ...){
  dots = list(...)

  pch_missing = is.null(dots$pch)

  sfn = sf::st_geometry(sf::st_as_sf(activate(x, "nodes")))

  if (has_spatially_explicit_edges(x)) {
    sfe = sf::st_geometry(sf::st_as_sf(activate(x, "edges")))
    sfgraph = c(sfn, sfe)
    dots$x = sfgraph
  } else {
    dots$x = sfn
  }

  dots$pch = if (pch_missing) 20 else dots$pch
  do.call(plot, dots)
}
