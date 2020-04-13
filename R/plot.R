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
#' @importFrom sf st_as_sf st_geometry st_sfc
#' @export
plot.sfnetwork = function(x, ...) {
  dots = list(...)
  nsf = sf::st_geometry(sf::st_as_sf(activate(x, "nodes"))) # Nodes
  if (has_spatially_explicit_edges(x)) {
    esf = sf::st_geometry(sf::st_as_sf(activate(x, "edges"))) # Edges
  } else {
    sources = sf::st_as_sf(get_nodes(x))[as_tibble(get_edges(x))$from,]
    targets = sf::st_as_sf(get_nodes(x))[as_tibble(get_edges(x))$to,]
    # Create edges when not spatially explicit
    esf = sf::st_sfc(
      mapply(
        function (a,b) points_to_lines(a,b),
        sf::st_geometry(sources),
        sf::st_geometry(targets),
        SIMPLIFY=FALSE
      )
    )
  }
  # Bind nodes and edges into one sf object if edges are spatially explicit.
  gsf = c(nsf, esf) # Full graph
  dots$x = gsf
  # Use pch of 20 by default.
  pch_missing = is.null(dots$pch)
  dots$pch = if (pch_missing) 20 else dots$pch
  do.call(plot, dots)
}
