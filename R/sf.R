#' @importFrom sf st_as_sf
as_sf = function(x, active = NULL) {
  if (is.sf(x)) {
    return(x)
  }
  if (! is.sfnetwork(x)) {
    stop("Object is not of class sfnetwork")
  }
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = sf::st_as_sf(as_tibble(as_tbl_graph(x), active = "nodes")),
    edges = sf::st_as_sf(as_tibble(as_tbl_graph(x), active = "edges")),
    stop("Unknown active element: ", active, ". Only nodes and edges supported")
  )
}

is.sf = function(x) {
  inherits(x, "sf") | inherits(x, "sfc") | inherits(x, "sfg")
}

#' sf methods for sfnetwork objects
#'
#' Tidyverse methods for sf objects. Use these methods without the .sfnetwork
#' suffix and after loading the sf package.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y An object of class \link{sfnetwork} or class \code{\link[sf]{sf}}.
#'
#' @param ... Arguments passed on the corresponding \code{sf} function.
#'
#' @name sf
#' @export
st_as_sf.sfnetwork = function(x, ...) {
  as_sf(x)
}

#' @name sf
#' @importFrom sf st_bbox
#' @export
st_bbox.sfnetwork = function(x, ...) {
  sf::st_bbox(as_sf(x), ...)
}

#' @name sf
#' @importFrom sf st_crs
#' @export
st_crs.sfnetwork = function(x, ...) {
  sf::st_crs(as_sf(x), ...)
}

#' @name sf
#' @importFrom sf st_make_grid
#' @export
st_make_grid.sfnetwork = function(x, ...) {
  sf::st_make_grid(as_sf(x), ...)
}

#' @name sf
#' @importFrom sf st_contains
#' @export
st_contains.sfnetwork = function(x, y, ...) {
  sf::st_contains(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_contains_properly
#' @export
st_contains_properly.sfnetwork = function(x, y, ...) {
  sf::st_contains_properly(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_covers
#' @export
st_covers.sfnetwork = function(x, y, ...) {
  sf::st_covers(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_covered_by
#' @export
st_covered_by.sfnetwork = function(x, y, ...) {
  sf::st_covered_by(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_crosses
#' @export
st_crosses.sfnetwork = function(x, y, ...) {
  sf::st_crosses(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_disjoint
#' @export
st_disjoint.sfnetwork = function(x, y, ...) {
  sf::st_disjoint(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_equals
#' @export
st_equals.sfnetwork = function(x, y, ...) {
  sf::st_equals(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_equals_exact
#' @export
st_equals_exact.sfnetwork = function(x, y, ...) {
  sf::st_equals_exact(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_intersects
#' @export
st_intersects.sfnetwork = function(x, y, ...) {
  sf::st_intersects(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_is_within_distance
#' @export
st_is_within_distance.sfnetwork = function(x, y, ...) {
  sf::st_is_within_distance(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_overlaps
#' @export
st_overlaps.sfnetwork = function(x, y, ...) {
  sf::st_overlaps(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_touches
#' @export
st_touches.sfnetwork = function(x, y, ...) {
  sf::st_touches(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_within
#' @export
st_within.sfnetwork = function(x, y, ...) {
  sf::st_within(as_sf(x), as_sf(y), ...)
}
