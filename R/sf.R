#' @importFrom sf st_as_sf
as_sf = function(x, active = NULL) {
  if (is.sf(x)) {
    return(x)
  } else {
    stopifnot(is.sfnetwork(x))
  }
  if (is.null(active)) {
    active = active(x)
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
#' In some cases, it can also be an object of \code{\link[sf]{sfc}},
#' \code{\link[sf:st]{sfg}} or \code{\link[sf:st_bbox]{bbox}}. Always look at
#' the documentation of the corresponding \code{sf} function for details.
#'
#' @param ... Arguments passed on the corresponding \code{sf} function.
#'
#' @param crs See \code{\link[sf]{st_transform}}.
#'
#' @param join See \code{\link[sf]{st_join}}.
#'
#' @param left See \code{\link[sf]{st_join}}.
#'
#' @param value See \code{\link[sf]{st_crs}} or \code{\link[sf]{st_geometry}}.
#'
#' @param .predicate See \code{\link[sf]{st_filter}}.
#'
#' @details See the \code{\link[sf]{sf}} documentation for details on all
#' functions.
#'
#' @name sf
#' @export
st_as_sf.sfnetwork = function(x, ...) {
  as_sf(x)
}

# =============================================================================
# CRS utils
# =============================================================================

#' @importFrom sf st_geometry
change_element_crs = function(x, what, set, op, val, ...) {
  if (what == "nodes") x = activate(x, "nodes")
  if (what == "edges") x = activate(x, "edges")
  xsf = as_sf(x)
  if (set) {
    st_crs(xsf) = val
    x = replace_geometry(x, sf::st_geometry(xsf))
  } else {
    n_tmp = do.call(match.fun(op), list(xsf, ...))
    x = replace_geometry(x, sf::st_geometry(n_tmp))
  }
  x
}

change_network_crs = function(x, set, op, val, ...) {
  if (active(x) == "nodes") {
    if (has_spatially_explicit_edges(x)) {
      x = change_element_crs(x, "edges", set = set, op = op, val = val, ...)
    }
    x = change_element_crs(x, "nodes", set = set, op = op, val = val, ...)
  }
  if (active(x) == "edges") {
    x = change_element_crs(x, "nodes", set = set, op = op, val = val, ...)
    x = change_element_crs(x, "edges", set = set, op = op, val = val, ...)
  }
  x
}

#' @name sf
#' @importFrom sf st_crs
#' @export
st_crs.sfnetwork = function(x, ...) {
  sf::st_crs(as_sf(x), ...)
}

#' @name sf
#' @importFrom sf st_crs<- st_geometry
#' @export
`st_crs<-.sfnetwork` = function(x, value) {
  change_network_crs(x, set = TRUE, op = NULL, val = value)
}

#' @name sf
#' @importFrom sf st_shift_longitude
#' @export
st_shift_longitude.sfnetwork = function(x, ...) {
  change_network_crs(x, set = FALSE, op = sf::st_shift_longitude, val = NULL, ...)
}

#' @name sf
#' @importFrom sf st_transform
#' @export
st_transform.sfnetwork = function(x, crs, ...) {
  change_network_crs(x, set = FALSE, op = sf::st_transform, val = NULL, crs, ...)
}

#' @name sf
#' @importFrom sf st_wrap_dateline
#' @export
st_wrap_dateline.sfnetwork = function(x, ...) {
  change_network_crs(x, set = FALSE, op = sf::st_wrap_dateline, val = NULL, ...)
}

# =============================================================================
# Dimension, simplicity, validity or is_empty queries
# =============================================================================

#' @name sf
#' @importFrom sf st_dimension
#' @export
st_dimension.sfnetwork = function(x, ...) {
  sf::st_dimension(as_sf(x), ...)
}

#' @name sf
#' @importFrom sf st_is_empty
#' @export
st_is_empty.sfnetwork = function(x) {
  sf::st_is_empty(as_sf(x))
}

#' @name sf
#' @importFrom sf st_is_simple
#' @export
st_is_simple.sfnetwork = function(x) {
  sf::st_is_simple(as_sf(x))
}

#' @name sf
#' @importFrom sf st_is_valid
#' @export
st_is_valid.sfnetwork = function(x, ...) {
  sf::st_is_valid(as_sf(x), ...)
}

# =============================================================================
# Geometric binary predicates
# =============================================================================

#' @name sf
#' @importFrom sf st_contains
#' @export
st_contains.sfnetwork = function(x, y = x, ...) {
  sf::st_contains(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_contains_properly
#' @export
st_contains_properly.sfnetwork = function(x, y = x, ...) {
  sf::st_contains_properly(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_covers
#' @export
st_covers.sfnetwork = function(x, y = x, ...) {
  sf::st_covers(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_covered_by
#' @export
st_covered_by.sfnetwork = function(x, y = x, ...) {
  sf::st_covered_by(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_crosses
#' @export
st_crosses.sfnetwork = function(x, y = x, ...) {
  sf::st_crosses(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_disjoint
#' @export
st_disjoint.sfnetwork = function(x, y = x, ...) {
  sf::st_disjoint(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_equals
#' @export
st_equals.sfnetwork = function(x, y = x, ...) {
  sf::st_equals(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_equals_exact
#' @export
st_equals_exact.sfnetwork = function(x, y = x, ...) {
  sf::st_equals_exact(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_intersects
#' @export
st_intersects.sfnetwork = function(x, y = x, ...) {
  sf::st_intersects(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_is_within_distance
#' @export
st_is_within_distance.sfnetwork = function(x, y = x, ...) {
  sf::st_is_within_distance(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_overlaps
#' @export
st_overlaps.sfnetwork = function(x, y = x, ...) {
  sf::st_overlaps(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_touches
#' @export
st_touches.sfnetwork = function(x, y = x, ...) {
  sf::st_touches(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_within
#' @export
st_within.sfnetwork = function(x, y = x, ...) {
  sf::st_within(as_sf(x), as_sf(y), ...)
}

# =============================================================================
# Geometric measurements
# =============================================================================

#' @name sf
#' @importFrom sf st_area
#' @export
st_area.sfnetwork = function(x) {
  sf::st_area(as_sf(x))
}

#' @name sf
#' @importFrom sf st_distance
#' @export
st_distance.sfnetwork = function(x, y = x, ...) {
  sf::st_distance(as_sf(x), as_sf(y), ...)
}

#' @name sf
#' @importFrom sf st_length
#' @export
st_length.sfnetwork = function(x) {
  sf::st_length(as_sf(x))
}

# =============================================================================
# Geometric unary operations
# =============================================================================

# NOTE: Only those geometric unary operations y = f(x) are supported in which:
# The geometry type of y is POINT when the geometry type of x is POINT and the
# POINT geometries in y have the same coordinates as their corresponding POINT
# geometries in x (this is basically useless but is what happens when you call
# for example st_reverse on POINT geometries).
# Or:
# The geometry type of y is LINESTRING when the geometry type of x is LINESTRING
# and the LINESTRING geometries in y have the same endpoints (source and target
# may be switched) as their corresponding LINESTRING geometries in x.

#' @importFrom sf st_geometry
geom_unary_ops = function(op, x, ...) {
  xsf = as_sf(x)
  d_tmp = do.call(match.fun(op), list(xsf, ...))
  replace_geometry(x, sf::st_geometry(d_tmp))
}

#' @name sf
#' @importFrom sf st_reverse
#' @export
st_reverse.sfnetwork = function(x, ...) {
  if (active(x) == "edges") {
    if (! is_directed(x)) {
      warning("st_reverse has no effect on undirected edges")
      return(x)
    }
    warning("Reversing edges swaps columns 'to' and 'from'")
    edges = as_sf(x)
    from = edges$from
    to = edges$to
    edges$to = from
    edges$from = to
    x = sfnetwork(as_sf(x, "nodes"), edges, directed = is_directed(x))
    x = activate(x, "edges")
  } else {
    warning("st_reverse has no effect on nodes. Maybe you want to activate edges?")
  }
  geom_unary_ops(sf::st_reverse, x, ...)
}

#' @name sf
#' @importFrom sf st_segmentize
#' @export
st_segmentize.sfnetwork = function(x, ...) {
  geom_unary_ops(sf::st_segmentize, x, ...)
}

#' @name sf
#' @importFrom sf st_simplify
#' @export
st_simplify.sfnetwork = function(x, ...) {
  geom_unary_ops(sf::st_simplify, x, ...)
}

# =============================================================================
# Geometry utils
# =============================================================================

#' @name sf
#' @importFrom sf st_bbox
#' @export
st_bbox.sfnetwork = function(x, ...) {
  sf::st_bbox(as_sf(x), ...)
}

#' @name sf
#' @importFrom sf st_coordinates
#' @export
st_coordinates.sfnetwork = function(x, ...) {
  sf::st_coordinates(as_sf(x), ...)
}

#' @name sf
#' @importFrom sf st_geometry
#' @export
st_geometry.sfnetwork = function(x, ...) {
  sf::st_geometry(as_sf(x), ...)
}

#' @name sf
#' @importFrom rlang !! :=
#' @importFrom sf st_geometry<-
#' @importFrom tidygraph mutate
#' @export
`st_geometry<-.sfnetwork` = function(x, value) {
  stopifnot(inherits(value, "sfc") || is.null(value))
  if (!is.null(value)) {
    if (active(x) == "nodes" && !st_is_all(value, "POINT")) {
      stop("Only geometries of type POINT are allowed as nodes")
    }
    if (active(x) == "edges" && !st_is_all(value, "LINESTRING")) {
      stop("Only geometries of type LINESTRING are allowed as edges")
    }
    if (has_spatially_explicit_edges(x)) {
      if (active(x) == "nodes") {
        stop("Node geometries cannot be replaced when edges are spatially explicit")
      }
      if (active(x) == "edges") {
        if (! same_crs(x, value)) {
          stop("Edge geometries can only be replaced when the CRS doesn't change")
        }
        if (! same_endpoints(as_sf(x), value)) {
          stop("Edge geometries can only be replaced when the endpoints don't change")
        }
      }
    }
    x = replace_geometry(x, value)
  } else {
    x = drop_geometry(x, what = active(x))
  }
  x
}

#' @name sf
#' @importFrom sf st_geometry_type
#' @export
st_geometry_type.sfnetwork = function(x, ...) {
  sf::st_geometry_type(as_sf(x), ...)
}

#' @name sf
#' @importFrom sf st_is
#' @export
st_is.sfnetwork = function(x, ...) {
  sf::st_is(as_sf(x), ...)
}

# =============================================================================
# Join and filter
# =============================================================================

#' @importFrom tidygraph slice
filter_network = function(op, x, y, ...) {
  xsf = as_sf(x)
  ysf = as_sf(y)
  if (".sfnetwork_index" %in% names(xsf)) {
    stop("The attribute name '.sfnetwork_index' is reserved")
  }
  xsf$.sfnetwork_index = seq_len(nrow(xsf))
  d_tmp = do.call(match.fun(op), list(xsf, ysf, ...))
  keep_ind = d_tmp$.sfnetwork_index
  tidygraph::slice(x, keep_ind)
}

#' @name sf
#' @importFrom sf st_crop
#' @export
st_crop.sfnetwork = function(x, y, ...) {
  filter_network(sf::st_crop, x, y, ...)
}

#' @name sf
#' @importFrom sf st_filter
#' @export
st_filter.sfnetwork = function(x, y, ..., .predicate = st_intersects) {
  filter_network(sf::st_filter, x, y, ..., .predicate = .predicate)
}

#' @name sf
#' @importFrom sf st_join
#' @importFrom tidygraph slice
#' @export
st_join.sfnetwork = function(x, y, join = st_intersects, ..., left = TRUE) {
  xsf = as_sf(x)
  ysf = as_sf(y)
  if (".sfnetwork_index" %in% names(xsf)) {
    stop("The attribute name '.sfnetwork_index' is reserved")
  }
  xsf$.sfnetwork_index = seq_len(nrow(xsf))
  d_tmp = sf::st_join(xsf, ysf, join = join, ..., left = left)
  if (active(x) == "nodes") {
    if (multiple_matches(d_tmp)) {
      stop("Multiple matches are not allowed when using st_join on the nodes")
    }
  }
  if (! left) {
    keep_ind = d_tmp$.sfnetwork_index
    x = tidygraph::slice(x, keep_ind)
  }
  d_tmp$.sfnetwork_index = NULL
  if (active(x) == "nodes") {
    n_tmp = d_tmp
    e_tmp = as_tibble(activate(x, "edges"))
  }
  if (active(x) == "edges") {
    n_tmp = as_tibble(activate(x, "nodes"))
    e_tmp = d_tmp
  }
  sfnetwork(nodes = n_tmp, edges = e_tmp, directed = is_directed(x))
}

# =============================================================================
# Other
# =============================================================================

#' @name sf
#' @importFrom sf st_nearest_feature
#' @export
st_nearest_feature.sfnetwork = function(x, y) {
  sf::st_nearest_feature(as_sf(x), as_sf(y))
}

#' @name sf
#' @importFrom sf st_relate
#' @export
st_relate.sfnetwork = function(x, y, ...) {
  sf::st_relate(as_sf(x), as_sf(y), ...)
}
