#' @export
st_as_sf.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = as_sf(x, "nodes"),
    edges = as_sf(x, "edges"),
    stop("Unknown active element: ", active, ". Only nodes and edges supported")
  )
}

#' @importFrom sf st_as_sf
#' @importFrom tidygraph as_tibble
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
    nodes = sf::st_as_sf(tidygraph::as_tibble(as_tbl_graph(x), active = "nodes")),
    edges = sf::st_as_sf(tidygraph::as_tibble(as_tbl_graph(x), active = "edges")),
    stop("Unknown active element: ", active, ". Only nodes and edges supported")
  )
}

is.sf = function(x) {
  inherits(x, "sf") | inherits(x, "sfc") | inherits(x, "sfg")
}

#' sf methods for sfnetwork objects
#'
#' \code{\link[sf]{sf}} methods for \code{\link{sfnetwork}} objects. Use these 
#' methods without the .sfnetwork suffix and after loading the sf package.
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
#' @param join See \code{\link[sf]{st_join}}.
#'
#' @param left See \code{\link[sf]{st_join}}.
#'
#' @param value See \code{\link[sf]{st_crs}} or \code{\link[sf]{st_geometry}}.
#'
#' @param .predicate See \code{\link[sf]{st_filter}}.
#'
#' @details See the \code{\link[sf]{sf}} documentation.
#'
#' @name sf
NULL

# =============================================================================
# CRS utils
# =============================================================================

#' @name sf
#' @importFrom sf st_crs
#' @export
st_crs.sfnetwork = function(x, ...) {
  sf::st_crs(as_sf(x), ...)
}

#' @name sf
#' @importFrom sf st_crs<-
#' @export
`st_crs<-.sfnetwork` = function(x, value) {
  switch(
    active(x),
    nodes = set_node_crs(x, value),
    edges = set_edge_crs(x, value)
  )
}

set_node_crs = function(x, value) {
  # If edges are spatially explicit, set edge crs as well.
  if (has_spatially_explicit_edges(x)) {
    x = set_element_crs(x, "edges", value)
  }
  set_element_crs(x, "nodes", value)
}

set_edge_crs = function(x, value) {
  # Set node crs as well.
  x = set_element_crs(x, "nodes", value)
  set_element_crs(x, "edges", value)
}

set_element_crs = function(x, element, value) {
  x = switch(
    element,
    nodes = activate(x, "nodes"),
    edges = activate(x, "edges")
  )
  geom = st_geometry(x)
  st_crs(geom) = value
  replace_geometry(x, geom)
}

#' @name sf
#' @importFrom sf st_shift_longitude
#' @export
st_shift_longitude.sfnetwork = function(x, ...) {
  change_coords(x, op = sf::st_shift_longitude, ...)
}

#' @name sf
#' @importFrom sf st_transform
#' @export
st_transform.sfnetwork = function(x, ...) {
  change_coords(x, op = sf::st_transform, ...)
}

#' @name sf
#' @importFrom sf st_wrap_dateline
#' @export
st_wrap_dateline.sfnetwork = function(x, ...) {
  change_coords(x, op = sf::st_wrap_dateline, ...)
}

change_coords = function(x, op, ...) {
  switch(
    active(x),
    nodes = change_node_coords(x, op, ...),
    edges = change_edge_coords(x, op, ...)
  )
}

change_node_coords = function(x, op, ...) {
  # If edges are spatially explicit, change edge coords as well.
  if (has_spatially_explicit_edges(x)) {
    x = change_element_coords(x, "edges", op, ...)
  }
  change_element_coords(x, "nodes", op, ...)
}

change_edge_coords = function(x, op) {
  # Change node coords as well.
  x = change_element_coords(x, "nodes", op, ...)
  change_element_coords(x, "edges", op, ...)
}

change_element_coords = function(x, element, op, ...) {
  x = switch(
    element,
    nodes = activate(x, "nodes"),
    edges = activate(x, "edges")
  )
  geom = st_geometry(x)
  new_geom = do.call(match.fun(op), list(geom, ...))
  replace_geometry(x, new_geom)
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
# and the LINESTRING geometries in y have the same boundary points (source and
# target may be switched) as their corresponding LINESTRING geometries in x.

#' @importFrom sf st_geometry
geom_unary_ops = function(op, x, ...) {
  xsf = as_sf(x)
  d_tmp = do.call(match.fun(op), list(xsf, ...))
  replace_geometry(x, sf::st_geometry(d_tmp))
}

#' @name sf
#' @importFrom sf st_reverse
#' @importFrom tidygraph reroute
#' @export
st_reverse.sfnetwork = function(x, ...) {
  if (active(x) == "edges") {
    if (! is_directed(x)) {
      warning("For undirected networks st_reverse has no effect on columns 'to' and 'from'")
    } else {
      warning("For directed networks st_reverse swaps columns 'to' and 'from'")
    }
    node_ids = get_boundary_node_indices(x, out = "both")
    from_ids = node_ids[, 1]
    to_ids = node_ids[, 2]
    x_tbg = tidygraph::reroute(as_tbl_graph(x), from = to_ids, to = from_ids)
    x = tbg_to_sfn(x_tbg)
  } else {
    warning("st_reverse has no effect on nodes. Maybe you want to activate edges?")
  }
  geom_unary_ops(sf::st_reverse, x, ...)
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
#' @importFrom sf st_geometry<-
#' @export
`st_geometry<-.sfnetwork` = function(x, value) {
  # Drop geometry when value = NULL.
  if (is.null(value)) {
    return(drop_geometry(x))
  }
  # Validate if the given geometry replacement keeps a valid sfnetwork structure.
  validate_geometry(x, value)
  # Replace the current geometry.
  replace_geometry(x, value)
}

validate_geometry = function(x, value) {
  switch(
    active(x),
    nodes = validate_node_geometry(x, value),
    edges = validate_edge_geometry(x, value)
  )
}

validate_node_geometry = function(x, value) {
  if (! st_is_all(value, "POINT")) {
    stop("Only geometries of type POINT are allowed as nodes")
  }
  if (has_spatially_explicit_edges(x)) {
    stop("Replacing node geometries requires spatially implicit edges")
  }
}

validate_edge_geometry = function(x, value) {
  if (! st_is_all(value, "LINESTRING")) {
    stop("Only geometries of type LINESTRING are allowed as edges")
  }
  if (! same_crs(x, value)) {
    stop("CRS of replacement not equal to network CRS. Run st_transform first?")
  }
  if (! same_boundary_points(as_sf(x), value)) {
    stop("Boundary points of replacement do not match their corresponding nodes")
  }
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
    e_tmp = as_tibble(x, "edges")
  }
  if (active(x) == "edges") {
    n_tmp = as_tibble(x, "nodes")
    e_tmp = d_tmp
  }
  sfnetwork(n_tmp, e_tmp, directed = is_directed(x), force = TRUE)
}

# =============================================================================
# Other
# =============================================================================

#' @name sf
#' @importFrom sf st_make_grid
#' @export
st_make_grid.sfnetwork = function(x, ...) {
  sf::st_make_grid(as_sf(x), ...)
}

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
