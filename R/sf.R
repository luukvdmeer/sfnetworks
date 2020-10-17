as_sf = function(x) {
  if (is.sf(x) | is.sfc(x) | is.sfg(x)) x else st_as_sf(x)
}

is.sf = function(x) {
  inherits(x, "sf")
}

is.sfc = function(x) {
  inherits(x, "sfc")
}

is.sfg = function(x) {
  inherits(x, "sfg")
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
#' @param active Which network element (i.e. nodes or edges) to activate before
#' extracting. If \code{NULL}, it will be set to the current active element of
#' the given network. Defaults to \code{NULL}.
#'
#' @param value See \code{\link[sf]{st_crs}} or \code{\link[sf]{st_geometry}}.
#'
#' @param join See \code{\link[sf]{st_join}}.
#'
#' @param left See \code{\link[sf]{st_join}}.
#'
#' @param .predicate See \code{\link[sf]{st_filter}}.
#'
#' @details See the \code{\link[sf]{sf}} documentation.
#'
#' @name sf
#' @importFrom sf st_as_sf
#' @export
st_as_sf.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  if (active == "edges") expect_spatially_explicit_edges(x)
  switch(
    active,
    nodes = nodes_as_sf(x, ...),
    edges = edges_as_sf(x, ...),
    throw_unknown_active_exception(active)
  )
}

#' @importFrom tidygraph as_tibble
nodes_as_sf = function(x, ...) {
  sf::st_as_sf(
    tidygraph::as_tibble(as_tbl_graph(x), "nodes"),
    agr = node_agr(x),
    sf_column_name = node_geom_colname(x)
  )
}

#' @importFrom tidygraph as_tibble
edges_as_sf = function(x, ...) {
  sf::st_as_sf(
    tidygraph::as_tibble(as_tbl_graph(x), "edges"),
    agr = edge_agr(x),
    sf_column_name = edge_geom_colname(x)
  )
}

# =============================================================================
# Geometries
# =============================================================================

#' @name sf
#' @importFrom sf st_geometry
#' @importFrom igraph edge_attr vertex_attr
#' @export
st_geometry.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  if (active == "edges") expect_spatially_explicit_edges(x)
  x_geom = switch(
    active,
    nodes = igraph::vertex_attr(x, node_geom_colname(x)),
    edges = igraph::edge_attr(x, edge_geom_colname(x)),
    throw_unknown_active_exception(active)
  )
  if (! is.sfc(x_geom)) {
    stop(
      "Attribute 'sf_column' does not point to a geometry column.\n",
      "Did you rename it, without setting st_geometry(x) = 'newname'?",
      call. = FALSE
    )
  }
  x_geom
}

#' @name sf
#' @importFrom sf st_geometry<-
#' @export
`st_geometry<-.sfnetwork` = function(x, value) {
  if (is.null(value)) {
    x_new = drop_geom(x) 
  } else  {
    x_new = mutate_geom(x, value)
    require_valid_network_structure(x_new)
  }
  x_new
}

#' @name sf
#' @importFrom sf st_bbox
#' @export
st_bbox.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  sf::st_bbox(st_geometry(x), ...)
}

#' @name sf
#' @importFrom sf st_coordinates
#' @export
st_coordinates.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  sf::st_coordinates(st_geometry(x), ...)
}

#' @name sf
#' @importFrom sf st_is
#' @export
st_is.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  sf::st_is(st_geometry(x), ...)
}

# =============================================================================
# Coordinates
# =============================================================================

#' @name sf
#' @importFrom sf st_crs
#' @export
st_crs.sfnetwork = function(x, ...) {
  sf::st_crs(st_geometry(x), ...)
}

#' @name sf
#' @importFrom sf st_crs<-
#' @export
`st_crs<-.sfnetwork` = function(x, value) {
  if (has_spatially_explicit_edges(x)) {
    geom = st_geometry(x, "edges")
    sf::st_crs(geom) = value
    x = mutate_geom(x, geom, "edges")
  }
  geom = st_geometry(x, "nodes")
  sf::st_crs(geom) = value
  mutate_geom(x, geom, "nodes")
}

#' @name sf
#' @importFrom sf st_shift_longitude
#' @export
st_shift_longitude.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  change_coords(x, op = sf::st_shift_longitude, ...)
}

#' @name sf
#' @importFrom sf st_transform
#' @export
st_transform.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  change_coords(x, op = sf::st_transform, ...)
}

#' @name sf
#' @importFrom sf st_wrap_dateline
#' @export
st_wrap_dateline.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  change_coords(x, op = sf::st_wrap_dateline, ...)
}

#' @name sf
#' @importFrom sf st_zm
#' @export
st_zm.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  change_coords(x, op = sf::st_zm, ...)
}

#' @name sf
#' @importFrom sf st_m_range
#' @export
st_m_range.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  sf::st_m_range(st_geometry(x))
}

#' @name sf
#' @importFrom sf st_z_range
#' @export
st_z_range.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  sf::st_z_range(st_geometry(x))
}

change_coords = function(x, op, ...) {
  if (has_spatially_explicit_edges(x)) {
    geom = st_geometry(x, "edges")
    new_geom = do.call(match.fun(op), list(geom, ...))
    x = mutate_geom(x, new_geom, "edges")
  }
  geom = st_geometry(x, "nodes")
  new_geom = do.call(match.fun(op), list(geom, ...))
  mutate_geom(x, new_geom, "nodes")
}

# =============================================================================
# Attribute Geometry Relationships
# =============================================================================

#' @name sf
#' @importFrom sf st_agr
#' @export
st_agr.sfnetwork = function(x, active = NULL, ...) {
  agr(x, active)
}

#' @name sf
#' @importFrom sf st_agr<- st_agr
#' @export
`st_agr<-.sfnetwork` = function(x, value) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  x_sf = st_as_sf(x)
  sf::st_agr(x_sf) = value
  agr(x) = sf::st_agr(x_sf)
  x
}

# =============================================================================
# Geometric binary predicates
# =============================================================================

# Geometric binary predicates internally are applied to the geometry of the 
# given object. Since there is a st_geometry.sfnetwork method, they work
# automatically on sfnetwork objects too. However, st_intersects is the only one
# that is a generic, and thus an sfnetwork method needs to be created for it.

#' @name sf
#' @importFrom sf st_intersects
#' @export
st_intersects.sfnetwork = function(x, y = x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  sf::st_intersects(as_sf(x), as_sf(y), ...)
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

#' @name sf
#' @importFrom sf st_reverse
#' @importFrom tidygraph reroute
#' @export
st_reverse.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") {
    expect_spatially_explicit_edges(x)
    if (is_directed(x)) {
      warning("In directed networks st_reverse swaps columns 'to' and 'from'")
    }
    node_ids = edge_boundary_node_indices(x)
    from_ids = node_ids[, 1]
    to_ids = node_ids[, 2]
    x_tbg = tidygraph::reroute(as_tbl_graph(x), from = to_ids, to = from_ids)
    x = tbg_to_sfn(x_tbg)
  } else {
    warning("st_reverse has no effect on nodes. Activate edges first?")
  }
  geom_unary_ops(sf::st_reverse, x, ...)
}

#' @name sf
#' @importFrom sf st_simplify
#' @export
st_simplify.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  geom_unary_ops(sf::st_simplify, x, ...)
}

geom_unary_ops = function(op, x, ...) {
  x_sf = as_sf(x)
  d_tmp = do.call(match.fun(op), list(x_sf, ...))
  mutate_geom(x, sf::st_geometry(d_tmp))
}

# =============================================================================
# Join and filter
# =============================================================================

#' @name sf
#' @importFrom sf st_join
#' @importFrom tidygraph slice
#' @export
st_join.sfnetwork = function(x, y, join = st_intersects, ..., left = TRUE) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  x_sf = as_sf(x)
  y_sf = as_sf(y)
  if (".sfnetwork_index" %in% names(x_sf)) {
    stop("The attribute name '.sfnetwork_index' is reserved", call. = FALSE)
  }
  x_sf$.sfnetwork_index = seq_len(nrow(x_sf))
  d_tmp = sf::st_join(x_sf, y_sf, join = join, ..., left = left)
  if (attr(x, "active") == "nodes") {
    if (has_duplicates(d_tmp$.sfnetwork_index)) {
      stop("One or more nodes have multiple matches", call. = FALSE)
    }
  }
  if (! left) {
    keep_ind = d_tmp$.sfnetwork_index
    x = tidygraph::slice(x, keep_ind)
  }
  d_tmp$.sfnetwork_index = NULL
  if (attr(x, "active") == "nodes") {
    n_tmp = d_tmp
    e_tmp = as_tibble(x, "edges")
  }
  if (attr(x, "active") == "edges") {
    n_tmp = as_tibble(x, "nodes")
    e_tmp = d_tmp
  }
  sfnetwork(n_tmp, e_tmp, directed = is_directed(x), force = TRUE)
}

#' @name sf
#' @importFrom sf st_crop
#' @export
st_crop.sfnetwork = function(x, y, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  filter_network(sf::st_crop, x, y, ...)
}

#' @name sf
#' @importFrom sf st_filter
#' @export
st_filter.sfnetwork = function(x, y, ..., .predicate = st_intersects) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  filter_network(sf::st_filter, x, y, ..., .predicate = .predicate)
}

filter_network = function(op, x, y, ...) {
  x_sf = as_sf(x)
  y_sf = as_sf(y)
  if (".sfnetwork_index" %in% names(x_sf)) {
    stop("The attribute name '.sfnetwork_index' is reserved", call. = FALSE)
  }
  x_sf$.sfnetwork_index = seq_len(nrow(x_sf))
  d_tmp = do.call(match.fun(op), list(x_sf, y_sf, ...))
  keep_ind = d_tmp$.sfnetwork_index
  tidygraph::slice(x, keep_ind)
}