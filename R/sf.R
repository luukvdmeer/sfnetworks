is.sf = function(x) {
  inherits(x, "sf")
}

is.sfc = function(x) {
  inherits(x, "sfc")
}

is.sfg = function(x) {
  inherits(x, "sfg")
}

#' sf methods for sfnetworks
#'
#' \code{\link[sf]{sf}} methods for \code{\link{sfnetwork}} objects.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y An object of class \code{\link[sf]{sf}}, or directly convertible to
#' it using \code{\link[sf]{st_as_sf}}. In some cases, it can also be an object
#' of \code{\link[sf:st]{sfg}} or \code{\link[sf:st_bbox]{bbox}}. Always look
#' at the documentation of the corresponding \code{sf} function for details.
#'
#' @param ... Arguments passed on the corresponding \code{sf} function.
#'
#' @param active Which network element (i.e. nodes or edges) to activate before
#' extracting. If \code{NULL}, it will be set to the current active element of
#' the given network. Defaults to \code{NULL}.
#'
#' @param value See \code{\link[sf]{st_crs}}, \code{\link[sf]{st_geometry}} or
#' \code{\link[sf]{st_agr}}.
#'
#' @details See the \code{\link[sf]{sf}} documentation.
#'
#' @name sf
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel)
#'
#' # Extract the active network element.
#' st_as_sf(net)
#'
#' # Extract any network element.
#' st_as_sf(net, "edges")
#'
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
    raise_unknown_input(active)
  )
}

#' @importFrom sf st_as_sf
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
nodes_as_sf = function(x, ...) {
  st_as_sf(
    as_tibble(as_tbl_graph(x), "nodes"),
    agr = node_agr(x),
    sf_column_name = node_geom_colname(x)
  )
}

#' @importFrom sf st_as_sf
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
edges_as_sf = function(x, ...) {
  st_as_sf(
    as_tibble(as_tbl_graph(x), "edges"),
    agr = edge_agr(x),
    sf_column_name = edge_geom_colname(x)
  )
}

# =============================================================================
# Geometries
# =============================================================================

#' @name sf
#' @examples
#' # Get geometry of the active network element.
#' st_geometry(net)
#'
#' # Get geometry of any network element.
#' st_geometry(net, "edges")
#'
#' @importFrom sf st_geometry
#' @export
st_geometry.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  if (active == "edges") expect_spatially_explicit_edges(x)
  x_geom = switch(
    active,
    nodes = node_geom(x),
    edges = edge_geom(x),
    raise_unknown_input(active)
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

#' @importFrom igraph vertex_attr
node_geom = function(x) {
  vertex_attr(x, node_geom_colname(x))
}

#' @importFrom igraph edge_attr
edge_geom = function(x) {
  edge_attr(x, edge_geom_colname(x))
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
#' @importFrom sf st_geometry<-
#' @export
`st_geometry<-.morphed_sfnetwork` = function(x, value) {
  x[] = lapply(x, `st_geometry<-`, value = value)
  x
}

#' @name sf
#' @examples
#' # Get bbox of the active network element.
#' st_bbox(net)
#'
#' @importFrom sf st_bbox st_geometry
#' @export
st_bbox.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  st_bbox(st_geometry(x), ...)
}

#' @name sf
#' @importFrom sf st_coordinates st_geometry
#' @export
st_coordinates.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  st_coordinates(st_geometry(x), ...)
}

#' @name sf
#' @importFrom sf st_geometry st_is
#' @export
st_is.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  st_is(st_geometry(x), ...)
}

# =============================================================================
# Coordinates
# =============================================================================

#' @name sf
#' @examples
#' # Get CRS of the network.
#' st_crs(net)
#'
#' @importFrom sf st_crs
#' @export
st_crs.sfnetwork = function(x, ...) {
  st_crs(node_geom(x), ...)
}

#' @name sf
#' @importFrom sf st_crs<- st_crs
#' @export
`st_crs<-.sfnetwork` = function(x, value) {
  if (has_spatially_explicit_edges(x)) {
    geom = edge_geom(x)
    st_crs(geom) = value
    x = mutate_edge_geom(x, geom)
  }
  geom = node_geom(x)
  st_crs(geom) = value
  mutate_node_geom(x, geom)
}

#' @name sf
#' @importFrom sf st_shift_longitude
#' @export
st_shift_longitude.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  change_coords(x, op = st_shift_longitude, ...)
}

#' @name sf
#' @importFrom sf st_transform
#' @export
st_transform.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  change_coords(x, op = st_transform, ...)
}

#' @name sf
#' @importFrom sf st_wrap_dateline
#' @export
st_wrap_dateline.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  change_coords(x, op = st_wrap_dateline, ...)
}

#' @name sf
#' @importFrom sf st_zm
#' @export
st_zm.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  change_coords(x, op = st_zm, ...)
}

#' @name sf
#' @importFrom sf st_geometry st_m_range
#' @export
st_m_range.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  st_m_range(st_geometry(x))
}

#' @name sf
#' @importFrom sf st_geometry st_z_range
#' @export
st_z_range.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  st_z_range(st_geometry(x))
}

change_coords = function(x, op, ...) {
  if (has_spatially_explicit_edges(x)) {
    geom = edge_geom(x)
    new_geom = do.call(match.fun(op), list(geom, ...))
    x = mutate_edge_geom(x, new_geom)
  }
  geom = node_geom(x)
  new_geom = do.call(match.fun(op), list(geom, ...))
  mutate_node_geom(x, new_geom)
}

# =============================================================================
# Attribute Geometry Relationships
# =============================================================================

#' @name sf
#' @examples
#' # Get agr factor of the active network element.
#' st_agr(net)
#'
#' # Get agr factor of any network element.
#' st_agr(net, "edges")
#'
#' @importFrom sf st_agr
#' @export
st_agr.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  if (active == "edges") expect_spatially_explicit_edges(x)
  switch(
    active,
    nodes = node_agr(x),
    edges = edge_agr(x),
    raise_unknown_input(active)
  )
}

#' @name sf
#' @importFrom sf st_agr<- st_agr st_as_sf
#' @export
`st_agr<-.sfnetwork` = function(x, value) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  x_sf = st_as_sf(x)
  st_agr(x_sf) = value
  agr(x) = st_agr(x_sf)
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
#' @importFrom sf st_as_sf st_intersects
#' @export
st_intersects.sfnetwork = function(x, y = x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  st_intersects(st_as_sf(x), st_as_sf(y), ...)
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
#' @importFrom igraph is_directed
#' @importFrom sf st_reverse
#' @importFrom tidygraph as_tbl_graph reroute
#' @export
st_reverse.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") {
    expect_spatially_explicit_edges(x)
    if (is_directed(x)) {
      warning(
        "In directed networks st_reverse swaps columns 'to' and 'from'",
        call. = FALSE
      )
      node_ids = edge_boundary_node_indices(x, matrix = TRUE)
      from_ids = node_ids[, 1]
      to_ids = node_ids[, 2]
      x_tbg = reroute(as_tbl_graph(x), from = to_ids, to = from_ids)
      x = tbg_to_sfn(x_tbg)
    }
  } else {
    warning(
      "st_reverse has no effect on nodes. Activate edges first?",
      call. = FALSE
    )
  }
  geom_unary_ops(st_reverse, x, ...)
}

#' @name sf
#' @importFrom sf st_simplify
#' @export
st_simplify.sfnetwork = function(x, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  geom_unary_ops(st_simplify, x, ...)
}

#' @importFrom sf st_as_sf st_geometry
geom_unary_ops = function(op, x, ...) {
  x_sf = st_as_sf(x)
  d_tmp = do.call(match.fun(op), list(x_sf, ...))
  mutate_geom(x, st_geometry(d_tmp))
}

# =============================================================================
# Join and filter
# =============================================================================

#' @name sf
#' @examples
#' # Spatial join applied to the active network element.
#' net = st_transform(net, 3035)
#' codes = st_as_sf(st_make_grid(net, n = c(2, 2)))
#' codes$post_code = as.character(seq(1000, 1000 + nrow(codes) * 10 - 10, 10))
#' 
#' joined = st_join(net, codes, join = st_intersects)
#' joined
#' 
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#' plot(net, col = "grey")
#' plot(codes, col = NA, border = "red", lty = 4, lwd = 4, add = TRUE)
#' text(st_coordinates(st_centroid(st_geometry(codes))), codes$post_code)
#' plot(st_geometry(joined, "edges"))
#' plot(st_as_sf(joined, "nodes"), pch = 20, add = TRUE)
#' @importFrom sf st_join
#' @export
st_join.sfnetwork = function(x, y, ...) {
  active = attr(x, "active")
  switch(
    active,
    nodes = join_nodes(x, y, ...),
    edges = join_edges(x, y, ...),
    raise_unknown_input(active)
  )
}

#' @name sf
#' @importFrom sf st_join
#' @export
st_join.morphed_sfnetwork = function(x, y, ...) {
  x[] = lapply(x, st_join, y = y, ...)
  x
}

#' @importFrom igraph is_directed
#' @importFrom sf st_as_sf st_join
join_nodes = function(x, y,  ...) {
  # Convert x and y to sf.
  x_sf = nodes_as_sf(x)
  y_sf = st_as_sf(y)
  # Add .sfnetwork_index column to keep track of original network indices.
  if (".sfnetwork_index" %in% c(names(x_sf), names(y_sf))) {
    raise_reserved_attr(".sfnetwork_index")
  }
  x_sf$.sfnetwork_index = seq_len(nrow(x_sf))
  # Join with st_join.
  n_new = st_join(x_sf, y_sf, ...)
  # If there were multiple matches:
  # --> Raise an error.
  # --> Allowing multiple matches for nodes breaks the valid network structure.
  # --> See the package vignettes for more info.
  if (has_duplicates(n_new$.sfnetwork_index)) raise_multiple_matches()
  # If an inner join was requested instead of a left join:
  # --> This means only nodes in x that had a match in y are preserved.
  # --> The other nodes are not preserved, i.e. removed.
  # --> Edges adjacent to these removed nodes need to be filtered out as well.
  args = list(...)
  if (!is.null(args$left) && args$left) {
    keep_ind = n_new$.sfnetwork_index
    x = slice(activate(x, "nodes"), keep_ind)
  }
  # Create a new network with the updated data.
  n_new$.sfnetwork_index = NULL
  x_new = sfnetwork_(n_new, edges_as_table(x), directed = is_directed(x))
  x_new %preserve_attrs% x
}

#' @importFrom igraph is_directed
#' @importFrom sf st_as_sf st_join
join_edges = function(x, y, ...) {
  expect_spatially_explicit_edges(x)
  # Convert x and y to sf.
  x_sf = edges_as_sf(x)
  y_sf = st_as_sf(y)
  # Join with st_join.
  e_new = st_join(x_sf, y_sf, ...)
  # Create a new network with the updated data.
  x_new = sfnetwork_(nodes_as_sf(x), e_new, directed = is_directed(x))
  x_new %preserve_attrs% x
}

#' @name sf
#' @importFrom sf st_crop
#' @export
st_crop.sfnetwork = function(x, y, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  filter_network(st_crop, x, y, ...)
}

#' @name sf
#' @importFrom sf st_crop
#' @export
st_crop.morphed_sfnetwork = function(x, y, ...) {
  x[] = lapply(x, st_crop, y = y, ...)
  x
}

#' @name sf
#' @examples
#' # Spatial filter applied to the active network element.
#' p1 = st_point(c(4151358, 3208045))
#' p2 = st_point(c(4151340, 3207520))
#' p3 = st_point(c(4151756, 3207506))
#' p4 = st_point(c(4151774, 3208031))
#'
#' poly = st_multipoint(c(p1, p2, p3, p4)) %>% 
#'   st_cast('POLYGON') %>% 
#'   st_sfc(crs = 3035) %>%
#'   st_as_sf()
#'
#' filtered = st_filter(net, poly, .pred = st_intersects)
#'
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#' plot(net, col = "grey")
#' plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
#' plot(filtered)
#' @importFrom sf st_filter
#' @export
st_filter.sfnetwork = function(x, y, ...) {
  if (attr(x, "active") == "edges") expect_spatially_explicit_edges(x)
  filter_network(st_filter, x, y, ...)
}

#' @name sf
#' @importFrom sf st_filter
#' @export
st_filter.morphed_sfnetwork = function(x, y, ...) {
  x[] = lapply(x, st_filter, y = y, ...)
  x
}

#' @importFrom sf st_as_sf
#' @importFrom tidygraph slice
filter_network = function(op, x, y, ...) {
  # Convert x and y to sf.
  x_sf = st_as_sf(x)
  y_sf = st_as_sf(y)
  # Add .sfnetwork_index column to keep track of original network indices.
  if (".sfnetwork_index" %in% names(x_sf)) {
    raise_reserved_attr(".sfnetwork_index")
  }
  x_sf$.sfnetwork_index = seq_len(nrow(x_sf))
  # Filter with the given operator.
  d_tmp = do.call(match.fun(op), list(x_sf, y_sf, ...))
  # Subset the original network based on the result of the filter operation.
  keep_ind = d_tmp$.sfnetwork_index
  slice(x, keep_ind)
}
