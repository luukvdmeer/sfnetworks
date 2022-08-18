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
#' @param value The value to be assigned. See the documentation of the
#' corresponding sf function for details.
#'
#' @return The \code{sfnetwork} method for \code{\link[sf]{st_as_sf}} returns
#' the active element of the network as object of class \code{\link[sf]{sf}}.
#' The \code{sfnetwork} and \code{morphed_sfnetwork} methods for
#' \code{\link[sf]{st_join}}, \code{\link[sf]{st_filter}} and
#' \code{\link[sf]{st_crop}} return an object of class \code{\link{sfnetwork}}
#' and \code{morphed_sfnetwork} respectively. All other
#' methods return the same type of objects as their corresponding sf function.
#' See the \code{\link[sf]{sf}} documentation for details.
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
  if (is.null(active)) active = attr(x, "active")
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
  require_explicit_edges(x)
  st_as_sf(
    as_tibble(as_tbl_graph(x), "edges"),
    agr = edge_agr(x),
    sf_column_name = edge_geom_colname(x)
  )
}

#' @name sf
#' @importFrom sf st_as_s2
#' @export
st_as_s2.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) active = attr(x, "active")
  switch(
    active,
    nodes = st_as_s2(pull_node_geom(x), ...),
    edges = st_as_s2(pull_edge_geom(x), ...),
    raise_unknown_input(active)
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
  pull_geom(x, active)
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
#' @importFrom sf st_drop_geometry
#' @export
st_drop_geometry.sfnetwork = function(x, ...) {
  drop_geom(x)
}

#' @name sf
#' @examples
#' # Get bbox of the active network element.
#' st_bbox(net)
#'
#' @importFrom sf st_bbox st_geometry
#' @export
st_bbox.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) active = attr(x, "active")
  switch(
    active,
    nodes = st_bbox(pull_node_geom(x), ...),
    edges = st_bbox(pull_edge_geom(x), ...),
    raise_unknown_input(active)
  )
}

#' @name sf
#' @importFrom sf st_coordinates st_geometry
#' @export
st_coordinates.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) active = attr(x, "active")
  switch(
    active,
    nodes = st_coordinates(pull_node_geom(x), ...),
    edges = st_coordinates(pull_edge_geom(x), ...),
    raise_unknown_input(active)
  )
}

#' @name sf
#' @importFrom sf st_is
#' @export
st_is.sfnetwork = function(x, ...) {
  active = attr(x, "active")
  st_is(pull_geom(x, active), ...)
}

#' @name sf
#' @importFrom sf st_is_valid
#' @export
st_is_valid.sfnetwork = function(x, ...) {
  active = attr(x, "active")
  st_is_valid(pull_geom(x, active), ...)
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
st_crs.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) active = attr(x, "active")
  switch(
    active,
    nodes = st_crs(pull_node_geom(x), ...),
    edges = st_crs(pull_edge_geom(x), ...),
    raise_unknown_input(active)
  )
}

#' @name sf
#' @importFrom sf st_crs<- st_crs
#' @export
`st_crs<-.sfnetwork` = function(x, value) {
  if (has_explicit_edges(x)) {
    geom = pull_edge_geom(x)
    st_crs(geom) = value
    x = mutate_edge_geom(x, geom)
  }
  geom = pull_node_geom(x)
  st_crs(geom) = value
  mutate_node_geom(x, geom)
}

#' @name sf
#' @importFrom sf st_precision
#' @export
st_precision.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) active = attr(x, "active")
  switch(
    active,
    nodes = st_precision(pull_node_geom(x), ...),
    edges = st_precision(pull_edge_geom(x), ...),
    raise_unknown_input(active)
  )
}

#' @name sf
#' @importFrom sf st_precision<- st_precision st_set_precision
#' @export
`st_precision<-.sfnetwork` = function(x, value) {
  st_set_precision(x, value)
}

#' @name sf
#' @importFrom sf st_set_precision st_precision<-
#' @export
st_set_precision.sfnetwork = function(x, value) {
  if (has_explicit_edges(x)) {
    geom = pull_edge_geom(x)
    st_precision(geom) = value
    x = mutate_edge_geom(x, geom)
  }
  geom = pull_node_geom(x)
  st_precision(geom) = value
  mutate_node_geom(x, geom)
}

#' @name sf
#' @importFrom sf st_shift_longitude
#' @export
st_shift_longitude.sfnetwork = function(x, ...) {
  change_coords(x, op = st_shift_longitude, ...)
}

#' @name sf
#' @importFrom sf st_transform
#' @export
st_transform.sfnetwork = function(x, ...) {
  change_coords(x, op = st_transform, ...)
}

#' @name sf
#' @importFrom sf st_wrap_dateline
#' @export
st_wrap_dateline.sfnetwork = function(x, ...) {
  change_coords(x, op = st_wrap_dateline, ...)
}

#' @name sf
#' @importFrom sf st_normalize
#' @export
st_normalize.sfnetwork = function(x, ...) {
  change_coords(x, op = st_normalize, ...)
}

#' @name sf
#' @importFrom sf st_zm
#' @export
st_zm.sfnetwork = function(x, ...) {
  change_coords(x, op = st_zm, ...)
}

#' @name sf
#' @importFrom sf st_geometry st_m_range
#' @export
st_m_range.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) active = attr(x, "active")
  switch(
    active,
    nodes = st_m_range(pull_node_geom(x), ...),
    edges = st_m_range(pull_edge_geom(x), ...),
    raise_unknown_input(active)
  )
}

#' @name sf
#' @importFrom sf st_geometry st_z_range
#' @export
st_z_range.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) active = attr(x, "active")
  switch(
    active,
    nodes = st_z_range(pull_node_geom(x), ...),
    edges = st_z_range(pull_edge_geom(x), ...),
    raise_unknown_input(active)
  )
}

change_coords = function(x, op, ...) {
  if (has_explicit_edges(x)) {
    geom = pull_edge_geom(x)
    new_geom = do.call(match.fun(op), list(geom, ...))
    x = mutate_edge_geom(x, new_geom)
  }
  geom = pull_node_geom(x)
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
  if (is.null(active)) active = attr(x, "active")
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
  active = attr(x, "active")
  x_sf = st_as_sf(x, active)
  st_agr(x_sf) = value
  agr(x, active) = st_agr(x_sf)
  x
}

# =============================================================================
# Geometric unary operations
# =============================================================================

# Only those geometric unary operations y = f(x) are supported in which:
# --> The geometry type of y is POINT when the geometry type of x is POINT and
# the POINT geometries in y have the same coordinates as their corresponding
# POINT geometries in x (this is basically useless but is what happens when
# you call for example st_reverse on POINT geometries).
# --> The geometry type of y is LINESTRING when the geometry type of x is
# LINESTRING and the LINESTRING geometries in y have the same boundary points
# as their corresponding LINESTRING geometries in x (source and target may be
# switched).

#' @name sf
#' @importFrom igraph is_directed
#' @importFrom sf st_reverse
#' @importFrom tidygraph as_tbl_graph reroute
#' @export
st_reverse.sfnetwork = function(x, ...) {
  active = attr(x, "active")
  if (active == "edges") {
    expect_explicit_edges(x)
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
  geom_unary_ops(st_reverse, x, active,...)
}

#' @name sf
#' @importFrom sf st_simplify
#' @export
st_simplify.sfnetwork = function(x, ...) {
  active = attr(x, "active")
  geom_unary_ops(st_simplify, x, active, ...)
}

#' @importFrom sf st_as_sf st_geometry
geom_unary_ops = function(op, x, active, ...) {
  x_sf = st_as_sf(x, active = active)
  d_tmp = do.call(match.fun(op), list(x_sf, ...))
  mutate_geom(x, st_geometry(d_tmp), active = active)
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
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#' plot(net, col = "grey")
#' plot(codes, col = NA, border = "red", lty = 4, lwd = 4, add = TRUE)
#' text(st_coordinates(st_centroid(st_geometry(codes))), codes$post_code)
#' plot(st_geometry(joined, "edges"))
#' plot(st_as_sf(joined, "nodes"), pch = 20, add = TRUE)
#' par(oldpar)
#' @importFrom sf st_join
#' @export
st_join.sfnetwork = function(x, y, ...) {
  active = attr(x, "active")
  switch(
    active,
    nodes = spatial_join_nodes(x, y, ...),
    edges = spatial_join_edges(x, y, ...),
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

#' @importFrom igraph delete_vertices vertex_attr<-
#' @importFrom sf st_as_sf st_join
spatial_join_nodes = function(x, y, ...) {
  # Convert x and y to sf.
  x_sf = nodes_as_sf(x)
  y_sf = st_as_sf(y)
  # Add .sfnetwork_index column to keep track of original node indices.
  if (".sfnetwork_index" %in% c(names(x_sf), names(y_sf))) {
    raise_reserved_attr(".sfnetwork_index")
  }
  orig_idxs = seq_len(nrow(x_sf))
  x_sf$.sfnetwork_index = orig_idxs
  # Join with st_join.
  n_new = st_join(x_sf, y_sf, ...)
  # If there were multiple matches:
  # --> Allowing multiple matches for nodes breaks the valid network structure.
  # --> We will only include the first match and raise a warning.
  # --> See the package vignettes for more info.
  duplicated_match = duplicated(n_new$.sfnetwork_index)
  if (any(duplicated_match)) {
    n_new = n_new[!duplicated_match, ]
    warning(
      "Multiple matches were detected from some nodes. ",
      "Only the first match is considered",
      call. = FALSE
    )
  }
  # If an inner join was requested instead of a left join:
  # --> This means only nodes in x that had a match in y are preserved.
  # --> The other nodes need to be removed.
  args = list(...)
  if (!is.null(args$left) && !args$left) {
    keep = n_new$.sfnetwork_index
    drop = if (length(keep) == 0) orig_idxs else orig_idxs[-keep]
    x = delete_vertices(x, drop) %preserve_all_attrs% x
  }
  # Update node attributes of the original network.
  n_new$.sfnetwork_index = NULL
  node_attribute_values(x) = n_new
  x
}

#' @importFrom igraph is_directed
#' @importFrom sf st_as_sf st_join
spatial_join_edges = function(x, y, ...) {
  expect_explicit_edges(x)
  # Convert x and y to sf.
  x_sf = edges_as_sf(x)
  y_sf = st_as_sf(y)
  # Join with st_join.
  e_new = st_join(x_sf, y_sf, ...)
  # Create a new network with the updated data.
  x_new = sfnetwork_(nodes_as_sf(x), e_new, directed = is_directed(x))
  x_new %preserve_network_attrs% x
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
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#' plot(net, col = "grey")
#' plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
#' plot(filtered)
#' par(oldpar)
#' @importFrom sf st_filter
#' @export
st_filter.sfnetwork = function(x, y, ...) {
  active = attr(x, "active")
  switch(
    active,
    nodes = spatial_filter_nodes(x, y, ...),
    edges = spatial_filter_edges(x, y, ...),
    raise_unknown_input(active)
  )
}

#' @name sf
#' @importFrom sf st_filter
#' @export
st_filter.morphed_sfnetwork = function(x, y, ...) {
  x[] = lapply(x, st_filter, y = y, ...)
  x
}

#' @importFrom igraph delete_vertices
#' @importFrom sf st_geometry st_filter
spatial_filter_nodes = function(x, y, ...) {
  x_sf = nodes_as_sf(x)
  y_sf = st_geometry(y)
  drop = find_indices_to_drop(x_sf, y_sf, ..., .operator = st_filter)
  delete_vertices(x, drop) %preserve_all_attrs% x
}

#' @importFrom igraph delete_edges
#' @importFrom sf st_geometry st_filter
spatial_filter_edges = function(x, y, ...) {
  expect_explicit_edges(x)
  x_sf = edges_as_sf(x)
  y_sf = st_geometry(y)
  drop = find_indices_to_drop(x_sf, y_sf, ..., .operator = st_filter)
  delete_edges(x, drop) %preserve_all_attrs% x
}

#' @name sf
#' @importFrom sf st_crop st_as_sfc
#' @export
st_crop.sfnetwork = function(x, y, ...) {
  if (inherits(y, "bbox")) y = st_as_sfc(y)
  active = attr(x, "active")
  switch(
    active,
    nodes = spatial_clip_nodes(x, y, ..., .operator = st_crop),
    edges = spatial_clip_edges(x, y, ..., .operator = st_crop),
    raise_unknown_input(active)
  )
}

#' @name sf
#' @importFrom sf st_crop
#' @export
st_crop.morphed_sfnetwork = function(x, y, ...) {
  x[] = lapply(x, st_crop, y = y, ...)
  x
}

#' @name sf
#' @importFrom sf st_difference st_as_sfc
#' @export
st_difference.sfnetwork = function(x, y, ...) {
  active = attr(x, "active")
  switch(
    active,
    nodes = spatial_clip_nodes(x, y, ..., .operator = st_difference),
    edges = spatial_clip_edges(x, y, ..., .operator = st_difference),
    raise_unknown_input(active)
  )
}

#' @name sf
#' @importFrom sf st_difference
#' @export
st_difference.morphed_sfnetwork = function(x, y, ...) {
  x[] = lapply(x, st_difference, y = y, ...)
  x
}

#' @name sf
#' @importFrom sf st_intersection st_as_sfc
#' @export
st_intersection.sfnetwork = function(x, y, ...) {
  active = attr(x, "active")
  switch(
    active,
    nodes = spatial_clip_nodes(x, y, ..., .operator = st_intersection),
    edges = spatial_clip_edges(x, y, ..., .operator = st_intersection),
    raise_unknown_input(active)
  )
}

#' @name sf
#' @importFrom sf st_intersection
#' @export
st_intersection.morphed_sfnetwork = function(x, y, ...) {
  x[] = lapply(x, st_intersection, y = y, ...)
  x
}

#' @importFrom igraph delete_vertices
#' @importFrom sf st_geometry
spatial_clip_nodes = function(x, y, ..., .operator = sf::st_intersection) {
  x_sf = nodes_as_sf(x)
  y_sf = st_geometry(y)
  drop = find_indices_to_drop(x_sf, y_sf, ..., .operator = .operator)
  delete_vertices(x, drop) %preserve_all_attrs% x
}

#' @importFrom dplyr bind_rows
#' @importFrom igraph is_directed
#' @importFrom sf st_cast st_equals st_geometry st_is st_line_merge st_sf
spatial_clip_edges = function(x, y, ..., .operator = sf::st_intersection) {
  expect_explicit_edges(x)
  # Clipping does not work good yet for undirected networks.
  if (is_directed(x)) {
    warning(
      .operator, " does not give correct results for undirected networks ",
      "when applied to the edges",
      call. = FALSE
    )
  }
  ## ===========================
  # STEP I: CLIP THE EDGES
  ## ===========================
  # Clip the edges using the given operator.
  # Possible operators are st_intersection, st_difference and st_crop.
  args = list(edges_as_sf(x), st_geometry(y), ...)
  e_new = do.call(match.fun(.operator), args)
  # A few issues need to be resolved before moving on.
  # 1) An edge shares a single point with the clipper:
  # --> The operator includes it as a point in the output.
  # --> This edge should not be part of the output.
  # 2) An edge intersects with the clipper in separate segments:
  # --> The operator includes it as a multilinestring in the output.
  # --> We want it as a single edge linestring if segments share a point.
  # --> We want it as separate edges otherwise.
  # First we select those clipped edges that are already valid.
  # These are the edges that are still a single linestring after clipping.
  e_new_l = e_new[st_is(e_new, "LINESTRING"), ]
  # Then we select the multilinestrings.
  e_new_ml = e_new[st_is(e_new, "MULTILINESTRING"), ]
  # If there are any multilinestrings, we go on processing them.
  if (nrow(e_new_ml) > 0) {
    # We run st_line_merge to merge multilinestrings into a single linestring.
    # This will only happen if their segments can be concatenated.
    e_new_ml = st_line_merge(e_new_ml)
    # Those features that got merged become an edge in the new network.
    e_new_mla = e_new_ml[st_is(e_new_ml, "LINESTRING"), ]
    # We 'unpack' those features that remained a multilinestring after merging.
    # Each of their segments becomes its own edge in the new network.
    e_new_mlb = e_new_ml[st_is(e_new_ml, "MULTILINESTRING"), ]
    if (nrow(e_new_mlb) > 0) {
      e_new_mlb = st_cast(e_new_mlb, "LINESTRING")
    } else {
      e_new_mlb = NULL
    }
    # Bind together the retrieved linestrings.
    e_new_ml = rbind(e_new_mla, e_new_mlb)
  } else {
    e_new_ml = NULL
  }
  # We bind together all retrieved linestrings.
  # This automatically exludes the point objects.
  e_new = rbind(e_new_l, e_new_ml)
  ## ===========================
  # STEP I: UPDATE THE NODES
  ## ===========================
  # Just as with any filtering operation on the edges:
  # --> All nodes of the original network will remain in the new network.
  n_orig = nodes_as_sf(x)
  # Create a new network with the original nodes and the clipped edges.
  x_tmp = sfnetwork_(n_orig, e_new, directed = directed)
  # Additional processing is required because of the following:
  # --> Edge geometries that cross the border of the clipper are cut.
  # --> Boundaries don't match their corresponding nodes anymore.
  # --> We need to add new nodes at the affected boundaries.
  # --> Otherwise the valid spatial network structure is broken.
  # We proceed as follows:
  # Retrieve the boundaries of the clipped edge geometries.
  bound_pts = edge_boundary_points(x_tmp)
  # Retrieve the nodes at the ends of each edge.
  # According to the from and to indices.
  bound_nds = edge_boundary_nodes(x_tmp)
  # Check if linestring boundaries match their corresponding nodes.
  matches = diag(st_equals(bound_pts, bound_nds, sparse = FALSE))
  # For boundary points that do not match their corresponding node:
  # --> These points will be added as new nodes to the network.
  n_add = list()
  n_add[attr(n_orig, "sf_column")] = list(bound_pts[which(!matches)])
  n_add = st_sf(n_add)
  n_new = bind_rows(n_orig, n_add)
  # Update the node indices of the from and two columns accordingly.
  idxs = edge_boundary_node_indices(x_tmp)
  idxs[!matches] = c((nrow(n_orig) + 1):(nrow(n_orig) + nrow(n_add)))
  e_new$from = idxs[seq(1, length(idxs) - 1, 2)]
  e_new$to = idxs[seq(2, length(idxs), 2)]
  # Create a new network with the updated nodes and edges.
  sfnetwork_(n_new, e_new) %preserve_network_attrs% x
}

find_indices_to_drop = function(x, y, ..., .operator = sf::st_filter) {
  # Add .sfnetwork_index column to keep track of original indices.
  if (".sfnetwork_index" %in% names(x)) {
    raise_reserved_attr(".sfnetwork_index")
  }
  orig_idxs = seq_len(nrow(x))
  x$.sfnetwork_index = orig_idxs
  # Filter with the given operator.
  filtered = do.call(match.fun(.operator), list(x, y, ...))
  # Subset the original network based on the result of the filter operation.
  keep = filtered$.sfnetwork_index
  drop = if (length(keep) == 0) orig_idxs else orig_idxs[-keep]
  drop
}

# =============================================================================
# Other
# =============================================================================

# All analytical functions in sf that do not modify the sf object itself, but
# instead return only a vector or an sfc object, should work on sfnetwork
# objects. For most of them this is already true, because they are non-generic
# functions that internally just call st_geometry() before applying the
# function itself.

# However, sf is sometimes inconsistent in deciding which functions are
# generics and which functions are not. For example:
# --> All geometric binary predicates are non-generics except st_intersects.
# --> st_line_sample is non-generic but st_sample is a generic.
# --> st_length is non-generic but st_area is a generic.

# When these functions are generics they will not work on sfnetwork objects no
# matter if they internally just call st_geometry(). Therefore we need to
# create specific sfnetwork methods for these functions in order to make them
# work as expected.

#' @name sf
#' @importFrom sf st_geometry st_intersects
#' @export
st_intersects.sfnetwork = function(x, y, ...) {
  active = attr(x, "active")
  if (missing(y)) {
    st_intersects(pull_geom(x, active), ...)
  } else {
    st_intersects(pull_geom(x, active), st_geometry(y), ...)
  }
}

#' @name sf
#' @importFrom sf st_as_sf st_sample
#' @export
st_sample.sfnetwork = function(x, size, ...) {
  active = attr(x, "active")
  st_sample(st_as_sf(x, active), size, ...)
}

#' @name sf
#' @importFrom sf st_geometry st_nearest_points
#' @export
st_nearest_points.sfnetwork = function(x, y, ...) {
  active = attr(x, "active")
  st_nearest_points(pull_geom(x, active), st_geometry(y), ...)
}

#' @name sf
#' @importFrom sf st_area
#' @export
st_area.sfnetwork = function(x, ...) {
  active = attr(x, "active")
  st_area(pull_geom(x, active), ...)
}


