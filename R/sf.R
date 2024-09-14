#' sf methods for sfnetworks
#'
#' \code{\link[sf]{sf}} methods for \code{\link{sfnetwork}} objects.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param obj An object of class \code{\link{sfnetwork}}.
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
#' @param focused Should only features that are in focus be extracted? Defaults
#' to \code{TRUE}. See \code{\link[tidygraph]{focus}} for more information on
#' focused networks.
#'
#' @param value The value to be assigned. See the documentation of the
#' corresponding sf function for details.
#'
#' @param precision The precision to be assigned. See
#' \code{\link[sf]{st_precision}} for details.
#'
#' @return The methods for \code{\link[sf]{st_join}},
#' \code{\link[sf]{st_filter}}, \code{\link[sf]{st_intersection}},
#' \code{\link[sf]{st_difference}} and \code{\link[sf]{st_crop}}, as well as
#' the methods for all setter functions and the geometric unary operations
#' preserve the class of the object it is applied to, i.e. either a
#' \code{\link{sfnetwork}} object or its morphed equivalent. When dropping node
#' geometries, an object of class \code{\link[tidygraph]{tbl_graph}} is
#' returned. All other methods return the same type of objects as their
#' corresponding sf function. See the \code{\link[sf]{sf}} documentation for
#' details.
#'
#' @details See the \code{\link[sf]{sf}} documentation. The following methods
#' have a special behavior:
#'
#' \itemize{
#'   \item \code{st_geometry<-}: The geometry setter requires the replacement
#'   geometries to have the same CRS as the network. Node replacements should
#'   all be points, while edge replacements should all be linestrings. When
#'   replacing node geometries, the boundaries of the edge geometries are
#'   replaced as well to preserve the valid spatial network structure. When
#'   replacing edge geometries, new edge boundaries that do not match the
#'   location of their specified incident node are added as new nodes to the
#'   network.
#'   \item \code{st_transform}: No matter if applied to the nodes or edge
#'   table, this method will update the coordinates of both tables. The same
#'   holds for all other methods that update the way in which the coordinates
#'   are encoded without changing their actual location, such as
#'   \code{st_precision}, \code{st_normalize}, \code{st_zm}, and others.
#'   \item \code{st_join}: When applied to the nodes table and multiple matches
#'   exist for the same node, only the first match is joined. A warning will be
#'   given in this case.
#'   \item \code{st_intersection}, \code{st_difference} and \code{st_crop}:
#'   These methods clip edge geometries when applied to the edges table. To
#'   preserve a valid spatial network structure, clipped edge boundaries are
#'   added as new nodes to the network.
#'   \item \code{st_reverse}: When reversing edge geometries in a directed
#'   network, the indices in the from and to columns will be swapped as well.
#'   \item \code{st_segmentize}: When segmentizing edge geometries, the edge
#'   boundaries are forced to remain the same such that the valid spatial
#'   network structure is preserved. This may lead to slightly inaccurate
#'   results.
#' }
#'
#' Geometric unary operations are only supported on \code{\link{sfnetwork}}
#' objects if they do not change the geometry type nor the spatial location
#' of the original features, since that would break the valid spatial network
#' structure. When applying the unsupported operations, first extract the
#' element of interest (nodes or edges) using \code{\link[sf]{st_as_sf}}.
#'
#' @name sf_methods
NULL

#' @name sf_methods
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#'
#' net = as_sfnetwork(roxel)
#'
#' # Extract the active network element as sf object.
#' st_as_sf(net)
#'
#' # Extract any network element as sf object.
#' st_as_sf(net, "edges")
#'
#' @importFrom sf st_as_sf
#' @export
st_as_sf.sfnetwork = function(x, active = NULL, focused = TRUE, ...) {
  if (is.null(active)) active = attr(x, "active")
  switch(
    active,
    nodes = nodes_as_sf(x, focused = focused, ...),
    edges = edges_as_sf(x, focused = focused, ...),
    raise_invalid_active(active)
  )
}

#' @importFrom sf st_as_sf
nodes_as_sf = function(x, focused = FALSE, ...) {
  st_as_sf(
    nodes_as_regular_tibble(x, focused = focused),
    agr = node_agr(x),
    sf_column_name = node_geom_colname(x),
    ...
  )
}

#' @importFrom sf st_as_sf
edges_as_sf = function(x, focused = FALSE, ...) {
  geom_colname = edge_geom_colname(x)
  if (is.null(geom_colname)) raise_require_explicit()
  st_as_sf(
    edges_as_regular_tibble(x, focused = focused),
    agr = edge_agr(x),
    sf_column_name = geom_colname,
    ...
  )
}

# =============================================================================
# Geometries
# =============================================================================

#' @name sf_methods
#' @examples
#' # Get the geometry of the active network element.
#' st_geometry(net)
#'
#' # Get the geometry of any network element.
#' st_geometry(net, "edges")
#'
#' @importFrom sf st_geometry
#' @export
st_geometry.sfnetwork = function(obj, active = NULL, focused = TRUE, ...) {
  pull_geom(obj, active, focused = focused)
}

#' @name sf_methods
#' @examples
#' # Replace the geometry of the nodes.
#' # This will automatically update edge geometries to match the new nodes.
#' newnet = net
#' newnds = rep(st_centroid(st_combine(st_geometry(net))), n_nodes(net))
#' st_geometry(newnet) = newnds
#'
#' plot(net)
#' plot(newnet)
#'
#' @importFrom cli cli_abort
#' @importFrom sf st_geometry<-
#' @export
`st_geometry<-.sfnetwork` = function(x, value) {
  if (is.null(value)) return (drop_geom(x))
  if (! have_equal_crs(x, value)) {
    cli_abort(c(
      "Replacement has a different CRS.",
      "i" = "The CRS of the replacement should equal the original CRS.",
      "i" = "You can transform to another CRS using {.fn sf::st_transform}."
    ))
  }
  if (attr(x, "active") == "nodes") {
    if (length(value) != n_nodes(x)) {
      cli_abort(c(
        "Replacement has a different number of features.",
        "i" = "The network has {n_nodes(x)} nodes, not {length(value)}."
      ))
    }
    if (! are_points(value)) {
      cli_abort(c(
        "Unsupported geometry types.",
        "i" = "Node geometries should all be {.cls POINT}."
      ))
    }
    x_new = mutate_node_geom(x, value, focused = TRUE)
    make_edges_valid(x_new)
  } else {
    if (length(value) != n_edges(x)) {
      cli_abort(c(
        "Replacement has a different number of features.",
        "i" = "The network has {n_edges(x)} edges, not {length(value)}."
      ))
    }
    if (! are_linestrings(value)) {
      cli_abort(c(
        "Unsupported geometry types.",
        "i" = "Edge geometries should all be {.cls LINESTRING}."
      ))
    }
    x_new = mutate_edge_geom(x, value, focused = TRUE)
    make_edges_valid(x_new, preserve_geometries = TRUE)
  }
}

#' @importFrom cli cli_abort
#' @importFrom igraph is_directed
#' @importFrom sf st_geometry<-
#' @importFrom tibble as_tibble
#' @export
`st_geometry<-.tbl_graph` = function(x, value) {
  if (attr(x, "active") == "edges") {
    cli_abort(c(
      "Edge geometries can not be set on {.cls tbl_graph} objects.",
      "i" = "Call {.fn tidygraph::activate} to activate nodes instead."
    ))
  }
  N = as_tibble(x, "nodes")
  st_geometry(N) = value
  x_new = tbg_to_sfn(x)
  node_data(x_new) = N
  x_new
}

#' @importFrom sf st_geometry<-
#' @importFrom tidygraph as_tbl_graph
#' @export
`st_geometry<-.igraph` = function(x, value) {
  `st_geometry<-`(as_tbl_graph(x), value)
}

#' @name sf_methods
#' @examples
#' # Drop the geometries of the edges.
#' # This returns an sfnetwork with spatially implicit edges.
#' st_drop_geometry(activate(net, "edges"))
#'
#' # Drop the geometries of the nodes.
#' # This returns a tbl_graph.
#' st_drop_geometry(net)
#'
#' @importFrom sf st_drop_geometry
#' @export
st_drop_geometry.sfnetwork = function(x, ...) {
  drop_geom(x)
}

#' @name sf_methods
#' @examples
#' # Get the bounding box of the active network element.
#' st_bbox(net)
#'
#' @importFrom sf st_bbox
#' @export
st_bbox.sfnetwork = function(obj, active = NULL, ...) {
  st_bbox(pull_geom(obj, active, focused = TRUE), ...)
}

#' @name sf_methods
#' @importFrom sf st_coordinates
#' @export
st_coordinates.sfnetwork = function(x, active = NULL, ...) {
  st_coordinates(pull_geom(x, active, focused = TRUE), ...)
}

#' @name sf_methods
#' @importFrom sf st_is
#' @export
st_is.sfnetwork = function(x, ...) {
  st_is(pull_geom(x, focused = TRUE), ...)
}

#' @name sf_methods
#' @importFrom sf st_is_valid
#' @export
st_is_valid.sfnetwork = function(x, ...) {
  st_is_valid(pull_geom(x, focused = TRUE), ...)
}

#' Extract the geometries of a sfnetwork as a S2 geography vector
#'
#' A method to convert an object of class \code{\link{sfnetwork}} into
#' \code{\link[s2]{s2_geography}} format. Use this method without the
#' .sfnetwork suffix and after loading the \pkg{s2} package.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments passed on the corresponding \code{s2} function.
#'
#' @return An object of class \code{\link[s2]{s2_geography}}.
#'
#' @name as_s2_geography
as_s2_geography.sfnetwork = function(x, focused = TRUE, ...) {
  s2::as_s2_geography(pull_geom(x, focused = focused), ...)
}

#' @name sf_methods
#' @importFrom sf st_as_s2
#' @export
st_as_s2.sfnetwork = function(x, active = NULL, focused = TRUE, ...) {
  st_as_s2(pull_geom(x, active, focused = focused), ...)
}

# =============================================================================
# Coordinates
# =============================================================================

#' @name sf_methods
#' @examples
#' # Get CRS of the network.
#' st_crs(net)
#'
#' @importFrom sf st_crs
#' @export
st_crs.sfnetwork = function(x, ...) {
  st_crs(pull_geom(x), ...)
}

#' @name sf_methods
#' @importFrom sf st_crs<- st_crs
#' @export
`st_crs<-.sfnetwork` = function(x, value) {
  if (attr(x, "active") == "edges" || has_explicit_edges(x)) {
    geom = pull_edge_geom(x)
    st_crs(geom) = value
    x = mutate_edge_geom(x, geom)
  }
  geom = pull_node_geom(x)
  st_crs(geom) = value
  mutate_node_geom(x, geom)
}

#' @name sf_methods
#' @importFrom sf st_precision
#' @export
st_precision.sfnetwork = function(x) {
  st_precision(pull_geom(x))
}

#' @name sf_methods
#' @importFrom sf st_set_precision st_precision<-
#' @export
st_set_precision.sfnetwork = function(x, precision) {
  if (attr(x, "active") == "edges" || has_explicit_edges(x)) {
    geom = pull_edge_geom(x)
    st_precision(geom) = precision
    x = mutate_edge_geom(x, geom)
  }
  geom = pull_node_geom(x)
  st_precision(geom) = precision
  mutate_node_geom(x, geom)
}

#' @name sf_methods
#' @importFrom sf st_shift_longitude
#' @export
st_shift_longitude.sfnetwork = function(x, ...) {
  change_coords(x, op = st_shift_longitude, ...)
}

#' @name sf_methods
#' @importFrom sf st_transform
#' @export
st_transform.sfnetwork = function(x, ...) {
  change_coords(x, op = st_transform, ...)
}

#' @name sf_methods
#' @importFrom sf st_wrap_dateline
#' @export
st_wrap_dateline.sfnetwork = function(x, ...) {
  change_coords(x, op = st_wrap_dateline, ...)
}

#' @name sf_methods
#' @importFrom sf st_normalize
#' @export
st_normalize.sfnetwork = function(x, ...) {
  change_coords(x, op = st_normalize, ...)
}

#' @name sf_methods
#' @importFrom sf st_zm
#' @export
st_zm.sfnetwork = function(x, ...) {
  change_coords(x, op = st_zm, ...)
}

#' @name sf_methods
#' @importFrom sf st_m_range
#' @export
st_m_range.sfnetwork = function(obj, active = NULL, ...) {
  st_m_range(pull_geom(obj, active, focused = TRUE), ...)
}

#' @name sf_methods
#' @importFrom sf st_z_range
#' @export
st_z_range.sfnetwork = function(obj, active = NULL, ...) {
  st_z_range(pull_geom(obj, active, focused = TRUE), ...)
}

change_coords = function(x, op, ...) {
  if (attr(x, "active") == "edges" || has_explicit_edges(x)) {
    geom = pull_edge_geom(x)
    new_geom = op(geom, ...)
    x = mutate_edge_geom(x, new_geom)
  }
  geom = pull_node_geom(x)
  new_geom = op(geom, ...)
  mutate_node_geom(x, new_geom)
}

# =============================================================================
# Attribute Geometry Relationships
# =============================================================================

#' @name sf_methods
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
  agr(x, active)
}

#' @name sf_methods
#' @importFrom sf st_agr<- st_agr st_as_sf
#' @export
`st_agr<-.sfnetwork` = function(x, value) {
  active = attr(x, "active")
  x_sf = st_as_sf(x, active, focused = FALSE)
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

#' @name sf_methods
#' @importFrom cli cli_warn
#' @importFrom igraph is_directed reverse_edges
#' @importFrom sf st_reverse
#' @export
st_reverse.sfnetwork = function(x, ...) {
  active = attr(x, "active")
  if (active == "edges") {
    if (is_directed(x)) {
      x = reverse_edges(x, eids = edge_ids(x)) %preserve_all_attrs% x
    }
  } else {
    cli_warn(c(
      "{.fn st_reverse} has no effect on nodes.",
      "i" = "Call {.fn tidygraph::activate} to activate edges instead."
    ))
  }
  geom_unary_ops(st_reverse, x, active,...)
}

#' @name sf_methods
#' @importFrom cli cli_warn
#' @importFrom igraph is_directed
#' @importFrom sf st_segmentize
#' @export
st_segmentize.sfnetwork = function(x, ...) {
  active = attr(x, "active")
  if (active == "edges") {
    x_new = geom_unary_ops(st_segmentize, x, active,...)
    # st_segmentize can sometimes slightly move linestring boundaries.
    # We need them to remain constant to preserve the valid network structure.
    # Therefore we have to update edge boundaries after calling st_segmentize.
    # Note that this may mean results are slightly inaccurate.
    # TODO: Do we need to warn users for this?
    if (is_directed(x)) x_new = make_edges_follow_indices(x_new)
    make_edges_valid(x_new)
  } else {
    cli_warn(c(
      "{.fn st_segmentize} has no effect on nodes.",
      "i" = "Call {.fn tidygraph::activate} to activate edges instead."
    ))
    geom_unary_ops(st_segmentize, x, active,...)
  }
}

#' @name sf_methods
#' @importFrom sf st_simplify
#' @export
st_simplify.sfnetwork = function(x, ...) {
  active = attr(x, "active")
  geom_unary_ops(st_simplify, x, active, ...)
}

#' @importFrom sf st_as_sf st_geometry
geom_unary_ops = function(op, x, active, ...) {
  x_sf = st_as_sf(x, active = active)
  d_tmp = op(x_sf, ...)
  mutate_geom(x, st_geometry(d_tmp), active = active, focused = TRUE)
}

# =============================================================================
# Join and filter
# =============================================================================

#' @name sf_methods
#' @examples
#' # Spatial join applied to the active network element.
#' net = st_transform(net, 3035)
#' codes = st_as_sf(st_make_grid(net, n = c(2, 2)))
#' codes$post_code = as.character(seq(1000, 1000 + nrow(codes) * 10 - 10, 10))
#'
#' joined = st_join(net, codes, join = st_intersects)
#' joined
#'
#' plot(net, col = "grey")
#' plot(codes, col = NA, border = "red", lty = 4, lwd = 4, add = TRUE)
#' text(st_coordinates(st_centroid(st_geometry(codes))), codes$post_code)
#'
#' plot(st_geometry(joined, "edges"))
#' plot(st_as_sf(joined, "nodes"), pch = 20, add = TRUE)
#' par(oldpar)
#'
#' @importFrom sf st_join
#' @importFrom tidygraph unfocus
#' @export
st_join.sfnetwork = function(x, y, ...) {
  x = unfocus(x)
  active = attr(x, "active")
  switch(
    active,
    nodes = spatial_join_nodes(x, y, ...),
    edges = spatial_join_edges(x, y, ...),
    raise_invalid_active(active)
  )
}

#' @name sf_methods
#' @importFrom sf st_join
#' @export
st_join.morphed_sfnetwork = function(x, y, ...) {
  x[] = lapply(x, st_join, y = y, ...)
  x
}

#' @importFrom cli cli_warn
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
    cli_warn(c(
      "{.fn st_join} for {.cls sfnetwork} objects only joins one feature per node.",
      "!" = paste(
        "Multiple matches were detected for some nodes,",
        "of which all but the first one are ignored."
      )
    ))
  }
  # If an inner join was requested instead of a left join:
  # --> This means only nodes in x that had a match in y are preserved.
  # --> The other nodes need to be removed.
  if (isTRUE(list(...)$left)) {
    keep = n_new$.sfnetwork_index
    drop = if (length(keep) == 0) orig_idxs else orig_idxs[-keep]
    x = delete_vertices(x, drop) %preserve_all_attrs% x
  }
  # Update node attributes of the original network.
  n_new$.sfnetwork_index = NULL
  node_data(x) = n_new
  x
}

#' @importFrom igraph is_directed
#' @importFrom sf st_as_sf st_join
spatial_join_edges = function(x, y, ...) {
  # Convert x and y to sf.
  x_sf = edges_as_sf(x)
  y_sf = st_as_sf(y)
  # Join with st_join.
  e_new = st_join(x_sf, y_sf, ...)
  # Create a new network with the updated data.
  x_new = sfnetwork_(nodes_as_sf(x), e_new, directed = is_directed(x))
  x_new %preserve_network_attrs% x
}

#' @name sf_methods
#' @examples
#' # Spatial filter applied to the active network element.
#' p1 = st_point(c(4151358, 3208045))
#' p2 = st_point(c(4151340, 3207520))
#' p3 = st_point(c(4151756, 3207506))
#' p4 = st_point(c(4151774, 3208031))
#'
#' poly = st_multipoint(c(p1, p2, p3, p4)) |>
#'   st_cast('POLYGON') |>
#'   st_sfc(crs = 3035) |>
#'   st_as_sf()
#'
#' filtered = st_filter(net, poly, .pred = st_intersects)
#'
#' plot(net, col = "grey")
#' plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
#' plot(filtered)
#'
#' par(oldpar)
#'
#' @importFrom sf st_filter
#' @importFrom tidygraph unfocus
#' @export
st_filter.sfnetwork = function(x, y, ...) {
  x = unfocus(x)
  active = attr(x, "active")
  switch(
    active,
    nodes = spatial_filter_nodes(x, y, ...),
    edges = spatial_filter_edges(x, y, ...),
    raise_invalid_active(active)
  )
}

#' @name sf_methods
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
  x_sf = edges_as_sf(x)
  y_sf = st_geometry(y)
  drop = find_indices_to_drop(x_sf, y_sf, ..., .operator = st_filter)
  delete_edges(x, drop) %preserve_all_attrs% x
}

#' @name sf_methods
#' @importFrom sf st_crop st_as_sfc
#' @importFrom tidygraph unfocus
#' @export
st_crop.sfnetwork = function(x, y, ...) {
  x = unfocus(x)
  if (inherits(y, "bbox")) y = st_as_sfc(y)
  active = attr(x, "active")
  switch(
    active,
    nodes = spatial_clip_nodes(x, y, ..., .operator = st_crop),
    edges = spatial_clip_edges(x, y, ..., .operator = st_crop),
    raise_invalid_active(active)
  )
}

#' @name sf_methods
#' @importFrom sf st_crop
#' @export
st_crop.morphed_sfnetwork = function(x, y, ...) {
  x[] = lapply(x, st_crop, y = y, ...)
  x
}

#' @name sf_methods
#' @importFrom sf st_difference st_as_sfc
#' @importFrom tidygraph unfocus
#' @export
st_difference.sfnetwork = function(x, y, ...) {
  x = unfocus(x)
  active = attr(x, "active")
  switch(
    active,
    nodes = spatial_clip_nodes(x, y, ..., .operator = st_difference),
    edges = spatial_clip_edges(x, y, ..., .operator = st_difference),
    raise_invalid_active(active)
  )
}

#' @name sf_methods
#' @importFrom sf st_difference
#' @export
st_difference.morphed_sfnetwork = function(x, y, ...) {
  x[] = lapply(x, st_difference, y = y, ...)
  x
}

#' @name sf_methods
#' @importFrom sf st_intersection st_as_sfc
#' @importFrom tidygraph unfocus
#' @export
st_intersection.sfnetwork = function(x, y, ...) {
  x = unfocus(x)
  active = attr(x, "active")
  switch(
    active,
    nodes = spatial_clip_nodes(x, y, ..., .operator = st_intersection),
    edges = spatial_clip_edges(x, y, ..., .operator = st_intersection),
    raise_invalid_active(active)
  )
}

#' @name sf_methods
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

#' @importFrom igraph is_directed
#' @importFrom sf st_cast st_geometry st_is st_line_merge
spatial_clip_edges = function(x, y, ..., .operator = sf::st_intersection) {
  # For this function edge geometries should follow the from/to column indices.
  # This is not by default the case in undirected networks.
  directed = is_directed(x)
  if (! directed) x = make_edges_follow_indices(x)
  # Clip the edges using the given operator.
  # Possible operators are st_intersection, st_difference and st_crop.
  e_new = .operator(edges_as_sf(x), st_geometry(y), ...)
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
  # Create a new network with the original nodes and the clipped edges.
  x_new = sfnetwork_(nodes_as_sf(x), e_new, directed = directed)
  # Boundaries of clipped edges may not match their original incident node.
  # In these cases we will add the affected edge boundary as a new node.
  # This makes sure the new network has a valid spatial network structure.
  make_edges_valid(x, preserve_geometries = TRUE)
}

find_indices_to_drop = function(x, y, ..., .operator = sf::st_filter) {
  # Add .sfnetwork_index column to keep track of original indices.
  if (".sfnetwork_index" %in% names(x)) {
    raise_reserved_attr(".sfnetwork_index")
  }
  orig_idxs = seq_len(nrow(x))
  x$.sfnetwork_index = orig_idxs
  # Filter with the given operator.
  filtered = .operator(x, y, ...)
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

#' @name sf_methods
#' @importFrom sf st_geometry st_intersects
#' @export
st_intersects.sfnetwork = function(x, y, ...) {
  if (missing(y)) {
    st_intersects(pull_geom(x), ...)
  } else {
    st_intersects(pull_geom(x), st_geometry(y), ...)
  }
}

#' @name sf_methods
#' @importFrom sf st_as_sf st_sample
#' @export
st_sample.sfnetwork = function(x, ...) {
  st_sample(st_as_sf(x), ...)
}

#' @name sf_methods
#' @importFrom sf st_geometry st_nearest_points
#' @export
st_nearest_points.sfnetwork = function(x, y, ...) {
  st_nearest_points(pull_geom(x), st_geometry(y), ...)
}

#' @name sf_methods
#' @importFrom sf st_area
#' @export
st_area.sfnetwork = function(x, ...) {
  st_area(pull_geom(x), ...)
}
