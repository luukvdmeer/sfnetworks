#' Blend geospatial points into a spatial network
#'
#' Blending a point into a network is the combined process of first snapping 
#' the given point to its nearest point on its nearest edge in the network, 
#' subsequently splitting that edge at the location of the snapped point, and
#' finally adding the snapped point as node to the network. If the location
#' of the snapped point is already a node in the network, the attributes of the
#' point (if any) will be joined to that node.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y The spatial features to be blended, either as object of class
#' \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}, with \code{POINT} geometries.
#' 
#' @param tolerance The tolerance distance to be used. Only features that are
#' at least as close to the network as the tolerance distance will be blended.
#' Should be a non-negative number preferably given as an object of class 
#' \code{\link[units]{units}}. Otherwise, it will be assumed that the unit is 
#' meters. If set to \code{Inf} all features will be blended. Defaults to 
#' \code{Inf}.
#'
#' @param sort Should the nodes in the resulting network be sorted in the same 
#' order as those in x, followed by the new nodes blended in from y? Defaults 
#' to \code{FALSE}, meaning that node order might be changed. However, sorting
#' might influence performance.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#' 
#' @export
st_blend = function(x, y, tolerance = Inf, sort = FALSE) {
  UseMethod("st_blend")
}

#' @importFrom dplyr sym
#' @importFrom rlang !! :=
#' @importFrom sf st_as_sf st_distance st_equals st_geometry st_intersection
#' st_join st_nearest_feature st_nearest_points st_set_crs
#' @importFrom tidygraph arrange mutate
#' @export
st_blend.sfnetwork = function(x, y, tolerance = Inf, sort = FALSE) {
  require_spatially_explicit_edges(x)
  stopifnot(has_single_geom_type(y, "POINT"))
  stopifnot(have_equal_crs(x, y))
  stopifnot(as.numeric(tolerance) >= 0)
  if (will_assume_planar(x)) raise_assume_planar("st_blend")
  # Set tolerance.
  # Allow a small deviation if tolerance = 0 to account for precision errors.
  tolerance = set_snapping_tolerance(tolerance, soft = TRUE)
  # Extract edges.
  edges = st_as_sf(x, "edges")
  # Extract geometries of the edges and of y.
  xgeom = st_geometry(edges)
  ygeom = st_geometry(y)
  # Find indices of features in y that are located:
  # --> *on* an edge.
  # --> *close* to an edge, i.e. within the tolerance but not on it.
  relative_feature_locs = locate_features(ygeom, xgeom, tolerance)
  on = relative_feature_locs$on
  close = relative_feature_locs$close
  # If there are neither features in y on or close to any edge in x:
  # --> Return x without splitting anything.
  # --> Give a warning.
  if (! any(on | close)) {
    warning(
      "No edges splitted. Increase the tolerance distance?",
      call. = FALSE
    )
    return (x)
  } else {
    raise_assume_constant("st_blend")
  }
  # For each feature p in y that is on an edge in x:
  # --> Split the edges in x by p.
  if (any(on)) {
    edges = split_lines(edges, ygeom[on])
  }
  # For each feature p in y that is close to an edge in x:
  # --> Find the nearest edge q to p.
  # --> Draw a line r that forms the shortest connection between p and q.
  # --> Make sure r intersects with q.
  # --> Split the edges in x by r.
  if (any(close)) {
    Q = xgeom[suppressMessages(st_nearest_feature(ygeom[close], xgeom))]
    connect = function(i) {
      p = ygeom[close][i]
      q = Q[i]
      r = suppressMessages(st_nearest_points(p, q))
      # If r does not intersect with q:
      # --> That means the created connection is an invalid connection
      # --> This occurs because of precision issues.
      # --> See https://github.com/r-spatial/sf/issues/790
      # --> To solve it:
      # --> Extend r in the same direction by 2x the distance d between r and q.
      # --> Assume Euclidean space for convenience.
      # --> This should guarantee intersection between r and q.
      d = st_distance(st_set_crs(r, NA), st_set_crs(q, NA))
      extend_line(r, 2 * d)
    }
    conns = do.call("c", lapply(seq_along(close[close]), connect))
    edges = split_lines(edges, conns)
  }
  # Construct a new network from scratch with the splitted edges.
  edges$from = NULL
  edges$to = NULL
  x_new = as_sfnetwork(edges)
  #
  # ===============
  # Post processing
  # ===============
  #
  # Spatial left join between nodes of x_new and original nodes of x. 
  # This is always needed when:
  # --> Nodes of x_new need to be sorted in the same order as nodes of x.
  # In other cases, it is also needed when:
  # --> Nodes of x had attributes (these got lost when constructing x_new).
  if (sort) {
    # Add index column to nodes of x to keep track of original node indices.
    orig_nodes = st_as_sf(x, "nodes")
    idx_col = ".sfnetwork_node_index"
    if (idx_col %in% names(orig_nodes)) raise_reserved_attr(idx_col)
    orig_nodes[, idx_col] = seq_len(nrow(orig_nodes))
    # Join original nodes spatially with the new network.
    x_new = st_join(x_new, orig_nodes, join = st_equals)
    # Sort based on original node index.
    x_new = arrange(x_new, !!sym(idx_col))
    # Remove the node index column.
    x_new = mutate(x_new, !!idx_col := NULL)
  } else if (length(node_spatial_attribute_names(x)) > 0) {
    # Join original nodes spatially with the new network.
    x_new = st_join(x_new, st_as_sf(x, "nodes"), join = st_equals)
  }
  # Spatial left join between nodes of x_new and point features of y.
  # This is needed when:
  # --> Features of y had attributes.
  if (is.sf(y) && ncol(y) > 1) {
    # Update geometries of features in y that were snapped to an edge in x.
    if (any(close)) {
      ygeom[close] = do.call(
        "c",
        lapply(
          seq_along(conns), 
          function(i) suppressMessages(st_intersection(conns[i], Q[i]))
        )
      )
      st_geometry(y) = ygeom
    }
    # Keep only those features in y that were blended.
    y = y[on | close, ]
    # Join spatially with the new network.
    x_new = st_join(x_new, y, join = sf::st_equals)
  }
  x_new %preserve_active% x
}