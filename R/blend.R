#' @importFrom sf st_as_sf st_distance st_equals st_geometry st_intersection
#' st_join st_nearest_feature st_nearest_points st_set_crs
#' @export
st_blend = function(x, y, tolerance = Inf) {
  require_spatially_explicit_edges(x)
  stopifnot(has_single_geom_type(y, "POINT"))
  stopifnot(have_equal_crs(x, y))
  stopifnot(as.numeric(tolerance) >= 0)
  # Inform about sf's planar assumption if needed.
  if (will_assume_planar(x)) {
    message(
      "Although coordinates are longitude/latitude, ",
      "st_blend assumes that they are planar"
    )
  }
  # Set tolerance.
  # Allow a small deviation if tolerance = 0 to account for precision errors.
  tolerance = set_snapping_tolerance(tolerance, soft = TRUE)
  # Extract edges.
  edges = sf::st_as_sf(x, "edges")
  # Extract geometries of the edges and of y.
  xgeom = sf::st_geometry(edges)
  ygeom = sf::st_geometry(y)
  # Find indices of features in x that are located:
  # --> *on* an edge.
  # --> *close* to an edge, i.e. within the tolerance but not on it.
  relative_feature_locs = locate_features(ygeom, xgeom)
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
    Q = xgeom[suppressMessages(sf::st_nearest_feature(ygeom[close], xgeom))]
    connect = function(i) {
      p = ygeom[close][i]
      q = Q[i]
      r = suppressMessages(sf::st_nearest_points(p, q))
      # If r does not intersect with q:
      # --> That means the created connection is an invalid connection
      # --> This occurs because of precision issues.
      # --> See https://github.com/r-spatial/sf/issues/790
      # --> To solve it:
      # --> Extend r in the same direction by 2x the distance d between r and q.
      # --> Make this extension CRS independent.
      # --> This should guarantee intersection between r and q.
      d = sf::st_distance(sf::st_set_crs(r, NA), sf::st_set_crs(q, NA))
      extend_line(r, 2 * d)
    }
    conns = do.call("c", lapply(seq_along(close[close]), connect))
    edges = split_lines(edges, conns)
  }
  # Construct a new network from scratch with the splitted edges.
  x_new = as_sfnetwork(edges)
  # Join attributes from x back in.
  if (length(node_spatial_attribute_names(x)) > 0) {
    # Extract nodes from the original network.
    nodes = sf::st_as_sf(x, "nodes")
    # Join spatially with the new network.
    x_new = sf::st_join(x_new, nodes, join = sf::st_equals)
  }
  # Join attributes from y in.
  if (is.sf(y) && ncol(y) > 1) {
    # Update geometries for features in y that were connected to an edge in x.
    ygeom[close] = do.call(
      "c",
      lapply(
        seq_along(conns), 
        function(i) suppressMessages(sf::st_intersection(conns[i], Q[i]))
      )
    )
    sf::st_geometry(y) = ygeom
    # Keep only those features in y that are either on or close to an edge in x.
    y = y[on | close, ]
    # Join spatially with the new network.
    x_new = sf::st_join(x_new, y, join = sf::st_equals)
  }
  x_new %preserve_active% x
}