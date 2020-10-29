#' Update geometries to their nearest point on the network
#'
#' Implementation of snapping techniques that find the nearest points on a
#' spatial network to a given set of input geometries. In theory, the
#' input geometries can be of any geometry type, but it is recommended to
#' only provide geometries of type \code{POINT}, for example by first
#' calculating the centroid of other geometry types.
#'
#' @param x The spatial features to be snapped, either as object of class
#' \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @param graph The network to be snapped to as object object of class
#' \code{\link{sfnetwork}}.
#'
#' @param method The snapping technique to be used. See details. Defaults to
#' \code{'nearest_node'}.
#'
#' @param tolerance The tolerance distance to be used. Only features that are
#' at least as close to the network as the tolerance distance will be snapped.
#' For all other features, an empty geometry will be returned. Should be a
#' non-negative number preferably given as an object of class 
#' \code{\link[units]{units}}. Otherwise, it will be assumed that the unit is 
#' meters. If set to \code{Inf} all features will be snapped. Defaults to 
#' \code{Inf}.
#'
#' @return An object of class \code{\link[sf]{sfc}}, containing the snapped 
#' geometries of x.
#'
#' @details Implemented snapping techniques are.
#' * \code{'nearest_node'}: Find the nearest node to the given features.
#' * \code{'nearest_point_on_edge'}: Finds the nearest point on the nearest
#' edge to the given features. This technique requires spatially explicit edges.
#' @md
#' 
#' @export
st_snap_to_network = function(x, graph, method = "nearest_node",
                              tolerance = Inf) {
  stopifnot(have_equal_crs(x, graph))
  stopifnot(as.numeric(tolerance) >= 0)
  # Inform about sf's planar assumption if needed.
  if (will_assume_planar(x)) {
    message(
      "Although coordinates are longitude/latitude, ",
      "st_snap_to_network assumes that they are planar"
    )
  }
  # Call snapping function corresponding to given method.
  switch(
    method,
    nearest_node = nearest_node(x, graph, tolerance),
    nearest_point_on_edge = nearest_point_on_edge(x, graph, tolerance),
    stop("Unknown snapping technique: ", method, call. = FALSE)
  )
}

#' @importFrom sf st_crs st_geometry st_is_within_distance st_nearest_feature
nearest_node = function(x, graph, tolerance) {
  # Set tolerance.
  tolerance = set_snapping_tolerance(tolerance, soft = FALSE)
  # Extract node geometries from the given network.
  nodes = st_geometry(graph, "nodes")
  # For each feature p in x:
  # --> Find the nearest node q to p.
  Q = nodes[suppressMessages(st_nearest_feature(x, nodes))]
  # If tolerance is not infinite:
  # --> Return nearest node geometries only for features within tolerance.
  # --> Return empty geometries for all other features.
  if (! is.infinite(tolerance)) {
    within = lengths(st_is_within_distance(x, Q, tolerance)) > 0
    Q[!within] = empty_point(crs = st_crs(x))
  }
  Q
}

#' @importFrom sf st_boundary st_cast st_crs st_geometry st_nearest_feature 
#' st_nearest_points
nearest_point_on_edge = function(x, graph, tolerance) {
  require_spatially_explicit_edges(graph)
  # Set tolerance.
  # Allow a small deviation if tolerance = 0 to account for precision errors.
  tolerance = set_snapping_tolerance(tolerance, soft = TRUE)
  # Extract geometries.
  edges = st_geometry(graph, "edges")
  xgeom = st_geometry(x)
  # Find indices of features in x that are located:
  # --> *on* an edge.
  # --> *close* to an edge, i.e. within the tolerance but not on it.
  relative_feature_locs = locate_features(xgeom, edges, tolerance)
  on = relative_feature_locs$on
  close = relative_feature_locs$close
  # For each feature p in x that is neither on nor close to an edge:
  # --> Replace the geometry of p by an empty point.
  xgeom[! (on  | close)] = empty_point(crs = st_crs(x))
  # For each feature p in x that is close to an edge:
  # --> Find the nearest edge q to p.
  # --> Draw a line r that forms the shortest connection between p and q.
  # --> The endpoint of r is the point on q closest to p.
  # --> Replace the geometry of p with that endpoint.
  if (any(close)) {
    Q = edges[suppressMessages(st_nearest_feature(xgeom[close], edges))]
    snap = function(i) {
      p = xgeom[close][i]
      q = Q[i]
      r = suppressMessages(st_nearest_points(p, q))
      st_cast(st_boundary(r), "POINT")[2]
    }
    snaps = do.call("c", lapply(seq_along(close[close]), snap))
    xgeom[close] = snaps
  }
  xgeom
}

#' @importFrom units set_units
set_snapping_tolerance = function(x, soft = FALSE) {
  # If soft is True:
  # --> Change a tolerance of 0 to a tolerance of a very small positive number.
  # --> This is needed to solve precision errors with intersections.
  # --> See https://github.com/r-spatial/sf/issues/790
  if (soft && (as.numeric(x) == 0)) x = set_units(1e-5, "m")
  # If tolerance is not a units object:
  # --> Convert into units object assuming a units of meters.
  # --> Unless tolerance is infinite.
  if (! (is.infinite(x) || inherits(x, "units"))) x = set_units(x, "m")
  x
}

#' @importFrom sf st_intersects st_is_within_distance
locate_features = function(x, y, tolerance) {
  # Find indices of features in x that are located:
  # --> *on* a feature in y.
  intersects = suppressMessages(st_intersects(x, y))
  on = lengths(intersects) > 0
  # Find indices of features in x that are located:
  # --> *close* to a feature in y.
  # We define a feature xi being 'close' to a feature yj when: 
  # --> xi is located within a given tolerance distance from yj.
  # --> xi is not located on yj.
  if (all(on)) {
    # If all features are already on an edge.
    # --> By definition no feature is 'close'.
    close = rep(FALSE, length(on))
  } else if (is.infinite(tolerance)) {
    # If tolerance was set to infinite:
    # --> That implies there is no upper bound for what to define as 'close'.
    # --> Hence, all features that are not on an edge are 'close'.
    close = !on
  } else {
    # If a non-infinite tolerance was set:
    # --> Features are 'close' if within tolerance distance from an edge.
    # --> But not on an edge.
    within_tolerance = st_is_within_distance(x[!on], y, tolerance)
    close = !on # First assume all features that are not 'on' are 'close'.
    close[close] = lengths(within_tolerance) > 0 # Update.
  }
  list("on" = on, "close" = close)
}