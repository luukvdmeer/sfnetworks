#' Update geometries to their nearest point on the network
#'
#' Implementation of snapping techniques that find the nearest point on a
#' spatial network to a given set of input geometries.
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
#' @details Implemented snapping techniques are.
#' * \code{'nearest_node'}: Find the nearest node to the given features.
#' * \code{'nearest_point_on_edge'}: Finds the nearest point on the nearest
#' edge to the given features. This technique requires spatially explicit edges.
#' @md
#' 
#' @return An object of class \code{\link[sf]{sfc}}, containing the snapped 
#' geometries of x.
#' 
#' @export
st_snap_to_network = function(x, graph, method = "nearest_node",
                              tolerance = Inf) {
  stopifnot(have_equal_crs(x, graph))
  stopifnot(has_single_geom_type(x, "POINT"))
  stopifnot(as.numeric(tolerance) >= 0)
  if (will_assume_planar(x)) {
    message(
      "Although coordinates are longitude/latitude, ",
      "st_snap_to_network assumes that they are planar"
    )
  }
  # Set tolerance.
  if (! (is.infinite(tolerance) || inherits(tolerance, "units"))) {
    tolerance = units::set_units(tolerance, "m")
  }
  # Call snapping function corresponding to given method.
  switch(
    method,
    nearest_node = nearest_node(x, graph, tolerance),
    nearest_point_on_edge = nearest_point_on_edge(x, graph, tolerance),
    stop("Unknown snapping technique: ", method, call. = FALSE)
  )
}

#' @importFrom pbapply pblapply
#' @importFrom sf st_crs st_intersects st_is_within_distance st_nearest_feature
nearest_node = function(x, graph, tolerance) {
  # Extract node geometries from the given network.
  nodes = st_geometry(graph, "nodes")
  # For each feature p in x:
  # --> Find the nearest node q to p.
  Q = nodes[suppressMessages(sf::st_nearest_feature(x, nodes))]
  # If tolerance is not infinite:
  # --> Return nearest node geometries only for features within tolerance.
  # --> Return empty geometries for all other features.
  if (! is.infinite(tolerance)) {
    within = lengths(sf::st_is_within_distance(x, Q, tolerance)) > 0
    Q[!within] = empty_point(crs = sf::st_crs(x))
  }
  Q
}

#' @importFrom sf st_boundary st_cast st_crs st_geometry st_intersects 
#' st_is_within_distance st_nearest_feature st_nearest_points
nearest_point_on_edge = function(x, graph, tolerance) {
  require_spatially_explicit_edges(graph)
  # Extract geometries.
  edges = st_geometry(graph, "edges")
  xgeom = sf::st_geometry(x)
  # Find indices of features in x that are located:
  # --> *on* their nearest edge.
  intersects = suppressMessages(sf::st_intersects(xgeom, edges))
  on = lengths(intersects) > 0
  # Find indices of features in x that are located:
  # --> *close* to their nearest edge, i.e. within the tolerance but not on it.
  if ((as.numeric(tolerance) == 0) || all(on)) {
    # If tolerance was set to zero:
    # --> That implies to only snap features that are already on edges.
    # --> No other feature is within the tolerance.
    # --> Hence, no feature is 'close'.
    # If all features are alread on an edge.
    # --> No feature is 'close'.
    close = rep(FALSE, length(on))
  } else if (is.infinite(tolerance)) {
    # If tolerance was set to infinite:
    # --> That implies there is no upper bound for what to define as 'close'.
    # --> Hence, all features that are not 'on' are 'close'.
    close = !on
  } else {
    # If a positive tolerance (but not infinite) was set:
    # --> Features are 'close' if within tolerance distance from any edge.
    # --> But not on an edge.
    within_tolerance = sf::st_is_within_distance(xgeom[!on], edges, tolerance)
    close = !on # First assume all features that are not 'on' are 'close'.
    close[close] = lengths(within_tolerance) > 0 # Update. 
  }
  # For each feature p in x that is close to an edge:
  # --> Find the nearest edge q to p.
  # --> Draw a line r that forms the shortest connection between p and q.
  # --> The endpoint of r is the point on q closest to p.
  # --> Replace the geometry of p with that endpoint.
  if (any(close)) {
    Q = edges[suppressMessages(sf::st_nearest_feature(xgeom[close], edges))]
    snap = function(i) {
      p = xgeom[close][i]
      q = Q[i]
      r = suppressMessages(sf::st_nearest_points(p, q))
      sf::st_cast(sf::st_boundary(r), "POINT")[2]
    }
    xgeom[close] = do.call("c", pbapply::pblapply(seq_along(close[close]), snap))
  }
  # Replace features that are not within tolerance by empty geometries.
  xgeom[! (on  | close)] = empty_point(crs = sf::st_crs(x))
  # Return the updated geometries.
  xgeom
}