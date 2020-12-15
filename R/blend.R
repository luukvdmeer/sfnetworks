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
#' @note Due to internal rounding of rational numbers, it may occur that the
#' intersection point between a line and a point is not evaluated as 
#' actually intersecting that line by the designated algorithm. Instead, the
#' intersection point lies a tiny-bit away from the edge. Therefore, it is
#' recommended to set the tolerance to a very small number (say 1e-5) even if
#' you only want to blend points that intersect the line.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' # Create a network and a set of points to blend.
#' n11 = st_point(c(0,0))
#' n12 = st_point(c(1,1))
#' e1 = st_sfc(st_linestring(c(n11, n12)), crs = 3857)
#'
#' n21 = n12
#' n22 = st_point(c(0,2))
#' e2 = st_sfc(st_linestring(c(n21, n22)), crs = 3857)
#'
#' n31 = n22
#' n32 = st_point(c(-1,1))
#' e3 = st_sfc(st_linestring(c(n31, n32)), crs = 3857)
#'
#' net = as_sfnetwork(c(e1,e2,e3))
#'
#' pts = net %>%
#'   st_bbox() %>%
#'   st_as_sfc() %>%
#'   st_sample(10, type = "random") %>%
#'   st_set_crs(3857) %>%
#'   st_cast('POINT')
#'
#' # Blend points into the network.
#' # --> By default tolerance is set to Inf
#' # --> Meaning that all points get blended
#' b1 = st_network_blend(net, pts)
#' b1
#'
#' # Blend points with a tolerance.
#' tol = units::set_units(40, "km")
#' b2 = st_network_blend(net, pts, tolerance = tol)
#' b2
#'
#' ## Plot results.
#' # Initial network and points.
#' par(mar = c(1,1,1,1), mfrow = c(1,3))
#' plot(net, cex = 2, main = "Network + set of points")
#' plot(pts, cex = 2, col = "red", pch = 20, add = TRUE)
#'
#' # Blend with no tolerance
#' plot(b1, cex = 2, main = "Blend with tolerance = Inf")
#' plot(pts, cex = 2, col = "red", pch = 20, add = TRUE)
#'
#' # Blend with tolerance.
#' within = st_is_within_distance(pts, st_geometry(net, "edges"), tol)
#' pts_within = pts[lengths(within) > 0]
#' plot(b2, cex = 2, main = "Blend with tolerance = 40 km")
#' plot(pts, cex = 2, col = "grey", pch = 20, add = TRUE)
#' plot(pts_within, cex = 2, col = "red", pch = 20, add = TRUE)
#'
#' @export
st_network_blend = function(x, y, tolerance = Inf, sort = FALSE) {
  UseMethod("st_network_blend")
}

#' @export
st_network_blend.sfnetwork = function(x, y, tolerance = Inf, sort = FALSE) {
  require_spatially_explicit_edges(x)
  stopifnot(has_single_geom_type(y, "POINT"))
  stopifnot(have_equal_crs(x, y))
  stopifnot(as.numeric(tolerance) >= 0)
  if (will_assume_planar(x)) raise_assume_planar("st_network_blend")
  blend_(x, y, tolerance, sort)
}

#' @importFrom dplyr sym
#' @importFrom rlang !! :=
#' @importFrom sf st_distance st_equals st_geometry st_intersection
#' st_intersects st_is_within_distance st_nearest_feature
#' st_nearest_points st_set_crs
#' @importFrom tidygraph arrange mutate
#' @importFrom units set_units
blend_ = function(x, y, tolerance, sort) {
    # =============
    # SET TOLERANCE
    # =============
    # If tolerance is not a units object:
    # --> Convert into units object assuming a units of meters.
    # --> Unless tolerance is infinite.
    if (! (is.infinite(tolerance) || inherits(tolerance, "units"))) {
      tolerance = set_units(tolerance, "m")
    }
    # ===============
    # LOCATE FEATURES
    # ===============
    # Extract edges.
    edges = edges_as_sf(x, "edges")
    # Extract the set of linestring geometries from the edges.
    # Extract the set of point geometries from y.
    L = st_geometry(edges)
    P = st_geometry(y)
    # Find indices of features in y that are located:
    # --> *on* an edge in x.
    intersects = suppressMessages(st_intersects(P, L))
    on = lengths(intersects) > 0
    # Find indices of features in y that are located:
    # --> *close* to an edge in x.
    # We define a feature yi being 'close' to an edge xj when:
    # --> yi is located within a given tolerance distance from xj.
    # --> yi is not located on xj.
    if (as.numeric(tolerance) == 0 | all(on)) {
      # If tolerance is 0.
      # --> By definition no feature is 'close'.
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
      within_tolerance = st_is_within_distance(P[!on], L, tolerance)
      close = !on # First assume all features that are not 'on' are 'close'.
      close[close] = lengths(within_tolerance) > 0 # Update.
    }
    # ===========
    # SPLIT EDGES
    # ===========
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
      raise_assume_constant("st_network_blend")
    }
    # For each feature p in y that is on an edge in x:
    # --> Split the edges in x by p.
    if (any(on)) {
      edges = split_lines(edges, P[on])
    }
    # For each point p in P that is close to an edge l in L:
    # --> Find the nearest edge q to p.
    # --> Draw a line r that forms the shortest connection between p and q.
    # --> Make sure r intersects with q.
    # --> Split the edges in x by r.
    if (any(close)) {
      Q = L[suppressMessages(st_nearest_feature(P[close], L))]
      connect = function(i) {
        p = P[close][i]
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
    # ============
    # POST PROCESS
    # ============
    # Spatial left join between nodes of x_new and original nodes of x.
    # This is always needed when:
    # --> Nodes of x_new need to be sorted in the same order as nodes of x.
    # In other cases, it is also needed when:
    # --> Nodes of x had attributes (these got lost when constructing x_new).
    if (sort) {
      # Add index column to nodes of x to keep track of original node indices.
      nodes = nodes_as_sf(x)
      idx_col = ".sfnetwork_node_index"
      if (idx_col %in% names(nodes)) raise_reserved_attr(idx_col)
      nodes[, idx_col] = seq_len(nrow(nodes))
      # Join original nodes spatially with the new network.
      x_new = join_nodes(x_new, nodes, join = st_equals)
      # Sort based on original node index.
      x_new = arrange(x_new, !!sym(idx_col))
      # Remove the node index column.
      x_new = mutate(x_new, !!idx_col := NULL)
    } else if (length(node_spatial_attribute_names(x)) > 0) {
      # Join original nodes spatially with the new network.
      x_new = join_nodes(x_new, nodes_as_sf(x), join = st_equals)
    }
    # Spatial left join between nodes of x_new and point features of y.
    # This is needed when:
    # --> Features of y had attributes.
    if (is.sf(y) && ncol(y) > 1) {
      # Update geometries of features in y that were snapped to an edge in x.
      if (any(close)) {
        P[close] = do.call(
          "c",
          lapply(
            seq_along(conns),
            function(i) suppressMessages(st_intersection(conns[i], Q[i]))
          )
        )
        st_geometry(y) = P
      }
      # Keep only those features in y that were blended.
      y = y[on | close, ]
      # Join spatially with the new network.
      x_new = join_nodes(x_new, y, join = sf::st_equals)
    }
    x_new %preserve_active% x
}
