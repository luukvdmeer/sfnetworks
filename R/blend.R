#' Blend spatial points into a spatial network
#'
#' Blending a point into a network is the combined process of first projecting
#' the point onto its nearest point on its nearest edge in the network, then
#' subdividing that edge at the location of the projected point, and finally
#' adding the projected point as node to the network. If the location of the
#' projected point is equal an existing node in the network, the attributes of
#' the point will be joined to that node, instead of adding a new node.
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
#' @param ignore_duplicates If there are multiple points in \code{y} that have
#' the same projected location, only the first one of them is blended into
#' the network. But what should happen with the others? If this argument is set
#' to \code{TRUE}, they will be ignored. If this argument is set to
#' \code{FALSE}, they will be added as isolated nodes to the returned network.
#' Nodes at equal locations can then be merged using the spatial morpher.
#' \code{\link{to_spatial_unique}}. Defaults to \code{TRUE}.
#'
#' @return The blended network as an object of class \code{\link{sfnetwork}}.
#'
#' @details When the projected location of a given point intersects with more
#' than one edge, it is only blended into the first of these edges. Edges are
#' not connected at blending locations. Use the spatial morpher
#' \code{\link{to_spatial_subdivision}} for that.
#'
#' To determine if a projected point is equal to an existing node, and to
#' determine if multiple projected points are equal to each other, sfnetworks
#' by default rounds coordinates to 12 decimal places. You can influence this
#' behavior by explicitly setting the precision of the network using
#' \code{\link[sf]{st_set_precision}}.
#'
#' @note Due to internal rounding of rational numbers, it may occur that the
#' intersection point between a line and a point is not evaluated as
#' actually intersecting that line by the designated algorithm. Instead, the
#' intersection point lies a tiny-bit away from the edge. Therefore, it is
#' recommended to set the tolerance to a very small number (for example 1e-5)
#' even if you only want to blend points that intersect an edge.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#'
#' # Create a spatial network.
#' n1 = st_point(c(0, 0))
#' n2 = st_point(c(1, 0))
#' n3 = st_point(c(2, 0))
#'
#' e1 = st_sfc(st_linestring(c(n1, n2)), crs = 3857)
#' e2 = st_sfc(st_linestring(c(n2, n3)), crs = 3857)
#'
#' net = as_sfnetwork(c(e1, e2))
#'
#' # Create spatial points to blend in.
#' p1 = st_sfc(st_point(c(0.5, 0.1)))
#' p2 = st_sfc(st_point(c(0.5, -0.2)))
#' p3 = st_sfc(st_point(c(1, 0.2)))
#' p4 = st_sfc(st_point(c(1.75, 0.2)))
#' p5 = st_sfc(st_point(c(1.25, 0.1)))
#'
#' pts = st_sf(foo = letters[1:5], geometry = c(p1, p2, p3, p4, p5), crs = 3857)
#'
#' # Blend all points into the network.
#' b1 = st_network_blend(net, pts)
#' b1
#'
#' plot(net)
#' plot(st_geometry(pts), pch = 20, col = "orange", add = TRUE)
#' plot(b1)
#' plot(st_geometry(pts), pch = 20, col = "orange", add = TRUE)
#'
#' # Blend points within a tolerance distance.
#' tol = units::set_units(0.1, "m")
#' b2 = st_network_blend(net, pts, tolerance = tol)
#' b2
#'
#' plot(net)
#' plot(st_geometry(pts), pch = 20, col = "orange", add = TRUE)
#' plot(b2)
#' plot(st_geometry(pts), pch = 20, col = "orange", add = TRUE)
#'
#' # Add points with duplicated projected location as isolated nodes.
#' b3 = st_network_blend(net, pts, ignore_duplicates = FALSE)
#' b3
#'
#' par(oldpar)
#'
#' @export
st_network_blend = function(x, y, tolerance = Inf, ignore_duplicates = TRUE) {
  UseMethod("st_network_blend")
}

#' @importFrom cli cli_abort
#' @importFrom tidygraph unfocus
#' @export
st_network_blend.sfnetwork = function(x, y, tolerance = Inf,
                                      ignore_duplicates = TRUE) {
  x = unfocus(x)
  if (! has_explicit_edges(x)) {
    cli_abort(c(
      "{.arg x} should have spatially explicit edges.",
      "i" = "Call {.fn sfnetworks::to_spatial_explicit} to explicitize edges."
    ))
  }
  if (! are_points(y)) {
    cli_abort("All features in {.arg y} should have {.cls POINT} geometries.")
  }
  if (! have_equal_crs(x, y)) {
    cli_abort(c(
      "{.arg x} and {.arg y} should have the same CRS.",
      "i" = "Call {.fn sf::st_transform} to transform to a different CRS."
    ))
  }
  if (! as.numeric(tolerance) >= 0) {
    cli_abort("{.arg tolerance} should be positive.")
  }
  if (will_assume_projected(x)) {
    raise_assume_projected("st_network_blend")
  }
  blend(x, y, tolerance = tolerance, ignore_duplicates = ignore_duplicates)
}

#' @importFrom cli cli_warn
#' @importFrom dplyr bind_rows left_join
#' @importFrom igraph is_directed
#' @importFrom sf st_distance st_drop_geometry st_geometry st_geometry<-
#' st_is_within_distance st_nearest_feature st_nearest_points st_precision
#' @importFrom sfheaders sfc_cast sfc_to_df
#' @importFrom units set_units
blend = function(x, y, tolerance, ignore_duplicates = TRUE) {
  # Extract the following:
  # --> The node data of x and its geometries.
  # --> The edge data of x and its geometries.
  # --> The geometries of the features to be blended.
  nodes = nodes_as_sf(x)
  edges = edges_as_sf(x)
  Y = st_geometry(y)
  # For later use:
  # --> Retrieve the name of the geometry column of the nodes in x.
  node_colname = attr(nodes, "sf_column")
  ## ===========================
  # STEP I: DECOMPOSE THE EDGES
  # Decompose the edges linestring geometries into the points that shape them.
  ## ===========================
  # Decompose edge linestrings into points.
  edge_pts = sf_to_df(edges)
  # Define the total number of edge points.
  n = nrow(edge_pts)
  # Store additional information for each edge point.
  edge_pts$pid = seq_len(n) # Unique id for each edge point.
  edge_pts$eid = edge_pts$linestring_id # Edge index for each edge point.
  # Define which edge points are boundaries.
  is_startpoint = !duplicated(edge_pts$eid)
  is_endpoint = !duplicated(edge_pts$eid, fromLast = TRUE)
  is_boundary = is_startpoint | is_endpoint
  # Store for each edge point the node index, if it is a boundary.
  edge_nids = rep(NA, n)
  edge_nids[is_boundary] = edge_incident_ids(x)
  edge_pts$nid = edge_nids
  # Store for each edge point a segment index.
  # The edge point gets the index of the segment it is the start of.
  edge_pts$sid = NA
  edge_pts$sid[!is_endpoint] = seq_len(n - nrow(edges))
  # Store for each edge point a feature index.
  # This will store the index of the feature in y it is the projection of.
  # This will be filled later, for now only store a placeholder.
  edge_pts$fid = NA
  # Clean up.
  edge_pts$sfg_id = NULL
  edge_pts$linestring_id = NULL
  ## ================================
  # STEP II: CONSTRUCT EDGE SEGMENTS
  # Create geometries for each individual edge segment.
  ## ================================
  # Subset the start points and end points of each segment.
  segment_src = edge_pts[!is_endpoint, ]
  segment_trg = edge_pts[!is_startpoint, ]
  segment_src$sid = seq_len(nrow(segment_src))
  segment_trg$sid = seq_len(nrow(segment_trg))
  # Construct the segment geometries.
  segment_pts = rbind(segment_src, segment_trg)
  segment_pts = segment_pts[order(segment_pts$sid), ]
  S = df_to_lines(segment_pts, x, id_col = "sid")
  # Store for each feature the index of its nearest segment.
  # This will be filled later, for now only store a placeholder.
  nearest = rep(NA, length(Y))
  ## ========================================
  # STEP III: SELECT FEATURES TO BE BLENDED.
  # This depends on the provided tolerance.
  ## ========================================
  # Define which features to blend.
  if (is.infinite(tolerance)) {
    # Infinite tolerance means:
    # --> All given features should be blended.
    do_blend = rep(TRUE, length(Y))
  } else {
    # Parse the tolerance.
    # If units are not explicitly specified we assume its in meters.
    if (! inherits(tolerance, "units")) {
      tolerance = set_units(tolerance, "m")
    }
    # Finite tolerance means:
    # --> Only features within tolerance distance should be blended.
    do_blend = lengths(st_is_within_distance(Y, S, tolerance)) > 0
  }
  # Subset the features.
  Y = Y[do_blend]
  # Return the network unmodified when there are no features to be blended.
  if (length(Y) == 0) {
    cli_warn(c(
      "{.fn st_network_blend} did not blend any points into the network.",
      "i" = "Increase {.arg tolerance} for a higher tolerance distance."
    ))
    return (x)
  } else {
    if (will_assume_constant(x)) raise_assume_constant("st_network_blend")
  }
  ## ============================================
  # STEP IV: PROJECT FEATURES ONTO THE NETWORK.
  # This means finding the nearest point on the nearest edge to each feature.
  ## ============================================
  # Find the nearest edge segment to each feature.
  nearest = suppressMessages(st_nearest_feature(Y, S))
  # Find the nearest point on the nearest edge to each close feature.
  # For this we can use sf::sf_nearest_points, which returns:
  # --> A straight line between feature and point if they are different.
  # --> A multipoint of feature and point if they are equal.
  # To make it easier for ourselves we cast all outputs to lines.
  # Then, the endpoint of that line is the location we are looking for.
  L = suppressMessages(st_nearest_points(Y, S[nearest], pairwise = TRUE))
  L = sfc_cast(L, "LINESTRING")
  P = linestring_end_points(L)
  # Determine if multiple features have the same projected location.
  # This features will not be blended into the network.
  # They may be added as isolated nodes afterwards, if ignore_duplicates = FALSE.
  is_duplicated = st_duplicated_points(P)
  P_dups = P[is_duplicated]
  P = P[!is_duplicated]
  nearest = nearest[!is_duplicated]
  ## =====================================================
  # STEP V: INCLUDE PROJECTED FEATURES IN EDGE GEOMETRIES
  # The projected features should be included in the edge geometries.
  # Only then we can start to subdivide the edges.
  # There are two options:
  # --> The projection already matches an interior or endpoint of an edge.
  # --> The projection does not match any interior or endpoint of an edge.
  # In case 1 we need to map the projection to the existing edge point.
  # In case 2 we need to include a new point in the edge geometry.
  ## =====================================================
  # Convert projection points into the same structure as the decomposed edges.
  p_pts = sfc_to_df(P)
  p_pts$pid = NA
  p_pts$eid = NA
  p_pts$nid = NA
  p_pts$sid = NA
  p_pts$fid = p_pts$point_id
  p_pts$sfg_id = NULL
  p_pts$point_id = NULL
  # Define a function to:
  # --> Include one or more projected features in an edge segment.
  include_in_segment = function(i) {
    # Extract the features to be included in segment i.
    fts = p_pts[which(nearest == i), ]
    fts_coords = df_to_coords(fts, st_precision(y))
    # Extract the source edge point of segment i.
    src_pid = which(edge_pts$sid == i)
    src = edge_pts[src_pid, ]
    # Extract the target edge point of segment i.
    trg_pid = src_pid + 1
    trg = edge_pts[trg_pid, ]
    # Define the position of the feature in the segment.
    if (nrow(fts) == 1) {
      # There is only one feature to be included in segment i.
      # First check if the feature matches the source.
      src_coords = df_to_coords(src, st_precision(edges))
      if (fts_coords == src_coords) {
        src$fid = fts$fid
        fts = src
      } else {
        # Then check if the feature matches the target.
        trg_coords = df_to_coords(trg, st_precision(edges))
        if (fts_coords == trg_coords) {
          trg$fid = fts$fid
          fts = trg
        } else {
          # Otherwise add the feature between the source and target.
          fts$pid = src_pid + 0.5
        }
      }
    } else {
      # There are multiple features to be included in segment i.
      # First check which of them equal source or target.
      # And which should be added as new points to the segment.
      src_coords = df_to_coords(src, st_precision(edges))
      trg_coords = df_to_coords(trg, st_precision(edges))
      equal_to_src = fts_coords == src_coords
      equal_to_trg = fts_coords == trg_coords
      not_equal = !(equal_to_src | equal_to_trg)
      # Match feature to source.
      if (any(equal_to_src)) {
        src$fid = fts$fid[equal_to_src]
        fts[equal_to_src, ] = src
      }
      # Match feature to target.
      if (any(equal_to_trg)) {
        trg$fid = fts$fid[equal_to_trg]
        fts[equal_to_trg, ] = trg
      }
      # Add feature(s) as new point(s).
      if (any(not_equal)) {
        n = sum(not_equal)
        if (n > 1) {
          # If there are multiple features to be added.
          # Determine their order based on distance to the source point.
          src_geom = df_to_points(src, edges) # Convert to sfc.
          fts_geom = df_to_points(fts[not_equal, ], y) # Convert to sfc.
          dists = st_distance(src_geom, fts_geom)
          d = 1 / (n + 1) # How much the pid should increment per feature.
          fts$pid[not_equal][order(dists)] = seq(d, d * n, d) + src_pid
        } else {
          # If there is one feature to be added.
          # Add it between the source and target points.
          fts$pid[not_equal] = src_pid + 0.5
        }
      }
    }
    # Fill the other columns.
    fts$eid = src$eid
    fts$sid = NA
    # Return the updated edge points for segment i.
    fts
  }
  # Apply the function to each segment that is nearest to a projected feature.
  new_pts = do.call("rbind", lapply(unique(nearest), include_in_segment))
  # Update the edge points data frame by integrating the updates.
  edge_pts = rbind(edge_pts, new_pts)
  edge_pts = edge_pts[!duplicated(edge_pts$pid, fromLast = TRUE), ]
  edge_pts = edge_pts[order(edge_pts$pid), ]
  # Clean up.
  rownames(edge_pts) = NULL
  edge_pts$pid = seq_len(nrow(edge_pts))
  ## ==========================================
  # STEP VI: SUBDIVIDE EDGE GEOMETRIES
  # Now we can subdivide edge geometries at each projected feature.
  # Then we need to build a linestring geometry for each new edge.
  ## ==========================================
  # Infer the new number of edge points after including the projected features.
  n = nrow(edge_pts)
  # Define where to subdivide.
  # This is at edge points that:
  # --> Match a projected feature location.
  # --> Are not already an endpoint of an edge.
  is_split = !is.na(edge_pts$fid) & is.na(edge_pts$nid)
  # Create the repetition vector:
  # --> This defines for each edge point if it should be duplicated.
  # --> A value of '1' means 'store once', i.e. don't duplicate.
  # --> A value of '2' means 'store twice', i.e. duplicate.
  # --> Split points will be part of two new edges and should be duplicated.
  reps = rep(1L, n)
  reps[is_split] = 2L
  # Create the new set of edge points by duplicating split points.
  new_edge_pts = edge_pts[rep(seq_len(n), reps), ]
  # Define the total number of new edge points.
  nn = nrow(new_edge_pts)
  # Define the new edge index of each new edge point.
  # We do so by incrementing each original edge index by 1 at each split point.
  incs = rep(0L, nn)
  incs[which(is_split) + 1:sum(is_split)] = 1L
  new_edge_ids = new_edge_pts$eid + cumsum(incs)
  # Use the new edge coordinates to create their linestring geometries.
  edge_coords = edge_pts[names(edge_pts) %in% c("x", "y", "z", "m")]
  new_edge_coords = edge_coords[rep(seq_len(n), reps), ]
  new_edge_coords$eid = new_edge_ids
  new_edge_geoms = df_to_lines(new_edge_coords, edges, "eid", select = FALSE)
  ## =====================================
  # STEP VII: CONSTRUCT THE NEW EDGE DATA
  # We now have the geometries of the new edges.
  # However, the original edge attributes got lost.
  # We will restore them by:
  # --> Adding back the attributes to edges that were not split.
  # --> Duplicating original attributes within splitted edges.
  # Beware that from and to columns will remain unchanged at this stage.
  # We will update them later.
  ## =====================================
  # Define at which new edge points a new edge starts and ends.
  is_new_startpoint = !duplicated(new_edge_ids)
  is_new_endpoint = !duplicated(new_edge_ids, fromLast = TRUE)
  # Use the original edge ids of the startpoints to copy original attributes.
  new_edges = edges[new_edge_pts$eid[is_new_startpoint], ]
  # Insert the newly constructed edge geometries.
  st_geometry(new_edges) = new_edge_geoms
  ## ======================================
  # STEP VIII: CONSTRUCT THE NEW NODE DATA
  # New nodes are added at the subdivision locations.
  ## ======================================
  # Identify and select the edge points that become a node in the new network.
  is_new_node = is_new_startpoint | is_new_endpoint
  new_node_pts = new_edge_pts[is_new_node, ]
  # Define the node indices of those nodes that are added to the network.
  is_add = is.na(new_node_pts$nid)
  add_node_pids = new_node_pts$pid[is_add]
  add_node_ids = match(add_node_pids, unique(add_node_pids)) + nrow(nodes)
  new_node_pts[is_add, ]$nid = add_node_ids
  # Construct the geometries of those nodes.
  add_node_pts = new_node_pts[is_add, ][!duplicated(add_node_ids), ]
  add_node_geoms = df_to_points(add_node_pts, nodes)
  # Construct the new node data.
  # This is done by simply binding original node data with added geometries.
  add_nodes = sfc_to_sf(add_node_geoms, colname = node_colname)
  new_nodes = bind_rows(nodes, add_nodes)
  # Join the attributes of the blended features into the new nodes.
  # This is of course only needed if the given features have attributes.
  if (is_sf(y) && ncol(y) > 1) {
    # Subset y to contain only attributes (not geometries) of blended features.
    y_blend = st_drop_geometry(y)
    y_blend = y_blend[do_blend, , drop = FALSE][!is_duplicated, , drop = FALSE]
    # Subset the node points data frame to contain each node only once.
    new_nodes_df = new_node_pts[!duplicated(new_node_pts$nid), ]
    # Add an index column to match nodes to features.
    if (".sfnetwork_index" %in% c(names(nodes), names(y))) {
      raise_reserved_attr(".sfnetwork_index")
    }
    y_blend$.sfnetwork_index = seq_len(nrow(y_blend))
    new_nodes$.sfnetwork_index = new_nodes_df$fid[order(new_nodes_df$nid)]
    # Join attributes of blended features with the new nodes table.
    new_nodes = left_join(new_nodes, y_blend, by = ".sfnetwork_index")
    new_nodes$.sfnetwork_index = NULL
    # Add features with duplicated projection locations if requested.
    if (!ignore_duplicates && any(is_duplicated)) {
      y_dups = y[do_blend, , drop = FALSE][is_duplicated, , drop = FALSE]
      st_geometry(y_dups) = P_dups
      st_geometry(y_dups) = node_colname # Use correct name.
      new_nodes = bind_rows(new_nodes, y_dups)
    }
  } else {
    # Add features with duplicated projection locations if requested.
    if (!ignore_duplicates && any(is_duplicated)) {
      y_dups = sfc_to_sf(P_dups, colname = node_colname)
      new_nodes = bind_rows(new_nodes, y_dups)
    }
  }
  ## ==================================================
  # STEP IX: UPDATE FROM AND TO INDICES OF NEW EDGES
  # Now we constructed the new node data with updated node indices.
  # Therefore we need to update the from and to columns of the edges as well.
  ## ==================================================
  new_edges$from = new_node_pts$nid[is_new_startpoint[is_new_node]]
  new_edges$to = new_node_pts$nid[is_new_endpoint[is_new_node]]
  ## ============================
  # STEP X: RECREATE THE NETWORK
  # Use the new nodes data and the new edges data to create the new network.
  ## ============================
  x_new = sfnetwork_(new_nodes, new_edges, directed = is_directed(x))
  x_new %preserve_network_attrs% x
}
