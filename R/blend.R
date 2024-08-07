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
#' @return The blended network as an object of class \code{\link{sfnetwork}}.
#'
#' @details There are two important details to be aware of. Firstly: when the
#' snap locations of multiple points are equal, only the first of these points
#' is blended into the network. By arranging \code{y} before blending you can
#' influence which (type of) point is given priority in such cases.
#' Secondly: when the snap location of a point intersects with multiple edges,
#' it is only blended into the first of these edges. You might want to run the
#' \code{\link{to_spatial_subdivision}} morpher after blending, such that
#' intersecting but unconnected edges get connected.
#'
#' @note Due to internal rounding of rational numbers, it may occur that the
#' intersection point between a line and a point is not evaluated as
#' actually intersecting that line by the designated algorithm. Instead, the
#' intersection point lies a tiny-bit away from the edge. Therefore, it is
#' recommended to set the tolerance to a very small number (for example 1e-5)
#' even if you only want to blend points that intersect the line.
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
#' tol = units::set_units(0.2, "m")
#' b2 = st_network_blend(net, pts, tolerance = tol)
#' b2
#'
#' ## Plot results.
#' # Initial network and points.
#' oldpar = par(no.readonly = TRUE)
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
#' plot(b2, cex = 2, main = "Blend with tolerance = 0.2 m")
#' plot(pts, cex = 2, col = "grey", pch = 20, add = TRUE)
#' plot(pts_within, cex = 2, col = "red", pch = 20, add = TRUE)
#' par(oldpar)
#'
#' @export
st_network_blend = function(x, y, tolerance = Inf) {
  UseMethod("st_network_blend")
}

#' @importFrom cli cli_abort
#' @export
st_network_blend.sfnetwork = function(x, y, tolerance = Inf) {
  if (! has_explicit_edges(x)) {
    cli_abort(c(
      "{.arg x} should have spatially explicit edges",
      "i" = "Call {.fn sfnetworks::to_spatial_explicit} to explicitize edges."
    ))
  }
  if (! has_single_geom_type(y, "POINT")) {
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
  blend_(x, y, tolerance)
}

#' @importFrom dplyr bind_rows full_join
#' @importFrom igraph is_directed
#' @importFrom sf st_as_sf st_cast st_crs st_crs<- st_distance st_equals
#' st_geometry st_geometry<- st_intersects st_is_within_distance
#' st_nearest_feature st_nearest_points st_precision st_precision<-
#' @importFrom sfheaders sfc_linestring sfc_to_df
#' @importFrom units set_units
blend_ = function(x, y, tolerance) {
  # Extract the following:
  # --> The node data of x and its geometries.
  # --> The edge data of x and its geometries.
  # --> The geometries of the features to be blended.
  nodes = nodes_as_sf(x)
  edges = edges_as_sf(x)
  N = st_geometry(nodes)
  E = st_geometry(edges)
  Y = st_geometry(y)
  # For later use:
  # --> Check wheter x is directed.
  # --> Count the number of nodes in x.
  # --> Retrieve the name of the geometry column of the nodes in x.
  directed = is_directed(x)
  ncount = n_nodes(x)
  geom_colname = attr(nodes, "sf_column")
  ## ===========================
  # STEP I: PARSE THE TOLERANCE
  # If tolerance is not a units object:
  # --> Convert into units object assuming a units of meters.
  # --> Unless tolerance is infinite.
  ## ===========================
  if (! (is.infinite(tolerance) || inherits(tolerance, "units"))) {
    tolerance = set_units(tolerance, "m")
  }
  ## ================================
  # STEP II: DEFINE SPATIAL RELATIONS
  # Relate each feature in y to the edges of x by checking if:
  # --> The feature in y is located *on* an edge in x.
  # --> The feature in y is located *close* to an edge in x.
  # With *on* being defined as:
  # --> Intersecting with the edge.
  # With *close* being defined as:
  # --> Within the tolerance distance from an edge.
  # --> But not intersecting with that edge.
  ## ================================
  # Find indices of features in y that are located:
  # --> *on* an edge in x.
  intersects = suppressMessages(st_intersects(Y, E))
  is_on = lengths(intersects) > 0
  # Find indices of features in y that are located:
  # --> *close* to an edge in x.
  # We define a feature yi being *close* to an edge xj when:
  # --> yi is located within a given tolerance distance from xj.
  # --> yi is not located on xj.
  if (as.numeric(tolerance) == 0 | all(is_on)) {
    # If tolerance is 0.
    # --> By definition no feature is *close*.
    # If all features are already *on* an edge.
    # --> By definition no feature is *close*.
    is_close = rep(FALSE, length(is_on))
  } else if (is.infinite(tolerance)) {
    # If tolerance was set to infinite:
    # --> That implies there is no upper bound for what to define as *close*.
    # --> Hence, all features that are not *on* are *close*.
    is_close = !is_on
  } else {
    # If a non-infinite tolerance was set:
    # --> Features are *close* if within tolerance distance from an edge.
    # --> But not *on* an edge.
    is_within = st_is_within_distance(Y[!is_on], E, tolerance)
    is_close = !is_on
    is_close[is_close] = lengths(is_within) > 0
  }
  ## =======================
  # STEP III: SNAP FEATURES
  # We need to "project" the features in y onto the edges of the network.
  # This is also called "snapping".
  # The geometries of the *on* features in y do not have to be changed.
  # Since they already are located on an edge geometry.
  # The geometries of the *close* features in y should be replaced by:
  # --> Their nearest point on their nearest edge.
  ## =======================
  if (any(is_close)) {
    # Find the nearest edge to each close feature.
    A = suppressMessages(st_nearest_feature(Y[is_close], E))
    # Find the nearest point on the nearest edge to each close feature.
    # st_nearest_points returns a straight line between two features.
    # Hence, the endpoint of that line is the location we are looking for.
    B = suppressMessages(st_nearest_points(Y[is_close], E[A], pairwise = TRUE))
    B = linestring_boundary_points(B)
    B = B[seq(2, length(B), 2)]
    # Replace the geometries of the *close* features.
    Y[is_close] = B
  }
  ## ========================
  # STEP IV: SUBSET FEATURES
  # Subset the features in y by removing those that:
  # --> Are neither *on* nor *close* to an edge in x.
  # --> Are duplicated.
  ## ========================
  # Keep only features that are *on* or *close*.
  Y = Y[is_on | is_close]
  # Return x when there are no features left to be blended.
  if (length(Y) == 0) {
    warning(
      "No points were blended. Increase the tolerance distance?",
      call. = FALSE
    )
    return (x)
  } else {
    if (will_assume_constant(x)) raise_assume_constant("st_network_blend")
  }
  # Remove duplicated features in y.
  # These features will have the same blending location.
  # Only one point can be blended per location.
  is_duplicated = st_duplicated(Y)
  Y = Y[!is_duplicated]
  ## ==========================================
  # STEP V: INCLUDE FEATURES IN EDGE GEOMETRIES
  # The snapped features in y should be included in the edge geometries.
  # Only then we can start to split the edges.
  # There are two options:
  # --> The feature already matches an interior or endpoint of an edge.
  # --> The feature does not match any interior or endpoint of an edge.
  # In the first case we need to map the feature to the edge point.
  # In the second case we also need to include a new point in the edge.
  ## ==========================================
  # Decompose the edge geometries into their points.
  # Map each of these points to the index of its "parent edge".
  edge_pts = st_cast(E, "POINT")
  pts_idxs = rep(seq_along(E), lengths(E) / 2)
  # Define for each snapped feature in y which edge point it equals.
  # If it equals more than one edge point, only the first match is taken.
  # Since blending only blends a feature into a single edge.
  matches = do.call("c", lapply(st_equals(Y, edge_pts), `[`, 1))
  # Define which snapped features in y:
  # --> Are actually equal to an edge point.
  # --> Are not equal to any edge point.
  real_matches = which(!is.na(matches))
  na_matches = which(is.na(matches))
  # Convert the edge points object into a dataframe.
  # As additional information, we will store for each edge point:
  # --> The index of its edge.
  # --> The index of the snapped feature in y that equals it, if any.
  # --> The row index.
  edge_pts = data.frame(
    geom = edge_pts,
    edge_id = pts_idxs,
    feat_id = NA,
    row_id = seq_along(edge_pts)
  )
  # Add the indices of the snapped features in y that equal an edge point.
  if (length(real_matches) > 0) {
    edge_pts$feat_id[matches[real_matches]] = real_matches
  }
  # Include the locations of the other snapped features as an edge point.
  if (length(na_matches) > 0) {
    # First we need to define where to include the feature geometries.
    # For that we need to subdivide the edge geometries into their segments.
    # A segment is the part of an edge between two edge points.
    # Hence: decompose the edge geometries into their segments.
    edge_sgs = linestring_segments(E)
    # Map each of these segments to the index of its "parent edge".
    sgs_idxs = rep(seq_along(E), lengths(E) / 2 - 1)
    # Define for each segment its position within its "parent edge".
    # Hence, the first segment within an edge gets a 1, etc.
    sgs_psns = do.call("c", lapply(rle(sgs_idxs)$lengths, seq_len))
    # Now we find for each feature its nearest segment.
    # Then we know exactly where to include the feature geometry.
    nearest = suppressMessages(st_nearest_feature(Y[na_matches], edge_sgs))
    # Include the features by looping over the identified nearest segments.
    # If only a single feature needs to be included in that segment:
    # --> Add that feature at the right position in the edge points table.
    # If multiple features need to be included in a single segment:
    # --> Order these features by distance to the startpoint of the segment.
    # --> Add them at the right position in the edge points table.
    include = function(i) {
      # Retrieve the following with respect to the current segment:
      # --> The index of the edge of which the segment is part.
      # --> The index of the edge point at the start of the segment.
      # --> The indices of the features for which this segment is nearest.
      edge_id = sgs_idxs[i]
      src_id = which(pts_idxs == edge_id)[sgs_psns[i]]
      feat_idxs = na_matches[which(nearest == i)]
      # If there are multiple features for which this segment is nearest:
      # --> Order them by distance to the startpoint of the segment.
      n = length(feat_idxs)
      if (n > 1) {
        feats = Y[feat_idxs]
        point = edge_pts$geom[src_id]
        dists = st_distance(point, feats)
        feat_idxs = feat_idxs[order(dists)]
      }
      # Define where to insert the features in the edge points table.
      # This is directly after the startpoint of the segment.
      # The row indices of the features should be a value between:
      # --> The row index of the startpoint of the segment.
      # --> The row index of the endpoint of the segment.
      # Recall that the latter is the startpoint index plus 1.
      # Hence, for the features to be inserted we need:
      # --> A value between 0 and 1 added to the segment startpoint index.
      # If there are multiple features, their order should be preserved.
      stepsize = 1 / (n + 1)
      values = seq(stepsize, stepsize * n, stepsize)
      row_idxs = values + src_id
      # Return in the same format as the edge points table.
      data.frame(
        geom = Y[feat_idxs],
        edge_id = rep(edge_id, n),
        feat_id = feat_idxs,
        row_id = row_idxs
      )
    }
    new_pts = do.call("rbind", lapply(unique(nearest), include))
    edge_pts = bind_rows(edge_pts, new_pts)
    edge_pts = edge_pts[order(edge_pts$row_id), ]
  }
  ## =============================
  # STEP V: SPLIT EDGE GEOMETRIES
  # New nodes should be added for snapped features of y whenever:
  # --> There is not an existing node yet at that location.
  # The edges should be splitted at the locations of these new nodes.
  ## =============================
  # First, we define where to split the edges. This is at edge points that:
  # --> Are equal to a snapped feature in y.
  # --> Are *not* already an edge boundary.
  is_startpoint = !duplicated(edge_pts$edge_id)
  is_endpoint = !duplicated(edge_pts$edge_id, fromLast = TRUE)
  is_boundary = is_startpoint | is_endpoint
  is_split = !is.na(edge_pts$feat_id) & !is_boundary
  # Create a repetition vector:
  # --> This defines for each edge point if it should be duplicated.
  # --> A value of '1' means 'store once', i.e. don't duplicate.
  # --> A value of '2' means 'store twice', i.e. duplicate.
  # --> Split points will be part of two new edges and should be duplicated.
  reps = rep(1L, nrow(edge_pts))
  reps[is_split] = 2L
  # Extract a coordinate data frame from the edge points.
  # Apply the repitition vector to this data frame.
  # This gives us the coordinates of the new edge points.
  edge_coords = sfc_to_df(edge_pts$geom)
  edge_coords = edge_coords[names(edge_coords) %in% c("x", "y", "z", "m")]
  new_edge_coords = data.frame(lapply(edge_coords, rep, reps))
  # Apply the repetition vector also to the edge indices of the edge points.
  # This gives us the *original* edge index of the new edge points.
  orig_edge_idxs = rep(edge_pts$edge_id, reps)
  # Update these original edge indices according to the splits.
  # Remember that edges are splitted at each split point.
  # That is: a new edge originates from each split point.
  # Hence, to get the new edge indices:
  # --> Increment each original edge index by 1 at each split point.
  incs = integer(nrow(new_edge_coords)) # By default don't increment.
  incs[which(is_split) + seq_len(sum(is_split))] = 1L # Add 1 after each split.
  new_edge_idxs = orig_edge_idxs + cumsum(incs)
  new_edge_coords$edge_id = new_edge_idxs
  # Build the new edge geometries.
  new_edge_geoms = sfc_linestring(new_edge_coords, linestring_id = "edge_id")
  st_crs(new_edge_geoms) = st_crs(edges)
  st_precision(new_edge_geoms) = st_precision(edges)
  new_edge_coords$edge_id = NULL
  ## ================================
  # STEP VI: RESTORE EDGE ATTRIBUTES
  # We now have the geometries of the new edges.
  # However, the original edge attributes got lost.
  # We will restore them by:
  # --> Adding back the attributes to edges that were not split.
  # --> Duplicating original attributes within splitted edges.
  # Beware that from and to columns will remain unchanged at this stage.
  # We will update them later.
  ## ================================
  # First, we find which *original* edge belongs to which *new* edge:
  # --> Use the lists of edge indices mapped to the new edge points.
  # --> There we already mapped each new edge point to its original edge.
  # --> First define which new edge points are startpoints of new edges.
  # --> Then retrieve the original edge index from these new startpoints.
  # --> This gives us a single original edge index for each new edge.
  is_new_startpoint = !duplicated(new_edge_idxs)
  orig_edge_idxs = orig_edge_idxs[is_new_startpoint]
  # Duplicate original edge data whenever needed.
  new_edges = edges[orig_edge_idxs, ]
  # Set the new edge geometries as geometries of these new edges.
  st_geometry(new_edges) = new_edge_geoms
  ## =================================================
  # STEP VII: UPDATE FROM AND TO INDICES OF NEW EDGES
  # Now we have:
  # --> Constructed new edge geometries.
  # --> Duplicated edge attributes wherever needed.
  # Still left to do is updating the from and to indices of the new edges.
  # They should match with the indices of the new nodes in the network.
  # The new nodes are a combination of:
  # --> Already existing nodes.
  # --> New nodes that are going to be added at split points.
  ## =================================================
  # Map each of the original edge points to the index of an original node.
  # Edge points that do no equal an original node get assigned NA.
  edge_pts$node_id = rep(NA, nrow(edge_pts))
  if (directed) {
    edge_pts[is_boundary, ]$node_id = edge_boundary_node_indices(x)
  } else {
    edge_pts[is_boundary, ]$node_id = edge_boundary_point_indices(x)
  }
  # Update this vector of original node indices by:
  # --> Adding a new, unique node index to each of the split points.
  # --> Applying the repetition vector to map them to the new edge points.
  new_node_idxs = edge_pts$node_id
  added_node_idxs = c((ncount + 1):(ncount + sum(is_split)))
  new_node_idxs[is_split] = added_node_idxs
  new_node_idxs = rep(new_node_idxs, reps)
  # Drop NA values from this vector of new node indices.
  # Recall that NA values belong to edge points that do not equal a node.
  # After dropping them we are left with an index vector of the form:
  # --> [source node edge 1, target node edge 1, source node edge 2, ...]
  new_node_idxs = new_node_idxs[!is.na(new_node_idxs)]
  # Define for each of the indices if it belongs to a source node.
  is_source = rep(c(TRUE, FALSE), length(new_node_idxs) / 2)
  # Update the from and to columns of the new edges accordingly.
  new_edges$from = new_node_idxs[is_source]
  new_edges$to = new_node_idxs[!is_source]
  ## ==================================================
  # STEP VIII: JOIN THE NODES WITH THE BLENDED FEATURES
  # The blended features of y are either:
  # --> Matched to an already existing node.
  # --> A new node in the network.
  # In the first case:
  # --> Their attributes (if any) should be joined with the existing nodes.
  # In the second case:
  # --> They should be binded to the already existing nodes.
  ## ==================================================
  # When a snapped feature in y matched a original node of x:
  # --> Get the index of both the feature and the node.
  is_match = is_boundary & !is.na(edge_pts$feat_id)
  matched_node_idxs = edge_pts$node_id[is_match]
  matched_feat_idxs = edge_pts$feat_id[is_match]
  # When a snapped feature in y is a new node of x:
  # --> Get the index of that feature.
  is_new = is_split
  new_feat_idxs = edge_pts$feat_id[is_new]
  # Join the orignal node data and the blended features.
  # Different scenarios require a different approach.
  if (is_sf(y) && ncol(y) > 1) {
    # Scenario I: the features in y have attributes.
    # This requires:
    # --> A full join between the original node data and the features.
    # First, subset y to keep only those features that were blended.
    y = y[is_on | is_close, ]
    y = y[!is_duplicated, ]
    # Add an index column matching the features in y to their new node index.
    y$.sfnetwork_index = NA_integer_
    y[matched_feat_idxs, ]$.sfnetwork_index = matched_node_idxs
    y[new_feat_idxs, ]$.sfnetwork_index = added_node_idxs
    # Add an index column matching the orginal nodes to their new node index.
    nodes$.sfnetwork_index = seq_len(ncount)
    # Remove the geometry columns.
    # Since the full join is an attribute join.
    # We will re-add geometries later on.
    st_geometry(y) = NULL
    st_geometry(nodes) = NULL
    # Perform a full join between the attributes of the nodes and features.
    # Base the join on the created index column.
    # Remove that index column afterwards.
    new_nodes = full_join(nodes, y, by = ".sfnetwork_index")
    new_nodes = new_nodes[order(new_nodes$.sfnetwork_index), ]
    new_nodes$.sfnetwork_index = NULL
    # Add the new node geometries.
    new_node_geoms = c(N, Y[new_feat_idxs])
    new_nodes[geom_colname] = list(new_node_geoms)
    new_nodes = st_as_sf(new_nodes, sf_column_name = geom_colname)
  } else if (ncol(nodes) > 1) {
    # Scenario II: the features in y don't have attributes but the nodes do.
    # This requires:
    # --> The geometries of the new nodes binded to the original nodes.
    # --> The attribute values of these new nodes being filled with NA.
    # First, we select only those blended features that became a new node.
    y_new = st_as_sf(Y[new_feat_idxs])
    # Align the name of the geometry columns.
    names(y_new)[1] = geom_colname
    st_geometry(y_new) = geom_colname
    # Bind the new nodes with original nodes.
    # The dplyr::bind_rows function will take care of the NA filling.
    new_nodes = bind_rows(nodes, y_new)
  } else {
    # Scenario III: neither the features in y nor the nodes have attributes.
    # This requires:
    # --> The geometries of the new nodes binded to the original nodes.
    # First, we select only those blended features that became a new node.
    y_new = Y[new_feat_idxs]
    # Bind these geometries to the original node geometries.
    new_nodes = st_as_sf(c(N, y_new))
    # Set the geometry column name equal to the one in the original network.
    names(new_nodes)[1] = geom_colname
    st_geometry(new_nodes) = geom_colname
  }
  ## ============================
  # STEP IX: RECREATE THE NETWORK
  # Use the new nodes data and the new edges data to create the new network.
  ## ============================
  x_new = sfnetwork_(new_nodes, new_edges, directed = directed)
  x_new %preserve_network_attrs% x
}
