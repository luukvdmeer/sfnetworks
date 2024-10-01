#' Subdivide edges at interior points
#'
#' Construct a subdivision of the network by subdividing edges at interior
#' points. Subdividing means that a new node is added on an edge, and the edge
#' is split in two at that location. Interior points are those points that
#' shape a linestring geometry feature but are not endpoints of it.
#'
#' @param x An object of class \code{\link{sfnetwork}} with spatially explicit
#' edges.
#'
#' @param protect An integer vector of edge indices specifying which edges
#' should be protected from being subdivided. Defaults to \code{NULL}, meaning
#' that none of the edges is protected.
#'
#' @param all Should edges be subdivided at all their interior points? If set
#' to \code{FALSE}, edges are only subdivided at those interior points that
#' share their location with any other interior or boundary point (a node) in
#' the edges table. Defaults to \code{FALSE}.
#'
#' @param merge Should multiple subdivision points at the same location be
#' merged into a single node, and should subdivision points at the same
#' location as an existing node be merged into that node? Defaults to
#' \code{TRUE}. If set to \code{FALSE}, each subdivision point is added
#' separately as a new node to the network.
#'
#' @note By default sfnetworks rounds coordinates to 12 decimal places to
#' determine spatial equality. You can influence this behavior by explicitly
#' setting the precision of the network using
#' \code{\link[sf]{st_set_precision}}.
#'
#' @returns The subdivision of x as object of class \code{\link{sfnetwork}}.
#'
#' @importFrom dplyr arrange bind_rows
#' @importFrom igraph is_directed
#' @importFrom sf st_geometry<-
#' @importFrom sfheaders sf_to_df
#' @export
subdivide_edges = function(x, protect = NULL, all = FALSE, merge = TRUE) {
  if (will_assume_constant(x)) raise_assume_constant("subdivide_edges")
  nodes = nodes_as_sf(x)
  edges = edges_as_sf(x)
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
  # Clean up.
  edge_pts$sfg_id = NULL
  edge_pts$linestring_id = NULL
  ## =======================================
  # STEP II: DEFINE WHERE TO SUBDIVIDE EDGES
  # If all = TRUE, edges should be split at all interior points.
  # Otherwise, edges should be split only at those interior points that:
  # --> Are equal to a boundary point in another edge.
  # --> Are equal to an interior point in another edge.
  ## =======================================
  # Define which edge points are boundaries.
  is_startpoint = !duplicated(edge_pts$eid)
  is_endpoint = !duplicated(edge_pts$eid, fromLast = TRUE)
  is_boundary = is_startpoint | is_endpoint
  # Store for each edge point the node index, if it is a boundary.
  edge_nids = rep(NA, n)
  edge_nids[is_boundary] = edge_incident_ids(x)
  edge_pts$nid = edge_nids
  # Compute for each edge point a unique location index.
  # Edge points that are spatially equal get the same location index.
  # Note that this is only needed if:
  # --> Only shared interior points should be split.
  # --> Shared interior points should be merged into a single node afterwards.
  edge_coords = edge_pts[names(edge_pts) %in% c("x", "y", "z", "m")]
  if (merge | !all) {
    edge_lids = st_match_points_df(edge_coords, st_precision(edges))
    edge_pts$lid = edge_lids
  }
  # Define which edges to protect from being subdivided.
  is_protected = rep(FALSE, nrow(edge_pts))
  if (! is.null(protect)) {
    is_protected[edge_pts$eid %in% protect] = TRUE
  }
  # Define the subdivision points.
  if (all) {
    is_split = !is_boundary & !is_protected
  } else {
    has_duplicate_desc = duplicated(edge_lids)
    has_duplicate_asc = duplicated(edge_lids, fromLast = TRUE)
    has_duplicate = has_duplicate_desc | has_duplicate_asc
    is_split = has_duplicate & !is_boundary & !is_protected
  }
  ## ==========================================
  # STEP III: CONSTRUCT THE NEW EDGE GEOMETRIES
  # First we duplicate each split point.
  # They become the endpoint of one edge *and* the startpoint of another.
  # With those extended edge points we need to recreate linestrings.
  # First we define for each edge point the new edge index.
  # Then we need to build a linestring geometry for each new edge.
  ## ==========================================
  # Create the new set of edge points by duplicating split points.
  new_edge_pts = create_new_edge_df(edge_pts, is_split)
  # Define the new edge index of each new edge point.
  new_edge_ids = create_new_edge_ids(new_edge_pts, is_split)
  # Construct the new edge linestring geometries.
  new_edge_geoms = create_new_edge_geoms(new_edge_pts, new_edge_ids, edges)
  ## ===================================
  # STEP IV: CONSTRUCT THE NEW EDGE DATA
  # We now have the geometries of the new edges.
  # However, the original edge attributes got lost.
  # We will restore them by:
  # --> Adding back the attributes to edges that were not split.
  # --> Duplicating original attributes within splitted edges.
  # Beware that from and to columns will remain unchanged at this stage.
  # We will update them later.
  ## ===================================
  # Define at which new edge points a new edge starts and ends.
  is_new_startpoint = !duplicated(new_edge_ids)
  is_new_endpoint = !duplicated(new_edge_ids, fromLast = TRUE)
  # Use the original edge ids of the startpoints to copy original attributes.
  new_edges = edges[new_edge_pts$eid[is_new_startpoint], ]
  # Insert the newly constructed edge geometries.
  st_geometry(new_edges) = new_edge_geoms
  ## ===================================
  # STEP V: CONSTRUCT THE NEW NODE DATA
  # New nodes are added at the split points.
  # Depending on settings these may be merged with nodes at the same location.
  # The nodes that are added should get a valid node index.
  ## ===================================
  # Identify and select the edge points that become a node in the new network.
  is_new_node = is_new_startpoint | is_new_endpoint
  new_node_pts = new_edge_pts[is_new_node, ]
  # Define the new node indices of those points.
  # If merge is set to TRUE:
  # --> Equal split points should be added as the same node.
  # --> Split points equal to an existing node should get that existing index.
  # If merge equal is set to FALSE:
  # --> Each split point is added as a separate node.
  if (merge) {
    # Arrange the new nodes table such that:
    # --> Existing nodes come before split points.
    new_node_pts = arrange(new_node_pts, nid)
    # Update the unique location indices.
    # Such that they match the length and order of the arranged node table.
    new_node_lids = match(new_node_pts$lid, unique(new_node_pts$lid))
    # If all existing nodes are at unique locations:
    # --> The location indices become the new node indices.
    # If there are multiple existing nodes at the same location:
    # --> We do not want those nodes to be merged.
    # --> Some more work is needed to define the new node indices.
    if (any(duplicated(edge_lids[is_boundary]))) {
      # First extract the current node indices.
      new_node_ids = new_node_pts$nid
      # Define which of the new nodes do not have an index yet.
      # These are those nodes that were not existing before.
      is_na = is.na(new_node_ids)
      # If such a node is equal to an existing node
      # --> Use the index of the existing node.
      # Otherwise:
      # --> Give it a new index continuing the current sequence of indices.
      k = max(new_node_lids[!is_na])
      give_index = function(i) ifelse(i > k, i - k + nrow(nodes), i)
      na_lids = new_node_lids[is_na]
      new_node_ids[is_na] = vapply(na_lids, give_index, FUN.VALUE = integer(1))
      new_node_pts$nid = new_node_ids
    } else {
      new_node_pts$nid = new_node_lids
    }
    # Arrange the new nodes table back into the original order.
    new_node_pts = arrange(new_node_pts, eid, pid)
    # Define for each of the new nodes:
    # --> Which of them did not exist yet in the original network.
    is_add = new_node_pts$nid > nrow(nodes)
    add_node_ids = new_node_pts$nid[is_add]
  } else {
    # If equal locations should not become the same node:
    # --> All split points did not exist yet as a node.
    is_add = is.na(new_node_pts$nid)
    add_node_pids = new_node_pts$pid[is_add]
    add_node_ids = match(add_node_pids, unique(add_node_pids)) + nrow(nodes)
    new_node_pts[is_add, ]$nid = add_node_ids
  }
  # Construct the geometries of the nodes that need to be added to the network.
  add_node_pts = new_node_pts[is_add, ][!duplicated(add_node_ids), ]
  add_node_geoms = df_to_points(add_node_pts, nodes)
  # Construct the new node data.
  # This is done by simply binding original node data with added geometries.
  add_nodes = sfc_to_sf(add_node_geoms, colname = attr(nodes, "sf_column"))
  new_nodes = bind_rows(nodes, add_nodes)
  ## ==================================================
  # STEP VI: UPDATE FROM AND TO INDICES OF NEW EDGES
  # Now we constructed the new node data with updated node indices.
  # Therefore we need to update the from and to columns of the edges as well.
  ## ==================================================
  new_edges$from = new_node_pts$nid[is_new_startpoint[is_new_node]]
  new_edges$to = new_node_pts$nid[is_new_endpoint[is_new_node]]
  ## ============================
  # STEP VII: RECREATE THE NETWORK
  # Use the new nodes data and the new edges data to create the new network.
  ## ============================
  x_new = sfnetwork_(new_nodes, new_edges, directed = is_directed(x))
  x_new %preserve_network_attrs% x
}

create_new_edge_df = function(df, splits) {
  # Determine the number of edge points.
  n = nrow(df)
  # Create the repetition vector:
  # --> This defines for each edge point if it should be duplicated.
  # --> A value of '1' means 'store once', i.e. don't duplicate.
  # --> A value of '2' means 'store twice', i.e. duplicate.
  # --> Split points will be part of two new edges and should be duplicated.
  reps = rep(1L, n)
  reps[splits] = 2L
  # Create the new set of edge points by duplicating split points.
  df[rep(seq_len(n), reps), ]
}

create_new_edge_ids = function(df, splits, id_col = "eid") {
  # Define the new edge index of each new edge point.
  # We do so by incrementing each original edge index by 1 at each split point.
  incs = rep(0L, nrow(df))
  incs[which(splits) + 1:sum(splits)] = 1L
  df[[id_col]] + cumsum(incs)
}

create_new_edge_geoms = function(df, ids, sf_obj) {
  coords = df[names(df) %in% c("x", "y", "z", "m")]
  coords$eid = ids
  new_edge_geoms = df_to_lines(coords, sf_obj, "eid", select = FALSE)
}