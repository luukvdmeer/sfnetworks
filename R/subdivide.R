#' @importFrom igraph is_directed
#' @importFrom sf st_crs st_geometry
#' @importFrom sfheaders sf_to_df sfc_linestring sfc_point
#' @export
st_network_subdivide = function(x) {
  require_spatially_explicit_edges(x)
  raise_assume_constant("st_network_subdivide")
  # Retrieve nodes and edges from the network.
  nodes = nodes_as_sf(x)
  edges = edges_as_sf(x)
  # Extract all points from the linestring geometries of the edges.
  edge_pts = sf_to_df(edges)
  edge_coords = edge_pts[names(edge_pts) %in% c("x", "y", "z", "m")]
  # Create an index set describing:
  # --> Which edge point belongs to which edge.
  edge_idxs = edge_pts$linestring_id
  # Find which of the edge points are edge boundaries.
  is_startpoint = !duplicated(edge_idxs)
  is_endpoint = !duplicated(edge_idxs, fromLast = TRUE)
  is_boundary = is_startpoint | is_endpoint
  # Create an index set describing:
  # --> Which edge point equals which node.
  node_idxs = rep(NA, nrow(points))
  node_idxs[is_boundary] = edge_boundary_node_indices(x)
  # Find which of the edge points occur more than once.
  is_duplicate_desc = duplicated(edge_coords)
  is_duplicate_asc = duplicated(edge_coords, fromLast = TRUE)
  has_duplicate = is_duplicate_desc | is_duplicate_asc
  # Split points are those edge points satisfying both of the following rules:
  # --> 1) They have at least one duplicate among the other edge points.
  # --> 2) They are not edge boundary points themselves.
  is_split = has_duplicate & !is_boundary
  # Create the repetition vector:
  # --> This defines for each edge point if it should be duplicated.
  # --> A value of '1' means 'store once', i.e. don't duplicate.
  # --> A value of '2' means 'store twice', i.e. duplicate.
  # --> Split points will be part of two new edges and should be duplicated.
  reps = rep(1L, nrow(edge_coords))
  reps[is_split] = 2L
  # Create the new coordinate data frame by duplicating split points.
  new_edge_coords = data.frame(lapply(edge_coords, function(i) rep(i, reps)))
  # Update edge indices:
  # --> First duplicate original indices at each split point.
  # --> Then increment those accordingly at each split point.
  dup_edge_idxs = rep(edge_idxs, reps)
  incs = integer(nrow(new_edge_coords)) # By default don't increment.
  incs[which(is_split) + 1:sum(is_split)] = 1L # Add 1 after each split.
  new_edge_idxs = dup_edge_idxs + cumsum(incs)
  # Build new edge geometries.
  new_edge_coords$edge_id = new_edge_idxs
  new_edge_geoms = sfc_linestring(new_edge_coords, linestring_id = "edge_id")
  st_crs(new_edge_geoms) = st_crs(edges)
  new_edge_coords$edge_id = NULL
  # Restore orignal edge attributes.
  # Duplicate attributes within splitted edges.
  orig_edge_idxs = dup_edge_idxs[!duplicated(new_edge_idxs)]
  new_edges = edges[orig_edge_idxs, ]
  st_geometry(new_edges) = new_edge_geoms
  # Build new node geometries.
  is_new_boundary = rep(is_split | is_boundary, reps)
  new_node_geoms = sfc_point(new_edge_coords[is_new_boundary, ])
  st_crs(new_node_geoms) = st_crs(nodes)
  # Set from and to columns of new edges.
  new_node_idxs = match(new_node_geoms, unique(new_node_geoms))
  is_source = rep(c(TRUE, FALSE), length(new_node_geoms) / 2)
  new_edges$from = new_node_idxs[is_source]
  new_edges$to = new_node_idxs[!is_source]
  # Restore original node attributes.
  # Fill attributes of newly created nodes with NA.
  orig_node_idxs = rep(node_idxs, reps)[is_new_boundary]
  #orig_node_idxs = c(1, NA, NA, 2, 3, NA, NA, 4, 5, 2, 2, 6)
  new_nodes = nodes[orig_node_idxs, ]
  st_geometry(new_nodes) = new_node_geoms
  # Remove duplicated nodes from the new nodes table.
  new_nodes = new_nodes[!duplicated(new_node_idxs), ]
  # Create new network.
  x_new = sfnetwork_(new_nodes, new_edges, directed = is_directed(x))
  x_new %preserve_active% x
}