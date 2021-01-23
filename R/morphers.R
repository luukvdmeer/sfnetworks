#' Spatial morphers for sfnetworks
#'
#' Spatial morphers form spatial add-ons to the set of
#' \code{\link[tidygraph]{morphers}} provided by \code{tidygraph}. These
#' functions are not meant to be called directly. They should either be passed
#' into \code{\link[tidygraph]{morph}} to create a temporary alternative
#' representation of the input network. Such an alternative representation is a
#' list of one or more network objects. Single elements of that list can be
#' extracted directly as a new network by passing the morpher to
#' \code{\link[tidygraph]{convert}} instead, to make the changes lasting rather
#' than temporary. Alternatively, if the morphed state contains multiple
#' elements, all of them can be extracted together inside a
#' \code{\link[tibble]{tbl_df}} by passing the morpher to
#' \code{\link[tidygraph]{crystallise}}.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments to be passed on to other functions. See the description
#' of each morpher for details.
#'
#' @param store_orig_data Whenever multiple features (i.e. nodes and/or edges)
#' are merged into a single feature during morphing, should the data of the
#' original features be stored as an attribute of the new feature, in a column
#' named \code{.orig_data}. This is in line with the design principles of
#' \code{tidygraph}. Defaults to \code{FALSE}.
#'
#' @return Either a \code{morphed_sfnetwork}, which is a list of one or more
#' \code{\link{sfnetwork}} objects, or a \code{morphed_tbl_graph}, which is a
#' list of one or more \code{\link[tidygraph]{tbl_graph}} objects. See the
#' description of each morpher for details.
#'
#' @details It also possible to create your own morphers. See the documentation
#' of \code{\link[tidygraph]{morph}} for the requirements for custom morphers.
#'
#' @seealso The vignette on
#' \href{https://luukvdmeer.github.io/sfnetworks/articles/morphers.html}{spatial morphers}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel, directed = FALSE) %>%
#'   st_transform(3035)
#'
#' # Temporary changes with morph and unmorph.
#' net %>%
#'  activate("edges") %>%
#'  mutate(weight = edge_length()) %>%
#'  morph(to_spatial_shortest_paths, from = 1, to = 10) %>%
#'  mutate(in_paths = TRUE) %>%
#'  unmorph()
#'
#' # Lasting changes with convert.
#' net %>%
#'  activate("edges") %>%
#'  mutate(weight = edge_length()) %>%
#'  convert(to_spatial_shortest_paths, from = 1, to = 10)
#'
#' @name spatial_morphers
NULL

#' @describeIn spatial_morphers Make a network directed in the direction given
#' by the linestring geometries of the edges. Differs from
#' \code{\link[tidygraph]{to_directed}}, which makes a network directed based
#' on the node indices given in the \code{from} and \code{to} columns. In
#' undirected networks these indices may not correspond with the endpoints of
#' the linestring geometries. Returns a \code{morphed_sfnetwork} containing a
#' single element of class \code{\link{sfnetwork}}. This morpher requires edges
#' to be spatially explicit. If not, use \code{\link[tidygraph]{to_directed}}.
#' @importFrom igraph is_directed
#' @importFrom sf st_equals
#' @export
to_spatial_directed = function(x) {
  require_spatially_explicit_edges(x)
  if (is_directed(x)) return (x)
  # Retrieve the edges from the network, without the to and from columns.
  edges = edges_as_sf(x)
  edges[, c("from", "to")] = NULL
  # Recreate the network as a directed one.
  x_new = as_sfnetwork(edges, directed = TRUE)
  # Spatial left join between nodes of x_new and original nodes of x.
  # This is needed since node attributes got lost when constructing x_new.
  if (length(node_spatial_attribute_names(x)) > 0) {
    x_new = spatial_join_nodes(x_new, nodes_as_sf(x), join = st_equals)
  }
  # Return in a list.
  list(
    directed = x_new %preserve_graph_attrs% x
  )
}

#' @describeIn spatial_morphers Create linestring geometries between source
#' and target nodes of edges. If the edges data can be directly converted to
#' an object of class \code{\link[sf]{sf}} using \code{\link[sf]{st_as_sf}},
#' extra arguments can be provided as \code{...} and will be forwarded to
#' \code{\link[sf]{st_as_sf}} internally. Otherwise, straight lines will be
#' drawn between the source and target node of each edge. Returns a
#' \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}.
#' @importFrom igraph edge_attr<-
#' @importFrom sf st_as_sf
#' @export
to_spatial_explicit = function(x, ...) {
  # Workflow:
  # --> If ... is given, convert edges to sf by forwarding ... to st_as_sf.
  # --> If ... is not given, draw straight lines from source to target nodes.
  args = list(...)
  if (length(args) > 0) {
    edges = edges_as_table(x)
    new_edges = st_as_sf(edges, ...)
    edge_attr(x) = as.list(new_edges[, !names(new_edges) %in% c("from", "to")])
    x_new = x
  } else {
    x_new = explicitize_edges(x)
  }
  # Return in a list.
  list(
    explicit = x_new
  )
}

#' @describeIn spatial_morphers Limit a network to those nodes and edges that
#' are part of the shortest path between two nodes. \code{...} is evaluated in
#' the same manner as \code{\link{st_network_paths}} with
#' \code{type = 'shortest'}. Returns a \code{morphed_sfnetwork} that may
#' contain multiple elements of class \code{\link{sfnetwork}}, depending on
#' the number of requested paths. When unmorphing only the first instance of
#' both the node and edge data will be used, as the the same node and/or edge
#' can be present in multiple paths.
#' @importFrom igraph delete_edges delete_vertices edge_attr vertex_attr
#' @export
to_spatial_shortest_paths = function(x, ...) {
  args = list(...)
  args$x = x
  args$type = "shortest"
  # Call st_network_paths with the given arguments.
  paths = do.call("st_network_paths", args)
  # Retrieve original node and edge indices from the network.
  orig_node_idxs = vertex_attr(x, ".tidygraph_node_index")
  orig_edge_idxs = edge_attr(x, ".tidygraph_edge_index")
  # Subset the network for each computed shortest path.
  get_single_path = function(i) {
    edge_idxs = as.integer(paths$edge_paths[[i]])
    node_idxs = as.integer(paths$node_paths[[i]])
    x_new = delete_edges(x, orig_edge_idxs[-edge_idxs])
    x_new = delete_vertices(x_new, orig_node_idxs[-node_idxs])
    x_new %preserve_all_attrs% x
  }
  lapply(seq_len(nrow(paths)), get_single_path)
}

#' @describeIn spatial_morphers Remove loops and parallel edges. Returns a
#' \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}.
#'
#' @param remove_parallels Should parallel edges be removed. Defaults to
#' \code{TRUE}.
#'
#' @param remove_loops Should loops be remove. Defaults to \code{TRUE}.
#'
#' @importFrom igraph delete_edges which_loop which_multiple
#' @export
to_spatial_simple = function(x, remove_parallels = TRUE, remove_loops = TRUE) {
  x_new = x
  # Remove parallels if requested.
  if (remove_parallels) {
    x_new = delete_edges(x_new, which(which_multiple(x_new)))
  }
  # Remove loops if requested.
  if (remove_loops) {
    x_new = delete_edges(x_new, which(which_loop(x_new)))
  }
  # Return in a list.
  list(
    simple = x_new %preserve_all_attrs% x
  )
}

#' @describeIn spatial_morphers Construct a smoothed version of the network by
#' iteratively removing pseudo nodes, while preserving the connectivity of the
#' network. In the case of directed networks, pseudo nodes are those nodes that
#' have only one incoming and one outgoing edge. In undirected networks, pseudo
#' nodes are those nodes that have two incident edges. Connectivity of the
#' network is preserved by concatenating the incident edges of each removed
#' pseudo node. Returns a \code{morphed_sfnetwork} containing a single element
#' of class \code{\link{sfnetwork}}.
#' @importFrom dplyr bind_rows
#' @importFrom igraph adjacent_vertices decompose degree delete_vertices
#' edge_attr edge_attr<- get.edge.ids induced_subgraph is_directed vertex_attr
#' @importFrom sf st_as_sf st_cast st_combine st_crs st_equals st_line_merge
#' @export
to_spatial_smooth = function(x, store_orig_data = FALSE) {
  # Retrieve nodes and edges from the network.
  nodes = nodes_as_sf(x)
  edges = edges_as_table(x)
  # Check whether:
  # --> x is directed.
  # --> x has spatially explicit edges.
  directed = is_directed(x)
  spatial = is.sf(edges)
  ## ==========================
  # STEP I: DETECT PSEUDO NODES
  # The first step is to detect which nodes in x are pseudo nodes.
  # In directed networks, we define a pseudo node as follows:
  # --> A node with only one incoming and one outgoing edge.
  # In undirected networks, we define a pseudo node as follows:
  # --> A node with only two connections.
  ## ==========================
  if (directed) {
    pseudo = degree(x, mode = "in") == 1 & degree(x, mode = "out") == 1
  } else {
    pseudo = degree(x) == 2
  }
  ## ===============================
  # STEP II: FIND EDGES TO BE MERGED
  # The connectivity of the network should be preserved.
  # Therefore we need to:
  # --> Find adjacent nodes of a pseudo node.
  # --> Connect these by merging the incident edges of the pseudo node.
  # However, an adjacent node can also be another pseudo node.
  # Then, we need to look further until we find a non-pseudo (junction) node.
  # Hence, instead of processing each pseudo node on its own, we need to:
  # --> Find connected sets of pseudo nodes.
  # --> Find the adjacent junction nodes to that set.
  # --> Connect these by merging the edges in the set *and* its incident edges.
  ## ===============================
  # Subset x to only contain pseudo nodes and the edges between them.
  # Decompose this subgraph to find connected sets of pseudo nodes.
  x_pseudo = decompose(induced_subgraph(x, pseudo))
  # For each set of connected pseudo nodes:
  # --> Find the indices of the adjacent junction node(s).
  # --> Find the indices of the edges that need to be merged.
  find_edges = function(G) {
    # Retrieve the original node indices of the pseudo nodes in this set.
    # Retrieve the original edge indices of the edges that connect them.
    N = vertex_attr(G, ".tidygraph_node_index")
    E = edge_attr(G, ".tidygraph_edge_index")
    # Find all required node and edge indices.
    if (directed) {
      # Find the following:
      # --> The index of the pseudo node where an edge comes into the set.
      # --> The index of the pseudo node where an edge goes out of the set.
      n_i = N[degree(G, mode = "in") == 0]
      n_o = N[degree(G, mode = "out") == 0]
      # If these nodes do not exists:
      # --> We are dealing with a loop of connected pseudo nodes.
      # --> The loop is by definition not connected to the rest of the network.
      # --> Hence, there is no need to create a new edge.
      # --> Therefore we should not return a path.
      if (length(n_i) == 0) return (NULL)
      # Find the following:
      # --> The index of the edge that comes in to the pseudo node set.
      # --> The index of the node at the other end of that edge.
      # We'll call this the source node and source edge of the set.
      src_node = as.integer(adjacent_vertices(x, n_i, mode = "in"))
      src_edge = get.edge.ids(x, c(src_node, n_i))
      # Find the following:
      # --> The index of the edge that goes out of the pseudo node set.
      # --> The index of the node at the other end of that edge.
      # We'll call this the target node and target edge of the set.
      trg_node = as.integer(adjacent_vertices(x, n_o, mode = "out"))
      trg_edge = get.edge.ids(x, c(n_o, trg_node))
    } else {
      # In an undirected network there is no in or out. Instead, we find:
      # --> As source: The node with the lowest index connected to the set.
      # --> As target: The node with the highest index connected to the set.
      if (length(N) == 1) {
        # When we have a single pseudo node that forms a set:
        # --> It will be adjacent to the source and target.
        con_nodes = as.integer(adjacent_vertices(x, N)[[1]])
        src_node = min(con_nodes)
        src_edge = get.edge.ids(x, c(src_node, N))
        trg_node = max(con_nodes)
        trg_edge = get.edge.ids(x, c(N, trg_node))
      } else {
        # When we have a set of multiple pseudo nodes:
        # --> There are two pseudo nodes that form the boundary of the set.
        # --> These are the ones connected to only one other pseudo node.
        N_b = N[degree(G) == 1]
        # If these boundaries do not exists:
        # --> We are dealing with a loop of connected pseudo nodes.
        # --> The loop is by definition not connected to the rest of the network.
        # --> Hence, there is no need to create a new edge.
        # --> Therefore we should not return a path.
        if (length(N_b) == 0) return (NULL)
        # Find the source/target node connected to the first set boundary.
        # --> Its adjacent nodes will be one pseudo node and a source/target.
        # --> The source/target node is the one not present in the pseudo set.
        n_1 = N_b[1]
        adj_nodes = as.integer(adjacent_vertices(x, n_1)[[1]])
        con_node_1 = adj_nodes[!(adj_nodes %in% N)]
        # Find the source/target node connected to the second set boundary.
        # --> Its adjacent nodes will be a pseudo node and a source/target.
        # --> The source/target node is the one not present in the pseudo set.
        n_2 = N_b[2]
        adj_nodes = as.integer(adjacent_vertices(x, n_2)[[1]])
        con_node_2 = adj_nodes[!(adj_nodes %in% N)]
        # Define which of found nodes is the source and which the target.
        if (con_node_1 < con_node_2) {
          src_node = con_node_1
          src_edge = get.edge.ids(x, c(src_node, n_1))
          trg_node = con_node_2
          trg_edge = get.edge.ids(x, c(trg_node, n_2))
        } else {
          src_node = con_node_2
          src_edge = get.edge.ids(x, c(src_node, n_2))
          trg_node = con_node_1
          trg_edge = get.edge.ids(x, c(trg_node, n_1))
        }
      }
    }
    # List all edge indices in the path.
    edge_idxs = c(src_edge, E, trg_edge)
    # Return all retrieved information in a list.
    list(from = src_node, to = trg_node, .tidygraph_edge_index = edge_idxs)
  }
  new_edge_list = lapply(x_pseudo, find_edges)
  new_edge_list = new_edge_list[lengths(new_edge_list) != 0] # Remove NULLs.
  # Create a data frame with the merged edges.
  new_edges = data.frame(do.call("rbind", new_edge_list))
  new_edges$from = as.integer(new_edges$from)
  new_edges$to = as.integer(new_edges$to)
  ## ====================================
  # STEP III: CONCATENATE EDGE GEOMETRIES
  # If the edges to be merged have geometries:
  # --> These geometries have to be concatenated into a single new geometry.
  # --> The new geometry should go from the defined source to target node.
  ## ====================================
  if (spatial) {
    # For each new edge:
    # --> Merge all original edge geometries in the path into a single geometry.
    edge_geoms = st_geometry(edges)
    node_geoms = st_geometry(nodes)
    merge_geoms = function(E) {
      orig_edges = E$.tidygraph_edge_index
      orig_geoms = edge_geoms[orig_edges]
      new_geom = st_line_merge(st_combine(orig_geoms))
      # There is one situation where merging lines like this is problematic.
      # That is when the source and target node of the new edge are the same.
      # Hence, the original edges to be merged form a closed loop.
      # Any original edge endpoint can then be the startpoint of the new edge.
      # st_line_merge chooses the point with the lowest x coordinate.
      # This is not necessarily the source node we defined.
      # This behaviour comes from third partly libs and can not be tuned.
      # Hence, we manually need to reorder the points in the merged line.
      src = E$from
      trg = E$to
      if (src == trg & length(orig_edges) > 1) {
        pts = st_cast(new_geom, "POINT")
        src_idx = st_equals(node_geoms[src], pts)[[1]]
        if (length(src_idx) == 1) {
          n = length(pts)
          ordered_pts = c(pts[c(src_idx:n)], pts[c(2:src_idx)])
          new_geom = st_cast(st_combine(ordered_pts), "LINESTRING")
        }
      }
      new_geom
    }
    new_geoms = do.call("c", lapply(new_edge_list, merge_geoms))
    # Add the geometries to the new edges data frame.
    # Use the same geometry column name as in the original edges data frame.
    new_edges$geometry = new_geoms
    new_edges = st_as_sf(new_edges)
    geom_colname = attr(edges, "sf_column")
    if (geom_colname != "geometry") {
      names(new_edges)[4] = geom_colname
      attr(new_edges, "sf_column") = geom_colname
    }
  }
  ## ========================================
  # STEP IV: ADD MERGED EDGES TO THE NETWORK
  # The newly created edges should be added to the original network.
  # This must happen before removing the pseudo nodes.
  # Otherwise, the source and target indices do not match their nodes anymore.
  ## ========================================
  # Bind the original and new edges.
  edges$.tidygraph_edge_index = as.list(edges$.tidygraph_edge_index)
  all_edges = bind_rows(edges, new_edges)
  # Recreate an sfnetwork.
  x_new = sfnetwork_(nodes, all_edges, directed = directed)
  ## ============================================
  # STEP V: REMOVE PSEUDO NODES FROM THE NETWORK
  # Remove all the detected pseudo nodes from the original network.
  # This will automatically also remove their incident edges.
  # Remember that their replacement edges have already been added in step IV.
  ## ============================================
  x_new = delete_vertices(x_new, pseudo) %preserve_all_attrs% x
  ## =============================================
  # STEP VI: STORE ORIGINAL EDGE DATA IF REQUESTED
  # Users can request to store the data of original edges in a special column.
  # This column will - by tidygraph design - be named .orig_data.
  # The value in this column is for each edge a tibble containing:
  # --> The data of the original edges that were merged into the new edge.
  ## =============================================
  if (store_orig_data) {
    # Store the original edge data in a .orig_data column.
    orig_edge_idxs = edge_attr(x_new, ".tidygraph_edge_index")
    copy_orig_data = function(i) edges[i, , drop = FALSE]
    edge_attr(x_new, ".orig_data") = lapply(orig_edge_idxs, copy_orig_data)
    edge_agr(x_new) = valid_agr(edge_agr(x_new))
  }
  # Return in a list.
  list(
    smooth = x_new
  )
}

#' @describeIn spatial_morphers Construct a subdivision of the network by
#' subdividing edges at each interior point that is equal to any
#' other interior or boundary point in the edges table. Interior points in this
#' sense are those points that are included in their linestring geometry
#' feature but are not endpoints of it, while boundary points are the endpoints
#' of the linestrings. The network is reconstructed after subdivision such that
#' edges are connected at the points of subdivision. Returns a
#' \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}. This morpher requires edges to be spatially
#' explicit.
#' @importFrom igraph is_directed
#' @importFrom sf st_crs st_geometry
#' @importFrom sfheaders sf_to_df sfc_linestring sfc_point
#' @export
to_spatial_subdivision = function(x) {
  require_spatially_explicit_edges(x)
  raise_assume_constant("to_spatial_subdivision")
  # Retrieve nodes and edges from the network.
  nodes = nodes_as_sf(x)
  edges = edges_as_sf(x)
  ## ===========================
  # STEP I: DECOMPOSE THE EDGES
  # Decompose the edges linestring geometries into the points that shape them.
  ## ===========================
  # Extract all points from the linestring geometries of the edges.
  edge_pts = sf_to_df(edges)
  # Extract two subsets of information:
  # --> One with only the coordinates of the points
  # --> Another with indices describing to which edge a point belonged.
  edge_coords = edge_pts[names(edge_pts) %in% c("x", "y", "z", "m")]
  edge_idxs = edge_pts$linestring_id
  ## =======================================
  # STEP II: DEFINE WHERE TO SUBDIVIDE EDGES
  # Edges should be split at locations where:
  # --> An edge interior point is equal to a boundary point in another edge.
  # --> An edge interior point is equal to an interior point in another edge.
  # Hence, we need to split edges at point that:
  # --> Are interior points.
  # --> Have at least one duplicate among the other edge points.
  ## =======================================
  # Find which of the edge points is a boundary point.
  is_startpoint = !duplicated(edge_idxs)
  is_endpoint = !duplicated(edge_idxs, fromLast = TRUE)
  is_boundary = is_startpoint | is_endpoint
  # Find which of the edge points occur more than once.
  is_duplicate_desc = duplicated(edge_coords)
  is_duplicate_asc = duplicated(edge_coords, fromLast = TRUE)
  has_duplicate = is_duplicate_desc | is_duplicate_asc
  # Split points are those edge points satisfying both of the following rules:
  # --> 1) They have at least one duplicate among the other edge points.
  # --> 2) They are not edge boundary points themselves.
  is_split = has_duplicate & !is_boundary
  ## ================================
  # STEP III: DUPLICATE SPLIT POINTS
  # The split points are currently a single interior point in an edge.
  # They will become the endpoint of one edge *and* the startpoint of another.
  # Hence, each split point needs to be duplicated.
  ## ================================
  # Create the repetition vector:
  # --> This defines for each edge point if it should be duplicated.
  # --> A value of '1' means 'store once', i.e. don't duplicate.
  # --> A value of '2' means 'store twice', i.e. duplicate.
  # --> Split points will be part of two new edges and should be duplicated.
  reps = rep(1L, nrow(edge_coords))
  reps[is_split] = 2L
  # Create the new coordinate data frame by duplicating split points.
  new_edge_coords = data.frame(lapply(edge_coords, function(i) rep(i, reps)))
  ## ==========================================
  # STEP IV: CONSTRUCT THE NEW EDGES GEOMETRIES
  # With the new coords of the edge points we need to recreate linestrings.
  # First we need to know which edge points belong to which *new* edge.
  # Then we need to build a linestring geometry for each new edge.
  ## ==========================================
  # First assign each new edge point coordinate its *original* edge index.
  # --> Then increment those accordingly at each split point.
  orig_edge_idxs = rep(edge_idxs, reps)
  # Original edges are subdivided at each split point.
  # Therefore, a new edge originates from each split point.
  # Hence, to get the new edge indices:
  # --> Increment each original edge index by 1 at each split point.
  incs = integer(nrow(new_edge_coords)) # By default don't increment.
  incs[which(is_split) + 1:sum(is_split)] = 1L # Add 1 after each split.
  new_edge_idxs = orig_edge_idxs + cumsum(incs)
  new_edge_coords$edge_id = new_edge_idxs
  # Build the new edge geometries.
  new_edge_geoms = sfc_linestring(new_edge_coords, linestring_id = "edge_id")
  st_crs(new_edge_geoms) = st_crs(edges)
  new_edge_coords$edge_id = NULL
  ## ===================================
  # STEP V: CONSTRUCT THE NEW EDGE DATA
  # We now have the geometries of the new edges.
  # However, the original edge attributes got lost.
  # We will restore them by:
  # --> Adding back the attributes to edges that were not split.
  # --> Duplicating original attributes within splitted edges.
  # Beware that from and to columns will remain unchanged at this stage.
  # We will update them later.
  ## ===================================
  # Find which *original* edge belongs to which *new* edge:
  # --> Use the list of new edge points constructed before.
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
  ## ==========================================
  # STEP VI: CONSTRUCT THE NEW NODE GEOMETRIES
  # All split points are now boundary points of new edges.
  # All edge boundaries become nodes in the network.
  ## ==========================================
  is_new_boundary = rep(is_split | is_boundary, reps)
  new_node_geoms = sfc_point(new_edge_coords[is_new_boundary, ])
  st_crs(new_node_geoms) = st_crs(nodes)
  ## =====================================
  # STEP VII: CONSTRUCT THE NEW NODE DATA
  # We now have the geometries of the new nodes.
  # However, the original node attributes got lost.
  # We will restore them by:
  # --> Adding back the attributes to nodes that were already a node before.
  # --> Filling attribute values of newly added nodes with NA.
  # Beware at this stage the nodes are recreated from scratch.
  # That means each boundary point of the new edges is stored as separate node.
  # Boundaries with equal geometries will be merged into a single node later.
  ## =====================================
  # Find which of the *original* edge points equaled which *original* node.
  # If an edge point did not equal a node, store NA instead.
  node_idxs = rep(NA, nrow(edge_pts))
  node_idxs[is_boundary] = edge_boundary_node_indices(x)
  # Find which of the *original* nodes belong to which *new* edge boundary.
  # If a new edge boundary does not equal an original node, store NA instead.
  orig_node_idxs = rep(node_idxs, reps)[is_new_boundary]
  # Retrieve original node data for each new edge boundary.
  # Rows of newly added nodes will be NA.
  new_nodes = nodes[orig_node_idxs, ]
  # Set the new node geometries as geometries of these new nodes.
  st_geometry(new_nodes) = new_node_geoms
  ## ==================================================
  # STEP VIII: UPDATE FROM AND TO INDICES OF NEW EDGES
  # Now we updated the node data, the node indices changes.
  # Therefore we need to update the from and to columns of the edges as well.
  ## ==================================================
  # Define the indices of the new nodes.
  # Equal geometries should get the same index.
  new_node_idxs = match(new_node_geoms, unique(new_node_geoms))
  # Map node indices to edges.
  is_source = rep(c(TRUE, FALSE), length(new_node_geoms) / 2)
  new_edges$from = new_node_idxs[is_source]
  new_edges$to = new_node_idxs[!is_source]
  ## =============================
  # STEP IX: UPDATE THE NEW NODES
  # We can now remove the duplicated node geometries from the new nodes data.
  # Then, each location is represented by a single node.
  ## =============================
  new_nodes = new_nodes[!duplicated(new_node_idxs), ]
  ## ============================
  # STEP X: RECREATE THE NETWORK
  # Use the new nodes data and the new edges data to create the new network.
  ## ============================
  # Create new network.
  x_new = sfnetwork_(new_nodes, new_edges, directed = is_directed(x))
  # Return in a list.
  list(
    subdivision = x_new %preserve_all_attrs% x
  )
}

#' @describeIn spatial_morphers Subset the network by applying a spatial
#' filter, i.e. a filter on the geometry column based on a spatial predicate.
#' \code{...} is evaluated in the same manner as \code{\link[sf]{st_filter}}.
#' Returns a \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}. For filters on an attribute column, use
#' \code{\link[tidygraph]{to_subgraph}}.
#'
#' @param subset_by Whether to create subgraphs based on nodes or edges.
#'
#' @export
to_spatial_subset = function(x, ..., subset_by = NULL) {
  if (is.null(subset_by)) {
    subset_by = attr(x, "active")
    message("Subsetting by ", subset_by)
  }
  x_new = switch(
    subset_by,
    nodes = spatial_filter_nodes(x, ...),
    edges = spatial_filter_edges(x, ...),
    raise_unknown_input(subset_by)
  )
  list(
    subset = x_new
  )
}

#' @describeIn spatial_morphers Transform the geospatial coordinates of the
#' network into a different coordinate reference system. \code{...} is
#' evaluated in the same manner as \code{\link[sf]{st_transform}}.
#' Returns a \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}.
#' @importFrom sf st_transform
#' @export
to_spatial_transformed = function(x, ...) {
  list(
    transformed = st_transform(x, ...)
  )
}

#' @describeIn spatial_morphers For a given \code{node}, it subsets only the
#'   vertices which are closer than a given \code{distance}. The edges' lengths
#'   are determined using the \code{weight} attribute from edges' table. If no
#'   \code{weight} attribute is present, then the geographic edge lengths will
#'   be calculated internally and used as weights. This morpher can be used to
#'   estimate isochrones and isodistances.
#' @param node The starting node that must be used for estimating the spatial local
#'   neighborhood
#' @param distance The threshold spatial distance
#' @importFrom tidygraph activate with_graph filter node_distance_from
#' @importFrom igraph edge_attr
#' @export
to_spatial_local_neighborhood = function(x, node, distance) {
  if (is.null(edge_attr(x, "weight"))) {
    weights = with_graph(activate(x, "edges"), edge_length())
  }

  filter(
    x,
    node_distance_from(node, weights = weights) <= distance
  )
}
