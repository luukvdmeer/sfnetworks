#' Smooth pseudo nodes
#'
#' Construct a smoothed version of the network by iteratively removing pseudo
#' nodes, while preserving the connectivity of the network. In the case of
#' directed networks, pseudo nodes are those nodes that have only one incoming
#' and one outgoing edge. In undirected networks, pseudo nodes are those nodes
#' that have two incident edges. Equality of attribute values among the two
#' edges can be defined as an additional requirement by setting the
#' \code{require_equal} parameter. Connectivity of the network is preserved by
#' concatenating the incident edges of each removed pseudo node.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param protect An integer vector of edge indices specifying which nodes
#' should be protected from being removed. Defaults to \code{NULL}, meaning
#' that none of the nodes is protected.
#'
#' @param require_equal A character vector of edge column names specifying
#' which attributes of the incident edges of a pseudo node should be equal in
#' order for the pseudo node to be removed? Defaults to \code{NULL}, meaning
#' that attribute equality is not considered for pseudo node removal.
#'
#' @param attribute_summary How should the attributes of concatenated edges
#' be summarized? There are several options, see
#' \code{\link[igraph]{igraph-attribute-combination}} for details.
#'
#' @param store_original_ids For each concatenated edge, should the indices of
#' the original edges be stored as an attribute of the new edge, in a column
#' named \code{.tidygraph_edge_index}? This is in line with the design
#' principles of \code{tidygraph}. Defaults to \code{FALSE}.
#'
#' @param store_original_data For each concatenated edge, should the data of
#' the original edges be stored as an attribute of the new edge, in a column
#' named \code{.orig_data}? This is in line with the design principles of
#' \code{tidygraph}. Defaults to \code{FALSE}.
#'
#' @returns The smoothed network as object of class \code{\link{sfnetwork}}.
#'
#' @importFrom cli cli_abort
#' @importFrom dplyr distinct slice
#' @importFrom igraph adjacent_vertices decompose degree delete_vertices
#' edge_attr get_edge_ids igraph_opt igraph_options incident_edges
#' induced_subgraph is_directed vertex_attr
#' @importFrom sf st_as_sf st_cast st_combine st_crs st_drop_geometry
#' st_equals st_is st_line_merge
#' @export
smooth_pseudo_nodes = function(x, protect = NULL,
                               require_equal = NULL,
                               attribute_summary = "ignore",
                               store_original_ids = FALSE,
                               store_original_data = FALSE) {
  # Change default igraph options.
  # This prevents igraph returns node or edge indices as formatted sequences.
  # We only need the "raw" integer indices.
  # Changing this option improves performance especially on large networks.
  default_igraph_opt = igraph_opt("return.vs.es")
  igraph_options(return.vs.es = FALSE)
  on.exit(igraph_options(return.vs.es = default_igraph_opt))
  # Add index columns if not present.
  # These keep track of original node and edge indices.
  x = add_original_ids(x)
  # Retrieve nodes and edges from the network.
  nodes = nodes_as_sf(x)
  edges = edge_data(x, focused = FALSE)
  # For later use:
  # --> Check if x is directed.
  # --> Check if x has spatially explicit edges.
  # --> Retrieve the name of the geometry column of the edges in x.
  directed = is_directed(x)
  explicit_edges = is_sf(edges)
  edge_geomcol = attr(edges, "sf_column")
  ## ==========================
  # STEP I: DETECT PSEUDO NODES
  # The first step is to detect which nodes in x are pseudo nodes.
  # In directed networks, we define a pseudo node as follows:
  # --> A node with only one incoming and one outgoing edge.
  # In undirected networks, we define a pseudo node as follows:
  # --> A node with only two connections.
  ## ==========================
  pseudo = is_pseudo_node(x)
  # Detected pseudo nodes that are protected should be filtered out.
  if (! is.null(protect)) {
    pseudo[protect] = FALSE
  }
  # Check for equality of certain attributes between incident edges.
  # Detected pseudo nodes that fail this check should be filtered out.
  if (! is.null(require_equal)) {
    pseudo_ids = which(pseudo)
    edge_attrs = st_drop_geometry(edges)
    edge_attrs = edge_attrs[, names(edge_attrs) %in% require_equal]
    incident_ids = incident_edges(x, pseudo_ids, mode = "all")
    check_equality = function(i) nrow(distinct(slice(edge_attrs, i + 1))) < 2
    pass = do.call("c", lapply(incident_ids, check_equality))
    pseudo[pseudo_ids[!pass]] = FALSE
  }
  # If there are no pseudo nodes left:
  # --> We do not have to smooth anything.
  if (! any(pseudo)) {
    # Store original edge data in a .orig_data column if requested.
    if (store_original_data) {
      x = add_original_edge_data(x, edges)
    }
    # Remove original indices if requested.
    if (! store_original_ids) {
      x = drop_original_ids(x)
    }
    # Return x without smoothing.
    return(x)
  }
  ## ====================================
  # STEP II: INITIALIZE REPLACEMENT EDGES
  # When removing pseudo nodes their incident edges get removed to.
  # To preserve the network connectivity we need to:
  # --> Find the two adjacent nodes of a pseudo node.
  # --> Connect these by merging the incident edges of the pseudo node.
  # An adjacent node of a pseudo node can also be another pseudo node.
  # Instead of processing each pseudo node on its own, we will:
  # --> Find connected sets of pseudo nodes.
  # --> Find the adjacent non-pseudo nodes (junction or pendant) to that set.
  # --> Connect them by merging the edges in the set plus its incident edges.
  ## ====================================
  # Subset x to only contain pseudo nodes and the edges between them.
  # Decompose this subgraph to find connected sets of pseudo nodes.
  pseudo_sets = decompose(induced_subgraph(x, pseudo))
  # For each set of connected pseudo nodes:
  # --> Find the indices of the adjacent nodes.
  # --> Find the indices of the edges that need to be merged.
  # The workflow for this is different for directed and undirected networks.
  if (directed) {
    initialize_replacement_edge = function(S) {
      # Retrieve the original node indices of the pseudo nodes in this set.
      # Retrieve the original edge indices of the edges that connect them.
      N = vertex_attr(S, ".tidygraph_node_index")
      E = edge_attr(S, ".tidygraph_edge_index")
      # Find the following:
      # --> The index of the pseudo node where an edge comes into the set.
      # --> The index of the pseudo node where an edge goes out of the set.
      n_i = N[degree(S, mode = "in") == 0]
      n_o = N[degree(S, mode = "out") == 0]
      # If these nodes do not exists:
      # --> We are dealing with a loop of connected pseudo nodes.
      # --> The loop is by definition not connected to the rest of the network.
      # --> Hence, there is no need to create a new edge.
      # --> Therefore we should not return a path.
      if (length(n_i) == 0) return (NULL)
      # Find the following:
      # --> The index of the edge that comes in to the pseudo node set.
      # --> The index of the non-pseudo node at the other end of that edge.
      # We'll call this the source node and source edge of the set.
      # Note the + 1 since adjacent_vertices returns indices starting from 0.
      source_node = adjacent_vertices(x, n_i, mode = "in")[[1]] + 1
      source_edge = get_edge_ids(x, c(source_node, n_i))
      # Find the following:
      # --> The index of the edge that goes out of the pseudo node set.
      # --> The index of the non-pseudo node at the other end of that edge.
      # We'll call this the sink node and sink edge of the set.
      # Note the + 1 since adjacent_vertices returns indices starting from 0.
      sink_node = adjacent_vertices(x, n_o, mode = "out")[[1]] + 1
      sink_edge = get_edge_ids(x, c(n_o, sink_node))
      # List indices of all edges that will be merged into the replacement edge.
      edge_idxs = c(source_edge, E, sink_edge)
      # Return all retrieved information in a list.
      list(
        from = as.integer(source_node),
        to = as.integer(sink_node),
        .tidygraph_edge_index = as.integer(edge_idxs)
      )
    }
  } else {
    initialize_replacement_edge = function(S) {
      # Retrieve the original node indices of the pseudo nodes in this set.
      # Retrieve the original edge indices of the edges that connect them.
      N = vertex_attr(S, ".tidygraph_node_index")
      E = edge_attr(S, ".tidygraph_edge_index")
      # Find the following:
      # --> The two adjacent non-pseudo nodes to the set.
      # --> The edges that connect these nodes to the set.
      # We'll call these the adjacent nodes and incident edges of the set.
      # --> The adjacent node with the lowest index will be the source node.
      # --> The adjacent node with the higest index will be the sink node.
      if (length(N) == 1) {
        # When we have a single pseudo node that forms a set:
        # --> It will be adjacent to both adjacent nodes of the set.
        # Note the + 1 since adjacent_vertices returns indices starting from 0.
        adjacent = adjacent_vertices(x, N)[[1]] + 1
        if (length(adjacent) == 1) {
          # If there is only one adjacent node to the pseudo node:
          # --> The two adjacent nodes of the set are the same node.
          # --> We only have to query for incident edges of the set once.
          incident = get_edge_ids(x, c(adjacent, N))
          source_node = adjacent
          source_edge = incident[1]
          sink_node = adjacent
          sink_edge = incident[2]
        } else {
          # If there are two adjacent nodes to the pseudo node:
          # --> The one with the lowest index will be source node.
          # --> The one with the highest index will be sink node.
          source_node = min(adjacent)
          source_edge = get_edge_ids(x, c(source_node, N))
          sink_node = max(adjacent)
          sink_edge = get_edge_ids(x, c(N, sink_node))
        }
      } else {
        # When we have a set of multiple pseudo nodes:
        # --> There are two pseudo nodes that form the boundary of the set.
        # --> These are the ones connected to only one other pseudo node.
        N_b = N[degree(S) == 1]
        # If these boundaries do not exist:
        # --> We are dealing with a loop of connected pseudo nodes.
        # --> The loop is by definition not connected to the rest of the network.
        # --> Hence, there is no need to create a new edge.
        # --> Therefore we should not return a path.
        if (length(N_b) == 0) return (NULL)
        # Find the adjacent nodes of the set.
        # These are the adjacent non-pseudo nodes to the boundaries of the set.
        # We find them iteratively for the two boundary nodes of the set:
        # --> A boundary connects to one pseudo node and one non-pseudo node.
        # --> The non-pseudo node is the one not present in the pseudo set.
        # Note the + 1 since adjacent_vertices returns indices starting from 0.
        get_set_neighbour = function(n) {
          all = adjacent_vertices(x, n)[[1]] + 1
          all[!(all %in% N)]
        }
        adjacent = do.call("c", lapply(N_b, get_set_neighbour))
        # The adjacent node with the lowest index will be source node.
        # The adjacent node with the highest index will be sink node.
        N_b = N_b[order(adjacent)]
        source_node = min(adjacent)
        source_edge = get_edge_ids(x, c(source_node, N_b[1]))
        sink_node = max(adjacent)
        sink_edge = get_edge_ids(x, c(N_b[2], sink_node))
      }
      # List indices of all edges that will be merged into the replacement edge.
      edge_idxs = c(source_edge, E, sink_edge)
      # Return all retrieved information in a list.
      list(
        from = as.integer(source_node),
        to = as.integer(sink_node),
        .tidygraph_edge_index = as.integer(edge_idxs)
      )
    }
  }
  new_idxs = lapply(pseudo_sets, initialize_replacement_edge)
  new_idxs = new_idxs[lengths(new_idxs) != 0] # Remove NULLs.
  ## ===================================
  # STEP III: SUMMARISE EDGE ATTRIBUTES
  # Each replacement edge replaces multiple original edges.
  # Their attributes should all be summarised in a single value.
  # The summary techniques to be used are given as attribute_summary.
  ## ===================================
  # Obtain the attribute values of all original edges in the network.
  # These should not include the geometries and original edge indices.
  exclude = c(".tidygraph_edge_index", edge_geomcol)
  edge_attrs = edge_attr(x)
  edge_attrs = edge_attrs[!(names(edge_attrs) %in% exclude)]
  # For each replacement edge:
  # --> Summarise the attributes of the edges it replaces into single values.
  merge_attrs = function(E) {
    ids = E$.tidygraph_edge_index
    summarize_attributes(edge_attrs, attribute_summary, subset = ids)
  }
  new_attrs = lapply(new_idxs, merge_attrs)
  ## ===================================
  # STEP VI: CONCATENATE EDGE GEOMETRIES
  # If the edges to be replaced have geometries:
  # --> These geometries have to be concatenated into a single new geometry.
  # --> The new geometry should go from the defined source to sink node.
  ## ===================================
  if (explicit_edges) {
    # Obtain geometries of all original edges and nodes in the network.
    edge_geoms = st_geometry(edges)
    node_geoms = st_geometry(nodes)
    # For each replacement edge:
    # --> Merge geometries of the edges it replaces into a single geometry.
    merge_geoms = function(E) {
      orig_edges = E$.tidygraph_edge_index
      orig_geoms = edge_geoms[orig_edges]
      new_geom = st_line_merge(st_combine(orig_geoms))
      # There are two situations where merging lines like this is problematic.
      # 1. When the source and sink node of the new edge are the same.
      # --> In this case the original edges to be replaced form a closed loop.
      # --> Any original endpoint can then be the startpoint of the new edge.
      # --> st_line_merge chooses the point with the lowest x coordinate.
      # --> This is not necessarily the source node we defined.
      # --> This behaviour comes from third partly libs and can not be tuned.
      # --> Hence, we manually need to reorder the points in the merged line.
      if (E$from == E$to && length(orig_edges) > 1) {
        pts = st_cast(new_geom, "POINT")
        from_idx = st_equals(node_geoms[E$from], pts)[[1]]
        if (length(from_idx) == 1) {
          n = length(pts)
          ordered_pts = c(pts[c(from_idx:n)], pts[c(2:from_idx)])
          new_geom = st_cast(st_combine(ordered_pts), "LINESTRING")
        }
      }
      # 2. When the new edge crosses itself.
      # --> In this case st_line_merge creates a multilinestring geometry.
      # --> We just want a regular linestring (even if this is invalid).
      if (any(st_is(new_geom, "MULTILINESTRING"))) {
        new_geom = force_multilinestrings_to_linestrings(new_geom)
      }
      new_geom
    }
    new_geoms = do.call("c", lapply(new_idxs, merge_geoms))
  }
  ## ============================================
  # STEP V: ADD REPLACEMENT EDGES TO THE NETWORK
  # The newly created edges should be added to the original network.
  # This must happen before removing the pseudo nodes.
  # Otherwise their from and to values do not match the correct node indices.
  ## ============================================
  # Create the data frame for the new edges.
  new_edges = cbind(
    data.frame(do.call("rbind", new_idxs)),
    data.frame(do.call("rbind", new_attrs))
  )
  # Bind together with the original edges.
  # Merged edges may have list-columns for some attributes.
  # This requires a bit more complicated rowbinding.
  if (explicit_edges) {
    new_edges[edge_geomcol] = list(new_geoms)
    all_edges = bind_rows_list(edges, new_edges)
    all_edges = st_as_sf(all_edges, sf_column_name = edge_geomcol)
  } else {
    all_edges = bind_rows_list(edges, new_edges)
  }
  # Recreate an sfnetwork.
  x_new = sfnetwork_(nodes, all_edges, directed = directed)
  ## ============================================
  # STEP VI: REMOVE PSEUDO NODES FROM THE NETWORK
  # Remove all the detected pseudo nodes from the original network.
  # This will automatically also remove their incident edges.
  # Remember that their replacement edges have already been added in step IV.
  # From and to indices will be updated automatically.
  ## ============================================
  x_new = delete_vertices(x_new, pseudo) %preserve_all_attrs% x
  ## =================================
  # STEP VII: POST-PROCESS AND RETURN
  ## =================================
  # Store original data if requested.
  if (store_original_data) {
    x_new = add_original_edge_data(x_new, edges)
  }
  # Remove original indices if requested.
  if (! store_original_ids) {
    x_new = drop_original_ids(x_new)
  }
  x_new
}
