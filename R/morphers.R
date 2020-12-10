#' Spatial morphers for sfnetworks
#'
#' Spatial morphers form spatial add-ons to the set of
#' \code{\link[tidygraph]{morphers}} provided by \code{tidygraph}. These
#' functions are not meant to be called directly. They should either be passed
#' into \code{\link[tidygraph]{morph}} to create a temporary alternate
#' representation of the input network. Such an alternate representation is a
#' list of one or more network objects. Single elements of that list can be
#' extracted directly as a new network by passing the morpher to
#' \code{\link[tidygraph]{convert}} instead. Alternatively, if the morphed
#' state contains multiple elements, all of them can be extracted together
#' inside a \code{\link[tibble]{tibble}} by passing the morpher to
#' \code{\link[tidygraph]{crystallise}}.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments to be passed on to other functions. See the description
#' of each morpher for details.
#'
#' @return Either a \code{morphed_sfnetwork}, which is a list of one or more
#' \code{\link{sfnetwork}} objects, or a \code{morphed_tbl_graph}, which is a
#' list of one or more \code{\link[tidygraph]{tbl_graph}} object. See the
#' description of each morpher for details.
#'
#' @details It also possible to create your own morphers. See the documentation
#' of \code{\link[tidygraph]{morph}} for the requirements for custom morphers.
#'
#' @examples
#' library(sf)
#' library(tidygraph)
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

#' @describeIn spatial_morphers Store the spatial coordinates of the nodes in
#' separate coordinate columns, instead of a \code{\link[sf]{sfc}} geometry
#' list column. If edges are spatially explicit, the edge geometries are
#' dropped. Returns a \code{morphed_tbl_graph} containing a single element of
#' class \code{\link[tidygraph]{tbl_graph}}.
#' @importFrom rlang !!!
#' @importFrom sf st_coordinates
#' @importFrom tidygraph mutate
#' @export
to_spatial_coordinates = function(x) {
  # Drop edge geometries if present.
  if (has_spatially_explicit_edges(x)) x = drop_edge_geom(x)
  # Create X and Y coordinate columns for the nodes.
  # Drop original node geometries.
  x_new = activate(x, "nodes")
  x_new = mutate(x_new, !!!as.data.frame(st_coordinates(x_new)))
  x_new = drop_node_geom(x_new)
  # Return in a list.
  list(
    coords = x_new %preserve_active% x
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
#' @export
to_spatial_subdivision = function(x) {
  require_spatially_explicit_edges(x)
  raise_assume_constant("to_spatial_subdivision")
  list(
    subdivision = subdivide(x) %preserve_active% x
  )
}

#' @importFrom igraph is_directed
#' @importFrom sf st_crs st_geometry
#' @importFrom sfheaders sf_to_df sfc_linestring sfc_point
subdivide = function(x) {
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
  node_idxs = rep(NA, nrow(edge_pts))
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
  new_nodes = nodes[orig_node_idxs, ]
  st_geometry(new_nodes) = new_node_geoms
  # Remove duplicated nodes from the new nodes table.
  new_nodes = new_nodes[!duplicated(new_node_idxs), ]
  # Create new network.
  sfnetwork_(new_nodes, new_edges, directed = is_directed(x))
}

#' @describeIn spatial_morphers Make a network directed in the direction given
#' by the linestring geometries of the edges. Differs from
#' \code{\link[tidygraph]{to_directed}}, which makes a network directed based
#' on the node indices given in the \code{from} and \code{to} columns. In
#' undirected networks these indices may not correspond with the endpoints of
#' the linestring geometries. Returns a \code{morphed_sfnetwork} containing a
#' single element of class \code{\link{sfnetwork}}. This morpher requires edges
#' to be spatially explicit. If not, use \code{\link[tidygraph]{to_directed}}.
#'
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
  x_new = join_nodes(x_new, nodes_as_sf(x), join = st_equals)
  # Return in a list.
  list(
    directed = x_new %preserve_active% x
  )
}

#' @describeIn spatial_morphers Create linestring geometries between from and
#' to nodes of spatially implicit edges. If the edges data can be directly
#' converted to an object of class \code{\link[sf]{sf}} using
#' \code{\link[sf]{st_as_sf}}, extra arguments can be provided as \code{...}
#' which will be forwarded to st_as_sf internally. Otherwise, straight lines
#' will be drawn between source and target nodes of edges. Returns a
#' \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}.
#' @importFrom rlang !! :=
#' @importFrom sf st_as_sf st_geometry
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph mutate
#' @export
to_spatial_explicit_edges = function(x, ...) {
  args = list(...)
  if (length(args) > 0) {
    # Convert edges to sf by forwarding ... to st_as_sf.
    e = as_tibble(as_tbl_graph(x), "edges")
    e_sf = st_as_sf(e, ...)
    geom_colname = attr(e_sf, "sf_column")
    # Add geometries of created sf object to the edges table of the network.
    x_new = mutate(activate(x, "edges"), !!geom_colname := st_geometry(e_sf))
    st_geometry(x_new) = geom_colname
    # Return in a list.
    list(
      explicit = x_new %preserve_active% x
    )
  } else {
    list(
      explicit = explicitize_edges(x)
    )
  }
}

#' @describeIn spatial_morphers Remove linestring geometries of spatially
#' explicit edges. Returns a \code{morphed_sfnetwork} containing a single
#' element of class \code{\link{sfnetwork}}.
#' @export
to_spatial_implicit_edges = function(x) {
  list(
    implicit = implicitize_edges(x)
  )
}

#' @describeIn spatial_morphers Limit a network to those nodes and edges that
#' are part of the shortest path between two nodes. \code{...} is evaluated in
#' the same manner as \code{\link{st_network_paths}}. Returns a
#' \code{morphed_sfnetwork} that may contain multiple elements of class
#' \code{\link{sfnetwork}}, depending on the number of requested paths. When
#' unmorphing only the first instance of both the node and edge data will be
#' used, as the the same node and/or edge can be present in multiple paths.
#' @importFrom tidygraph slice
#' @export
to_spatial_shortest_paths = function(x, ...) {
  args = list(...)
  args$x = x
  args$output = "both"
  # Call st_network_paths with the given arguments.
  paths = do.call("st_network_paths", args)
  # Subset the network for each computed shortest path.
  get_single_path = function(i) {
    x_new = slice(activate(x, "edges"), as.integer(paths$edge_paths[[i]]))
    x_new = slice(activate(x_new, "nodes"), as.integer(paths$node_paths[[i]]))
    x_new %preserve_active% x
  }
  lapply(seq_len(nrow(paths)), get_single_path)
}

#' @describeIn spatial_morphers Remove loops and parallel edges. Returns a
#' \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}. This morpher requires edges to be spatially
#' explicit. If not, use \code{\link[tidygraph]{to_simple}}.
#'
#' @param remove_parallels Should parallel edges be removed. Defaults to
#' \code{TRUE}.
#'
#' @param remove_loops Should loops be remove. Defaults to \code{TRUE}.
#'
#' @importFrom tidygraph filter edge_is_loop edge_is_multiple
#' @export
to_spatial_simple = function(x, remove_parallels = TRUE, remove_loops = TRUE) {
  # Activate edges.
  x_new = activate(x, "edges")
  # Remove parallels if requested.
  if (remove_parallels) {
    x_new = filter(x_new, !edge_is_multiple())
  }
  # Remove loops if requested.
  if (remove_loops) {
    x_new = filter(x_new, !edge_is_loop())
  }
  # Return in a list.
  list(
    simple = x_new %preserve_active% x
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
#' @importFrom igraph add_edges degree delete_edges delete_edge_attr edge_attr
#' edge_attr_names incident is_directed neighbors
#' @importFrom sf st_equals st_geometry st_reverse
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph filter mutate
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
to_spatial_smooth = function(x) {
  # Check if edges are spatially explicit and if network is directed.
  spatial = has_spatially_explicit_edges(x)
  directed = is_directed(x)
  # Extract edges and geometries of nodes from x.
  edges = if (spatial) edges_as_sf(x) else as_tibble(as_tbl_graph(x), "edges")
  nodes = node_geom(x)
  # Extract geometry column name from edges.
  edge_geom_colname = attr(edges, "sf_column")
  # Find pseudo nodes in x.
  if (directed) {
    # A node is a pseudo node if its in degree is 1 and its out degree is 1.
    pseudo = degree(x, mode = "in") == 1 & degree(x, mode = "out") == 1
  } else {
    # A node is a pseudo node if its degree is 2.
    pseudo = degree(x) == 2
  }
  # Pre-process.
  # --> Create network to be updated iteratively when processing pseudo nodes.
  # --> Separate the original edge indices column.
  # --> Separate the geometry column if present.
  # --> These columns will be updated separately from the network structure.
  G = x
  I = edges$.tidygraph_edge_index
  G = delete_edge_attr(x, ".tidygraph_edge_index")
  if (spatial) {
    L = st_geometry(edges)
    G = drop_edge_geom(G)
  }
  # Initialize a progress bar for large networks.
  # Since the function is still quite slow.
  print_pb = if (length(pseudo) > 500) TRUE else FALSE
  if (print_pb) {
    my_pb = txtProgressBar(
      min = sum(!pseudo),
      max = length(pseudo),
      initial = 0,
      style = 3
    )
  }
  # Iteratively process pseudo nodes:
  # --> Find incident edges to the node.
  # --> Concatenate the incident edges and add this as a new edge.
  # --> Remove the original incident edges.
  # --> Update the original edge indices data accordingly.
  # --> Update the edge geometries accordingly.
  # --> Repeat until all pseudo nodes are processed.
  pseudo_remaining = pseudo
  while (any(pseudo_remaining)) {
    # Get the index j and geometry p of the processed pseudo node.
    j = which(pseudo_remaining)[1]
    p = nodes[j]
    # Find the indices of incidents edges and neighboring nodes.
    if (directed) {
      incidents = as.integer(c(incident(G, j, "in"), incident(G, j, "out")))
      neighbors = as.integer(c(neighbors(G, j, "in"), neighbors(G, j, "out")))
    } else {
      incidents = as.integer(incident(G, j))
      neighbors = as.integer(neighbors(G, j))
    }
    # If there is only one indicent edge:
    # --> This means this edge is a loop.
    # --> There is no need to add a new, concatenated edge.
    # --> Hence, mark node as processed and move on to the next iteration.
    if (length(incidents) == 1) {
      pseudo_remaining[j] = FALSE
      next
    }
    # If equal attributes of incident edges are required:
    # --> Check for each edge attr if it has the same value for both incidents.
    # --> If not all attr values are equal the node is not a real pseudo node.
    # --> In that case, mark node as non-pseudo and move on to next iteration.
    if (require_equal_attrs) {
      eq = sapply(edge_attr(G), function(x) x[incidents[1]] == x[incidents[2]])
      if (! all(eq, na.rm = TRUE)) {
        pseudo[j] = FALSE
        pseudo_remaining[j] = FALSE
        next
      }
    }
    # Process the pseudo node by:
    # --> Removing its incident edges.
    # --> Adding a new edge between its neighbors.
    # --> Note that we don't remove the node itself at this stage.
    # --> This will happen all at once during post-processing.
    G = delete_edges(G, incidents)
    G = add_edges(G, neighbors)
    # Update the original indices object of the edges accordingly.
    i = list(c(unlist(I[incidents[1]]), unlist(I[incidents[2]])))
    I = c(I[-incidents], i)
    # Update the geometry object of the edges accordingly.
    if (spatial) {
      # Extract geometries of the incident edges.
      l1 = L[incidents[1]]
      l2 = L[incidents[2]]
      # In directed networks:
      # --> The pseudo node is always the endpoint of the 1st incident edge.
      # --> The pseudo node is always the startpoint of the 2nd indicdent edge.
      # In undirected networks, it may be that:
      # --> The pseudo node is the startpoint of the 1st incident edge.
      # --> The pseudo node is the endpoint of the 2nd incident edge.
      # In those cases, we can not easily concatenate the incident edges.
      # Hence, we should re-arrange their geometries first.
      if (! directed) {
        ep = linestring_boundary_points(l1)[2]
        if (! st_equals(ep, p, sparse = FALSE)) {
          l1 = st_reverse(l1)
        }
        sp = linestring_boundary_points(l2)[1]
        if (! st_equals(sp, p, sparse = FALSE)) {
          l2 = st_reverse(l2)
        }
      }
      # Concatenate the two incident edges.
      l = concat_lines(l1, l2)
      # Remove the original incident edge geometries.
      # Add the concatenated edge geometry.
      L = c(L[-incidents], l)
    }
    # Mark node as processed.
    pseudo_remaining[j] = FALSE
    if (print_pb) {
      setTxtProgressBar(my_pb, value = sum(!pseudo_remaining))
    }
  }
  # Post-process.
  # --> Convert the updated network back to a sfnetwork.
  x_new = tbg_to_sfn(as_tbl_graph(G))
  # --> Remove attributes from edges.
  for (i in edge_attr_names(x_new)) x_new = delete_edge_attr(x_new, i)
  # --> Add original indices of concatenated edges.
  # --> Add original data of concatenated edges.
  edge_attr(x_new, ".tidygraph_edge_index") = I
  edge_attr(x_new, ".orig_data") = lapply(I, function(i) edges[i, , drop = F])
  # --> Add updated edge geometries.
  if (spatial) x_new = mutate(activate(x_new, "edges"), !!edge_geom_colname := L)
  # --> Remove pseudo nodes all at once.
  x_new = filter(activate(x_new, "nodes"), !pseudo)
  # Return in a list.
  list(
    smooth = x_new %preserve_active% x
  )
}

#' @describeIn spatial_morphers Subset the network by applying a spatial
#' filter, i.e. a filter on the geometry column based on a spatial predicate.
#' \code{...} is evaluated in the same manner as \code{\link{st_filter}}.
#' Returns a \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}. For filters on an attribute column, use
#' \code{\link[tidygraph]{to_subgraph}}.
#'
#' @param subset_by Whether to create subgraphs based on nodes or edges.
#'
#' @importFrom sf st_filter
#' @export
to_spatial_subset = function(x, ..., subset_by = NULL) {
  if (is.null(subset_by)) {
    subset_by = attr(x, "active")
    message("Subsetting by ", subset_by)
  }
  x_new = switch(
    subset_by,
    nodes = st_filter(activate(x, "nodes"), ...),
    edges = st_filter(activate(x, "edges"), ...),
    raise_unknown_input(subset_by)
  )
  list(
    subset = x_new %preserve_active% x
  )
}
