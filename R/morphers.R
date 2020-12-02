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
#' @name spatial_morphers
NULL

#' @describeIn spatial_morphers Store the spatial coordinates of the nodes in
#' separate coordinate columns, instead of a \code{\link[sf]{sfc}} geometry
#' list column. If edges are spatially explicit, the edge geometries are
#' dropped. Returns a \code{morphed_tbl_graph} containing a single element of
#' class \code{\link[tidygraph]{tbl_graph}}.
#'
#' @examples
#' library(sf)
#' library(tidygraph)
#'
#' net = as_sfnetwork(roxel, directed = FALSE) %>%
#'   st_transform(3035)
#'
#' ## to_spatial_coordinates
#' convert(net, to_spatial_coordinates)
#'
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
#' subdividing all edges at those points that are included in their linestring
#' geometry feature but are not endpoints of it. The network is reconstructed
#' afterwards such that edges which did share points in their geometries but
#' not endpoints are now connected as well. Returns a \code{morphed_sfnetwork}
#' containing a single element of class \code{\link{sfnetwork}}. This morpher
#' requires edges to be spatially explicit.
#'
#' @examples
#' ## to_spatial_subdivision
#' par(mar = c(1, 1, 1, 1), mfrow = c(1,2))
#' plot(net)
#' net %>%
#'   convert(to_spatial_subdivision) %>%
#'   plot()
#'
#' @importFrom igraph is_directed
#' @importFrom sf st_crs st_equals st_geometry
#' @importFrom sfheaders sf_linestring sf_to_df
#' @export
to_spatial_subdivision = function(x) {
  require_spatially_explicit_edges(x)
  raise_assume_constant("to_spatial_subdivision")
  # Retrieve the edges from the network, without the to and from columns.
  edges = edges_as_sf(x)
  edges[, c("from", "to")] = NULL
  # Get coordinate pairs of all edge points.
  coords = sf_to_df(edges)
  # Find indices of points that are edge boundaries (i.e. first or last point).
  first = !duplicated(coords[["sfg_id"]])
  last = !duplicated(coords[["sfg_id"]], fromLast = TRUE)
  # Select points where new edges should start from.
  # --> These are all points except the last one of each original edge.
  from_coords = coords[!last, ]
  from_coords$linestring_id = c(1:nrow(from_coords))
  # Select points where new edges should go to.
  # --> These are all points except the first one of each original edge. 
  to_coords = coords[!first, ]
  to_coords$linestring_id = c(1:nrow(to_coords))
  # Create new edges.
  # --> Note that sf_linestring needs the input coordinates ordered.
  new_coords = rbind(from_coords, to_coords)
  new_edges = sf_linestring(
    obj = new_coords[order(new_coords$linestring_id), ],
    x = "x", 
    y = "y", 
    linestring_id = "linestring_id"
  )
  st_crs(new_edges) = st_crs(x)
  # Copy original edge attributes to each part of the divided edge.
  st_geometry(edges) = NULL
  new_edges = cbind(new_edges, edges[coords[!first, "linestring_id"], ])  
  # Reconstruct the network with the new edges.
  x_new = as_sfnetwork(new_edges, directed = is_directed(x))
  # Spatial left join between nodes of x_new and original nodes of x.
  # This is needed since node attributes got lost when constructing x_new.
  x_new = join_nodes(x_new, nodes_as_sf(x), join = st_equals)
  # Return in a list.
  list(
    subdivision = x_new %preserve_active% x
  )
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
#' @examples
#' ## to_spatial_directed
#' net %>%
#'   activate("edges") %>%
#'   st_reverse() %>%
#'   convert(to_spatial_directed)
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
#'
#' @examples
#' ## to_spatial_explicit_edges
#' par(mar = c(1, 1, 1, 1), mfrow = c(1,2))
#' plot(net)
#' net %>%
#'  convert(to_spatial_implicit_edges) %>%
#'  convert(to_spatial_explicit_edges) %>%
#'  plot()
#'
#' @importFrom sf st_as_sf st_geometry
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
#' @export
to_spatial_explicit_edges = function(x, ...) {
  args = list(...)
  if (length(args) > 0) {
    # Convert edges to sf by forwarding ... to st_as_sf.
    e = as_tibble(as_tbl_graph(x), "edges")
    e_sf = st_as_sf(e, ...)
    list(
      explicit = mutate_edge_geom(x, st_geometry(e_sf))
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
#'
#' @examples
#' ## to_spatial_implicit_edges
#' net %>%
#'  convert(to_spatial_implicit_edges)
#'
#' @export
to_spatial_implicit_edges = function(x) {
  list(
    implicit = implicitize_edges(x)
  )
}

#' @describeIn spatial_morphers Limit a network to those nodes and edges that
#' are part of the shortest path between two nodes. \code{...} is evaluated in
#' the same manner as \code{\link{st_shortest_paths}}. Returns a
#' \code{morphed_sfnetwork} that may contain multiple elements of class
#' \code{\link{sfnetwork}}, depending on the number of requested paths. When
#' unmorphing only the first instance of both the node and edge data will be
#' used, as the the same node and/or edge can be present in multiple paths.
#'
#' @examples
#' ## to_spatial_shortest_paths
#' # Plot shortest path.
#' par(mar = c(1, 1, 1, 1), mfrow = c(1,1))
#' plot(net)
#' net %>%
#'   convert(to_spatial_shortest_paths, 171, 190) %>%
#'   plot(col = "red", add = TRUE)
#' # Calculate lengths of multiple shortest paths.
#' net %>%
#'  activate("edges") %>%
#'  morph(to_spatial_shortest_paths, 1, c(171, 190)) %>%
#'  sapply(function(x) sum(st_length(x)))
#'
#' @importFrom tidygraph slice
#' @export
to_spatial_shortest_paths = function(x, ...) {
  args = list(...)
  args$x = x
  args$output = "both"
  # Call st_shortest_paths with the given arguments.
  paths = do.call("st_shortest_paths", args)
  # Subset the network for each computed shortest path.
  get_single_path = function(i) {
    x_new = slice(activate(x, "edges"), as.integer(paths$edge_paths[[i]]))
    x_new = slice(activate(x_new, "nodes"), as.integer(paths$node_paths[[i]]))
    x_new %preserve_active% x
  }
  lapply(seq_len(nrow(paths)), get_single_path)
}

#' @describeIn spatial_morphers Remove loops in a graph and collapse parallel
#' edges. \code{...} is passed on to \code{\link[tidygraph]{to_simple}}.
#' Differs from \code{\link[tidygraph]{to_simple}} by assigning a single
#' linestring geometry to combined parallel edges. This is either the geometry
#' of the shortest or the longest of the parallel edges. Returns a
#' \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}. This morpher requires edges to be spatially
#' explicit. If not, use \code{\link[tidygraph]{to_simple}}.
#'
#' @param keep Which geometry should be preserved when collapsing parallel
#' edges. Either \code{"longest"} or \code{"shortest"}. Defaults to
#' \code{"shortest"}.
#'
#' @examples
#' ## to_spatial_simple
#' # Number of edges in original network.
#' net %>%
#'   activate("edges") %>%
#'   st_as_sf() %>%
#'   nrow()
#' # Number of edges in simplified network.
#' net %>%
#'   activate("edges") %>%
#'   convert(to_spatial_simple) %>%
#'   st_as_sf() %>%
#'   nrow()
#'
#' @importFrom igraph is_directed
#' @importFrom sf st_as_sf st_length
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph convert to_simple
#' @export
to_spatial_simple = function(x, keep = "shortest", ...) {
  require_spatially_explicit_edges(x)
  # Retrieve the column name of geometry list column of the edges.
  geom_colname = edge_geom_colname(x)
  # Run tidygraphs to_simple morpher.
  # Extract edges from the result.
  x_tmp = convert(x, to_simple, ...)
  edges = as_tibble(as_tbl_graph(x_tmp), "edges")
  # For each of the edges that are a result of a merge of original edges:
  # --> Select the geometry of either the longest or shortest edge.
  select_edge = function(e) {
    if (length(e) == 1) return (e)
    L = st_length(e)
    switch(
      keep,
      longest = e[which(L == max(L))][1],
      shortest = e[which(L == min(L))][1],
      raise_unknown_input(keep)
    )
  }
  edges[[geom_colname]] = do.call(c, lapply(edges[[geom_colname]], select_edge))
  x_new = sfnetwork_(
    nodes = nodes_as_sf(x_tmp),
    edges = st_as_sf(edges),
    directed = is_directed(x)
  )
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
#'
#' @param require_equal_attrs Should pseudo nodes only be removed when all
#' attributes of their incident edges are equal? Defaults to \code{FALSE}.
#'
#' @examples
#' ## to_spatial_smoothed
#' G = as_sfnetwork(roxel[c(4, 5, 8), ], directed = FALSE)
#' # Remove pseudo nodes.
#' smoothed_1 = convert(G, to_spatial_smooth)
#' # Only remove pseudo nodes when attributes of incident edges are equal.
#' smoothed_2 = convert(G, to_spatial_smooth, require_equal_attrs = TRUE)
#' # Compare results.
#' par(mar = c(1, 1, 1, 1), mfrow = c(1,3))
#' plot(G, cex = 3, main = "Original network")
#' plot(smoothed_1, cex = 3, main = "Smoothed network 1")
#' plot(smoothed_2, cex = 3, main = "Smoothed network 2")
#'
#' @importFrom igraph add_edges degree delete_edges delete_edge_attr edge_attr
#' edge_attr_names incident is_directed neighbors
#' @importFrom sf st_equals st_geometry st_reverse
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph filter mutate
#' @export
to_spatial_smooth = function(x, require_equal_attrs = FALSE) {
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
#' @examples
#' ## to_spatial_subset
#' p1 = st_point(c(7.53173, 51.95662))
#' p2 = st_point(c(7.53173, 51.95190))
#' p3 = st_point(c(7.53778, 51.95190))
#' p4 = st_point(c(7.53778, 51.95662))
#' rect = st_multipoint(c(p1, p2, p3, p4)) %>%
#'   st_cast('POLYGON') %>%
#'   st_sfc(crs = 4326) %>%
#'   st_transform(3035)
#' net %>%
#'   convert(to_spatial_subset, rect, .predicate = st_intersects)
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
