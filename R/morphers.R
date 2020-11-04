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
#'     st_transform(3035)
#'
#' net %>%
#'     convert(to_spatial_coordinates)
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

#' @describeIn spatial_morphers Reconstruct the network by treating all points
#' that shape geometries of edge linestrings as nodes, instead of only the
#' endpoints. Returns a \code{morphed_sfnetwork} containing a single element of
#' class \code{\link{sfnetwork}}. This morpher requires edges to be spatially
#' explicit.
#'
#' @examples
#' par(mar = c(1, 1, 1, 1), mfrow = c(1,2))
#' plot(net)
#' net %>%
#'     convert(to_spatial_dense) %>%
#'     plot()
#'
#' @importFrom igraph is_directed
#' @importFrom lwgeom st_split
#' @importFrom sf st_as_sf st_cast st_collection_extract st_equals st_join
#' @export
to_spatial_dense = function(x) {
  require_spatially_explicit_edges(x)
  raise_assume_constant("to_spatial_dense")
  # Retrieve the edges from the network, without the to and from columns.
  edges = st_as_sf(x, "edges")
  edges[, c("from", "to")] = NULL
  # Split the edges by the points they are composed of.
  splitted_edges = suppressWarnings(st_split(edges, st_cast(edges, "POINT")))
  # The result is:
  # --> For each original edge a geometrycollection of sub-edges.
  # --> These sub-edges need to be extracted as linestrings.
  new_edges = st_collection_extract(splitted_edges, "LINESTRING")
  # Reconstruct the network with the new edges.
  x_new = as_sfnetwork(new_edges, directed = is_directed(x))
  # Spatial left join between nodes of x_new and original nodes of x.
  # This is needed since node attributes got lost when constructing x_new.
  x_new = st_join(x_new, st_as_sf(x, "nodes"), join = st_equals)
  # Return in a list.
  list(
    dense = x_new %preserve_active% x
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
#' net %>%
#'   activate("edges") %>%
#'   st_reverse() %>%
#'   convert(to_spatial_directed)
#'
#' @importFrom igraph is_directed
#' @importFrom sf st_as_sf st_equals st_join
#' @export
to_spatial_directed = function(x) {
  require_spatially_explicit_edges(x)
  if (is_directed(x)) return (x)
  # Retrieve the edges from the network, without the to and from columns.
  edges = st_as_sf(x, "edges")
  edges[, c("from", "to")] = NULL
  # Recreate the network as a directed one.
  x_new = as_sfnetwork(edges, directed = TRUE)
  # Spatial left join between nodes of x_new and original nodes of x.
  # This is needed since node attributes got lost when constructing x_new.
  x_new = st_join(x_new, st_as_sf(x, "nodes"), join = st_equals)
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
#' par(mar = c(1, 1, 1, 1), mfrow = c(1,2))
#' plot(net)
#' net %>%
#'  convert(to_spatial_implicit_edges) %>%
#'  convert(to_spatial_explicit_edges) %>%
#'  plot()
#'
#' @export
to_spatial_explicit_edges = function(x, ...) {
  args = list(...)
  if (length(args) > 0) {
    # Convert edges to sf by forwarding ... to st_as_sf.
    e = as_tibble(x, "edges")
    e_sf = st_as_sf(e, ...)
    x_new = activate(x, "edges")
    st_geometry(x_new) = st_geometry(e_sf)
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
#'
#' @examples
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
#' net %>%
#'   convert(to_spatial_shortest_paths, 171, 190)
#'
#' # Plot shortest paths:
#' par(mar = c(1, 1, 1, 1), mfrow = c(1,1))
#' plot(net)
#' plot(
#'   net %>% convert(to_spatial_shortest_paths, 171, 190),
#'   col = "red",
#'   add = TRUE
#' )
#'
#' # Calculate lengths of multiple shortest paths
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
#' @param keep Which geometry should be preserved when collapsing parallel
#' edges. Either \code{"longest"} or \code{"shortest"}. Defaults to
#' \code{"shortest"}.
#'
#' @examples
#' # Original number of edges
#' net %>%
#'   activate("edges") %>%
#'   st_as_sf() %>%
#'   nrow()
#' # Simplify parallel edges
#' net %>%
#'   activate("edges") %>%
#'   convert(to_spatial_simple) %>%
#'   st_as_sf() %>%
#'   nrow()
#'
#' @importFrom igraph is_directed
#' @importFrom sf st_as_sf st_length
#' @importFrom tibble as_tibble
#' @importFrom tidygraph convert to_simple
#' @export
to_spatial_simple = function(x, keep = "shortest", ...) {
  require_spatially_explicit_edges(x)
  # Retrieve the column name of geometry list column of the edges.
  geom_colname = edge_geom_colname(x)
  # Run tidygraphs to_simple morpher.
  # Extract edges from the result.
  x_tmp = convert(x, to_simple, ...)
  edges = as_tibble(x_tmp, "edges")
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
    nodes = st_as_sf(x_tmp, "nodes"),
    edges = st_as_sf(edges),
    directed = is_directed(x)
  )
  # Return in a list.
  list(
    simple = x_new %preserve_active% x
  )
}

#' @describeIn spatial_morphers Reconstruct the network by iteratively removing
#' pseudo nodes, while preserving the connectivity of the network. In the case
#' of directed networks, pseudo nodes are those nodes that have only one
#' incoming and one outgoing edge. In undirected networks, pseudo nodes are
#' those nodes that have two incident edges. Connectivity of the network is
#' preserved by merging the incident edges of each removed pseudo node. Returns
#' a \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}.
#' @param require_equal_attrs Should pseudo nodes only be removed when all
#' attributes of their incident edges are equal? Defaults to \code{FALSE}.
#' @examples
#' netw = as_sfnetwork(roxel[c(4, 5, 8), ], directed = FALSE)
#' # Remove pseudo nodes
#' netw_sparse1 = tidygraph::convert(netw, to_spatial_sparse)
#' # Only merge edges when their attributes are equal
#' netw_sparse2 = tidygraph::convert(netw, to_spatial_sparse, require_equal_attrs = TRUE)
#' # Compare results
#' par(mar = c(1, 1, 1, 1), mfrow = c(1,3))
#' plot(netw, cex = 3, main = "Network with pseudo nodes")
#' plot(netw_sparse1, cex = 3, main = "Pseudo nodes removed")
#' plot(netw_sparse2, cex = 3, main = "Only edges with equal attributes merged")
#'
#' @importFrom igraph incident is_directed neighbors
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom sf st_as_sf st_cast st_crs st_equals st_geometry st_reverse
#' st_sfc
#' @importFrom tibble as_tibble
#' @importFrom tidygraph centrality_degree filter mutate pull select with_graph
#' @export
to_spatial_sparse = function(x, require_equal_attrs = FALSE) {
  # Extract edges and nodes from x.
  nodes = st_as_sf(x, "nodes")
  edges = as_tibble(x, "edges")
  # Initialize the new network.
  x_new = x
  # Find pseudo nodes in x.
  if (is_directed(x)) {
    # A node is a pseudo node if its in degree is 1 and its out degree is 1.
    di = with_graph(x, centrality_degree(mode = "in"))
    do = with_graph(x, centrality_degree(mode = "out"))
    pseudo = di == 1 & do == 1
  } else {
    # A node is a pseudo node if its degree is 2.
    d = with_graph(x, centrality_degree())
    pseudo = d == 2
  }
  # Iteratively process pseudo nodes:
  # --> Find incident edges to the node.
  # --> Merge the incident edges into one to keep connectivity of the graph.
  # --> Remove the incident edges.
  # --> Repeat until all pseudo nodes are processed.
  pseudo_remaining = pseudo
  while (any(pseudo_remaining)) {
    # Extract edges from the network updated in the former iteration.
    E = st_as_sf(x_new, "edges")
    # Get the index and geometry of the node to be processed in this iteration.
    i = which(pseudo_remaining)[1]
    p = nodes[i, ]
    # Find the indices of incidents edges and neighboring nodes.
    if (is_directed(x)) {
      in_edge = incident(x_new, i, "in")
      out_edge = incident(x_new, i, "out")
      incidents = as.integer(c(in_edge, out_edge))
      in_node = neighbors(x_new, i, "in")
      out_node = neighbors(x_new, i, "out")
      neighbors = as.integer(c(in_node, out_node))
    } else {
      incidents = as.integer(incident(x_new, i))
      neighbors = as.integer(neighbors(x_new, i))
    }
    # Normally, there should be two indicent edges to a pseudo node.
    # If there is only one indicent edge, this means this edge is a loop.
    # In that case, mark node as processed and move on to the next iteration.
    if (length(incidents) == 1) {
      pseudo_remaining[i] = FALSE
      next
    }
    # Separate the incident edges from the edges data.
    E_i = E[incidents, ]
    E = E[-incidents, ]
    # If equal attributes of incident edges are required:
    # --> Check if the attributes of the edges are varying.
    # --> If yes, then the node is not a real pseudo node.
    # --> Mark node as non-pseudo and move on to the next iteration.
    if (require_equal_attrs) {
      E_i_attrs = E_i
      E_i_attrs$from = NULL
      E_i_attrs$to = NULL
      E_i_attrs$.tidygraph_edge_index = NULL
      if (has_varying_feature_attributes(E_i_attrs)) {
        pseudo[i] = FALSE
        pseudo_remaining[i] = FALSE
        next
      }
    }
    if (has_spatially_explicit_edges(x)) {
      # In directed networks, a pseudo node will always have:
      # --> One linestring moving towards the node (the in edge).
      # --> One linestring moving away from the node (the out edge).
      # In undirected networks, it can also have either 2 in or 2 out edges.
      # Note that in or out in that case does not mean anything.
      # But we do need arranged geometries to correctly merge the edges.
      # Hence, there is a need to rearrange the edges before proceeding.
      # The arrangement should be:
      # --> Edge one has a geometry that moves towards the pseudo node.
      # --> Edge two has a geometry that moves away from the pseudo node.
      if (! is_directed(x)) {
        if (! st_equals(st_endpoint(E_i[1, ]), p, sparse = FALSE)) {
          E_i[1, ] = st_reverse(E_i[1, ])
        }
        if (! st_equals(st_startpoint(E_i[2, ]), p, sparse = FALSE)) {
          E_i[2, ] = st_reverse(E_i[2, ])
        }
      }
      # Decompose the in and out edges into the points that shape them.
      # The pseudo node point is in both of them, so should be removed once.
      pts1 = st_cast(st_geometry(E_i[1, ]), "POINT")
      pts2 = st_cast(st_geometry(E_i[2, ]), "POINT")[-1]
      l = st_cast(do.call("c", c(pts1, pts2)), "LINESTRING")
      l = st_sfc(l, crs = st_crs(E))
    }
    # Merge the in and out edges into a single new edge.
    e = E_i[1, ]
    e$from = neighbors[1]
    e$to = neighbors[2]
    tei_1 = E_i[1, ]$.tidygraph_edge_index
    tei_2 = E_i[2, ]$.tidygraph_edge_index
    e$.tidygraph_edge_index = list(c(unlist(tei_1), unlist(tei_2)))
    if (has_spatially_explicit_edges(x)) st_geometry(e) = l
    # Reconstruct the network with the new edge added and old ones removed.
    x_new = sfnetwork_(nodes, rbind(E, e), directed = is_directed(x))
    # Update list of remaining pseudo nodes.
    pseudo_remaining[i] = FALSE
  }
  # Post-process:
  # --> Remove pseudo nodes from the new network.
  # --> Remove original attributes from edges.
  # --> Store the original edge data in an .orig_data column.
  x_new = activate(x_new, "nodes")
  x_new = filter(x_new, !pseudo)
  x_new = activate(x_new, "edges")
  x_new = select(x_new, c("from", "to", ".tidygraph_edge_index"))
  orig_data = lapply(
    pull(x_new, ".tidygraph_edge_index"),
    function(i) edges[i, , drop = FALSE]
  )
  x_new = mutate(x_new, .orig_data = orig_data)
  # Return in a list.
  list(
    sparse = x_new %preserve_active% x
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
#' library(tidygraph)
#' net = as_sfnetwork(roxel[c(4, 5, 8), ], directed = FALSE)
#' # Remove pseudo nodes.
#' smoothed_net_1 = convert(net, to_spatial_smooth)
#' # Only remove pseudo nodes when attributes of incident edges are equal.
#' smoothed_net_2 = convert(net, to_spatial_smooth, require_equal_attrs = TRUE)
#' # Compare results.
#' par(mar = c(1, 1, 1, 1), mfrow = c(1,3))
#' plot(net, cex = 3, main = "Original network")
#' plot(smoothed_net_1, cex = 3, main = "Smoothed network 1")
#' plot(smoothed_net_2, cex = 3, main = "Smoothed network 2")
#'
#' @importFrom igraph add_edges degree delete_edges delete_edge_attr edge_attr
#' edge_attr_names incident is_directed neighbors
#' @importFrom sf st_equals st_geometry st_reverse
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph filter
#' @export
to_spatial_smooth = function(x, require_equal_attrs = FALSE) {
  # Extract edges and geometries of nodes from x.
  edges = as_tibble(x, "edges")
  nodes = st_geometry(x, "nodes")
  # Check if edges are spatially explicit and if network is directed.
  spatial = has_sfc(edges)
  directed = is_directed(x)
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
  if (spatial) x_new = mutate_edge_geom(x_new, L)
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
#' @param subset_by Whether to create subgraphs based on nodes or edges.
#'
#' @examples
#' e1 = st_point(c(7.53173, 51.95662))
#' e2 = st_point(c(7.53173, 51.95190))
#' e3 = st_point(c(7.53778, 51.95190))
#' e4 = st_point(c(7.53778, 51.95662))
#'
#' rect = st_multipoint(c(e1, e2, e3, e4)) %>%
#'   st_cast('POLYGON') %>%
#'   st_sfc(crs = 4326) %>%
#'   st_transform(3035)
#'
#' net %>%
#'   convert(to_spatial_subset, rect, .predicate = st_intersects)
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
