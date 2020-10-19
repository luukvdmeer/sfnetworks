#' Spatial morphers for sfnetwork objects
#'
#' These functions are meant to be passed into [morph()] to create a temporary
#' alternate representation of the input graph. They are thus not meant to be
#' called directly. See below for detail of each morpher. For a more detailed
#' explanations on how to use morphing, see \code{\link[tidygraph]{morph}}.
#'
#' @param graph An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments to be passed on to other functions. See the description
#' of each morpher for details.
#'
#' @return A list of \code{\link{sfnetwork}} objects.
#'
#' @name spatial_morphers
NULL

#' @describeIn spatial_morphers Store the spatial coordinates of the nodes in 
#' separate coordinate columns (e.g. "X" and "Y"), instead of an 
#' \code{\link[sf]{sfc}} geometry list column. If edges are spatially
#' explicit, the edge geometries are dropped.
#' @export
to_spatial_coordinates = function(graph) {
  # Create X and Y coordinate columns for the nodes.
  coords_graph = add_coordinate_columns(activate(graph, "nodes"))
  # Drop edge geometries if present.
  if (has_spatially_explicit_edges(coords_graph)) {
    coords_graph = drop_edge_geom(coords_graph)
  }
  # Drop original node geometries.
  coords_graph = drop_node_geom(coords_graph)
  # Return in a list.
  list(
    coords_graph = coords_graph %preserve_active% graph
  )
}

#' @importFrom rlang !!!
#' @importFrom sf st_coordinates
#' @importFrom tidygraph mutate
add_coordinate_columns = function(x) {
  tidygraph::mutate(x, !!!as.data.frame(sf::st_coordinates(x)))
}

#' @describeIn spatial_morphers Reconstruct the network by using all edge 
#' linestring points as nodes, instead of only the endpoints.
#' @importFrom lwgeom st_split
#' @importFrom sf st_cast st_collection_extract
#' @export
to_spatial_dense_graph = function(graph) {
  require_spatially_explicit_edges(graph)
  # Retrieve the edges from the network, without the to and from columns.
  edges = st_as_sf(graph, "edges")
  edges[, c("from", "to")] = NULL
  # Split the edges by the points they are composed of.
  splitted_edges = lwgeom::st_split(edges, sf::st_cast(edges, "POINT"))
  # The result are for each original edge a geometrycollection of sub-edges.
  # These sub-edges need to be extracted as linestrings to form the new edges.
  new_edges = sf::st_collection_extract(splitted_edges, "LINESTRING")
  # Reconstruct the network with the new edges.
  list(
    dense_graph = as_sfnetwork(new_edges) %preserve_active% graph
  )
}

#' @describeIn spatial_morphers Make a graph directed in the direction given by 
#' the linestring geometries of the edges.
#' @importFrom lwgeom st_startpoint
#' @importFrom tidygraph convert reroute to_directed
#' @export
to_spatial_directed = function(graph) {
  require_spatially_explicit_edges(graph)
  # Convert to a directed tbl_graph.
  # Force conversion to sfnetwork (i.e. without valid sfnetwork structure).
  graph = tidygraph::convert(as_tbl_graph(graph), to_directed)
  graph = tbg_to_sfn(graph)
  # Get boundary node indices.
  # Returns a matrix with source ids in column 1 and target ids in column 2.
  node_ids = edge_boundary_node_indices(graph)
  from_ids = node_ids[, 1]
  to_ids = node_ids[, 2]
  # Get nodes and edge geometries.
  nodes = st_geometry(graph, "nodes")
  edges = st_geometry(graph, "edges")
  # Get source node geometries of all edges.
  srcnodes = nodes[from_ids]
  # Get startpoint geometries of all edges.
  stpoints = lwgeom::st_startpoint(edges)
  # Retrieve the edges that don't have a matching source node and startpoint.
  invalid = which(!have_equal_geometries(srcnodes, stpoints))
  # Swap from and to indices for those "invalid" edges.
  new_from_ids = from_ids
  new_from_ids[invalid] = to_ids[invalid]
  new_to_ids = to_ids
  new_to_ids[invalid] = from_ids[invalid]
  # Insert the new indices into the graph.
  repared_graph = tidygraph::reroute(
    as_tbl_graph(activate(graph, "edges")), 
    from = new_from_ids, 
    to = new_to_ids
  )
  # Convert to sfnetwork and return in list.
  list(
    directed_graph = tbg_to_sfn(repared_graph) %preserve_active% graph
  )
}

#' @describeIn spatial_morphers Draw linestring geometries for spatially
#' implicit edges.
#' @export
to_spatial_explicit_edges = function(graph) {
  list(
    explicit_graph = explicitize_edges(graph)
  )
}

#' @describeIn spatial_morphers Remove linestring geometries of spatially 
#' explicit edges.
#' @export
to_spatial_implicit_edges = function(graph) {
  list(
    implicit_graph = implicitize_edges(graph)
  )
}

#' @describeIn spatial_morphers Limit a graph to those nodes and edges that are
#' part of the shortest path between two nodes. If multiple \code{to} nodes are 
#' given, multiple shortest paths are returned. \code{...} is evaluated in the 
#' same manner as \code{\link{st_shortest_paths}}, except that the \code{output},
#' \code{predecessors} and \code{inbound.edges} arguments are ignored. When 
#' unmorphing only the first instance of both the node and edge data will be 
#' used, as the the same node and/or edge can be present in multiple paths.
#' @importFrom tidygraph slice
#' @export
to_spatial_shortest_paths = function(graph, ...) {
  args = list(...)
  args$x = graph
  args$output = "both"
  # Call st_shortest_paths with the given arguments.
  paths = do.call("st_shortest_paths", args)
  # Subset the graph for each single shortest path.
  get_single_path = function(i) {
    epath = tidygraph::slice(activate(graph, "edges"), as.integer(paths$epath[[i]]))
    npath = tidygraph::slice(activate(epath, "nodes"), as.integer(paths$vpath[[i]]))
    npath %preserve_active% graph
  }
  lapply(c(1:length(paths$vpath)), get_single_path)
}

#' @describeIn spatial_morphers Remove loops in a graph and collapse parallel
#' edges by keeping only one of them.
#' @param keep Which edge should be kept when collapsing parallel edges. Either
#' \code{"longest"} or \code{"shortest"}. Defaults to \code{"shortest"}.
#' \code{...} is passed on to \code{\link[tidygraph]{to_simple}}.
#' @importFrom sf st_length
#' @importFrom tidygraph convert to_simple
#' @export
to_spatial_simple = function(graph, keep = "shortest", ...) {
  require_spatially_explicit_edges(graph)
  # Retrieve the column name of geometry list column of the edges.
  geom_colname = edge_geom_colname(graph)
  # Run tidygraphs to_simple morpher.
  # Force conversion to sfnetwork (i.e. without valid sfnetwork structure).
  simple_graph = tidygraph::convert(graph, to_simple, ...)
  simple_graph = tbg_to_sfn(simple_graph)
  simple_graph = activate(simple_graph, "edges")
  edges = as_tibble(simple_graph)
  # For each of the edges that are a result of a merge of original edges:
  # Select the geometry of either the longest or shortest edge.
  select_edge = function(x) {
    if (length(x) == 1) {
      x 
    } else {
      lengths = sf::st_length(x)
      switch(
        keep,
        longest = x[which(lengths == max(lengths))][1],
        shortest = x[which(lengths == min(lengths))][1]
      )
    }
  }
  edges[[geom_colname]] = do.call(c, lapply(edges[[geom_colname]], select_edge))
  new_edges = sf::st_as_sf(edges)
  new_graph = sfnetwork(
    nodes = st_as_sf(simple_graph, "nodes"),
    edges = new_edges,
    directed = is_directed(graph),
    force = TRUE
  )
  list(
    simple_graph = new_graph %preserve_active% graph
  ) 
}

#' @describeIn spatial_morphers Reconstruct the network by iteratively removing 
#' all nodes that have only one incoming and one outgoing edge (or simply a 
#' degree centrality of 2 in the case of undirected networks), but at the same 
#' time preserving the connectivity of the graph by merging the incoming and
#' outgoing edge of the removed node.
#' @param require_equal_attrs Should selected nodes only be removed when the
#' attributes of their adjacent edges are equal? Defaults to \code{FALSE}.
#' @param store_original_data Should the original edge data be kept in a special
#' \code{.orig_data} column? Defaults to \code{TRUE}.
#' @importFrom sf st_boundary st_geometry st_cast st_reverse st_union
#' @importFrom tidygraph filter
#' @export
to_spatial_smoothed = function(graph, require_equal_attrs = FALSE,
                               store_original_data = TRUE) {
  require_spatially_explicit_edges(graph)
  # Check which nodes in the graph are pseudo nodes.
  pseudo = is_pseudo_node(activate(graph, "nodes"))
  # Initialize the smoothed graph.
  smoothed_graph = graph
  # Iteratively process pseudo nodes until none remain.
  # Preserve the connectivity of the graph while doing so.
  while (any(pseudo)) {
    # Retrieve the nodes and edges from the graph.
    nodes = st_as_sf(smoothed_graph, "nodes")
    node_geoms = sf::st_geometry(nodes)
    edges = st_as_sf(smoothed_graph, "edges")
    # Get the node index of the first pseudo node.
    # This is the one that will be processed in this iteration.
    idx = which(pseudo)[1]
    # Find the adjacent edges to the pseudo node.
    adj_edges = rbind(edges[edges$to == idx, ], edges[edges$from == idx, ])
    # Normally, there should be two adjacent edges to a pseudo node.
    # If there is only one adjacent edge, this means this edge is a loop.
    # In that case, filter out this node and edge and move on to next pseudo.
    if (nrow(adj_edges) == 1) {
      smoothed_graph = tidygraph::filter(smoothed_graph, !idx)
      pseudo[idx] = FALSE
      next
    }
    # If equal attributes of adjacent edges are required:
    # Check if the attributes of the edges are equal.
    # If not, then the node is not a real pseudo node.
    # Mark node as FALSE and move on to the next iteration.
    if (require_equal_attrs) {
      if (!have_equal_attributes(adj_edges[, !names(adj_edges) %in% c("from", "to")])) {
        pseudo[idx] = FALSE
        next
      }
    }
    # In directed networks, a pseudo node will always have:
    # - One linestring moving towards the node (the in edge).
    # - One linestring moving away from the node (the out edge).
    # In undirected networks, it can also have either 2 in or 2 out edges.
    # Note that in or out in that case does not mean anything.
    # But we do need arranged geometries to correctly merge the edges.
    # Hence, there is a need to rearrange the edges before proceeding.
    # The arrangement should be:
    # - Edge one has a geometry that moves towards the pseudo node.
    # - Edge two has a geometry that moves away from the pseudo node.
    if (!is_directed(graph)) {
      edge_1_bounds = sf::st_boundary(sf::st_geometry(adj_edges[1, ]))
      if (sf::st_cast(edge_1_bounds, "POINT")[1] == node_geoms[idx]) {
        adj_edges[1, ] = sf::st_reverse(adj_edges[1, ])
      }
      if (adj_edges[1, ]$from == idx) {
        adj_edges[1, ]$from = adj_edges[1, ]$to
        adj_edges[1, ]$to = idx
      }
      edge_2_bounds = sf::st_boundary(sf::st_geometry(adj_edges[2, ]))
      if (sf::st_cast(edge_2_bounds, "POINT")[2] == node_geoms[idx]) {
        adj_edges[2, ] = sf::st_reverse(adj_edges[2, ])
      }
      if (adj_edges[2, ]$to == idx) {
        adj_edges[2, ]$to = adj_edges[2, ]$from
        adj_edges[2, ]$from = idx
      }
    }
    # Decompose the in and out edges into their vertices.
    # The pseudo node vertice is in both of them, so should be removed once.
    in_pts = sf::st_cast(sf::st_geometry(adj_edges[1, ]), "POINT")
    out_pts = sf::st_cast(sf::st_geometry(adj_edges[2, ]), "POINT")[-1]
    line = sf::st_cast(do.call("c", c(in_pts, out_pts)), "LINESTRING")
    # Merge the in and out edges into a single edge.
    new_edge = adj_edges[1, ]
    new_edge$to = adj_edges[2, ]$to
    new_edge$.tidygraph_edge_index = list(
      c(
        unlist(adj_edges[1, ]$.tidygraph_edge_index),
        unlist(adj_edges[2, ]$.tidygraph_edge_index)
      )
    )
    sf::st_geometry(new_edge) = sf::st_sfc(line, crs = sf::st_crs(edges))
    # Reconstruct the graph with the new edge added.
    smoothed_graph = sfnetwork(
      nodes = nodes, 
      edges = rbind(edges, new_edge), 
      directed = is_directed(graph),
      force = TRUE
    )
    # Filter out the processed pseudo node and adjacent edges.
    smoothed_graph = tidygraph::slice(smoothed_graph, -idx)
    # Find pseudo nodes in the smoothed graph.
    pseudo = is_pseudo_node(smoothed_graph)
  }
  # Remove attributes from edges.
  smoothed_graph = activate(smoothed_graph, "edges")
  keep = c("from", "to", ".tidygraph_edge_index")
  smoothed_graph = tidygraph::select(smoothed_graph, keep)
  # Store the original edge data in a special column if requested.
  if (store_original_data) {
    orig_edges = st_as_sf(graph, "edges")
    orig_data = lapply(
      tidygraph::pull(smoothed_graph, ".tidygraph_edge_index"), 
      function(i) orig_edges[i, , drop = FALSE]
    )
    smoothed_graph = tidygraph::mutate(smoothed_graph, .orig_data = orig_data)
  }
  list(
    smoothed_graph = smoothed_graph %preserve_active% graph
  )
}

#' @importFrom tidygraph centrality_degree with_graph
is_pseudo_node = function(x) {
  if (is_directed(x)) {
    # A node is a pseudo node if its in degree is 1 and its out degree is 1.
    d_in = tidygraph::with_graph(x, tidygraph::centrality_degree(mode = "in"))
    d_out = tidygraph::with_graph(x, tidygraph::centrality_degree(mode = "out"))
    d_in == 1 & d_out == 1
  } else {
    # A node is a pseudo node if its degree is 2.
    d = tidygraph::with_graph(x, tidygraph::centrality_degree())
    d == 2
  }
}

#' @describeIn spatial_morphers Limit a graph to a single spatial subset. 
#' \code{...} is evaluated in the same manner as \code{\link{st_filter}}.
#' @param subset_by Whether to create subgraphs based on nodes or edges.
#' @export
to_spatial_subgraph = function(graph, ..., subset_by = NULL) {
  if (is.null(subset_by)) {
    subset_by = active(graph)
    message("Subsetting by ", subset_by)
  }
  sub_graph = switch(
    subset_by,
    nodes = st_filter(activate(graph, "nodes"), ...),
    edges = st_filter(activate(graph, "edges"), ...),
    stop("Only possible to subset by nodes and edges", call. = FALSE)
  )
  list(
    sub_graph = sub_graph %preserve_active% graph
  )
}