#' Spatial morphers for sfnetwork objects
#'
#' These functions are meant to be passed into [morph()] to create a temporary
#' alternate representation of the input graph. They are thus not meant to be
#' called directly. See below for detail of each morpher. For a more detailed
#' explanations on how to use morphing, see \code{\link[tidygraph]{morph}}.
#'
#' @param graph An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments to be passed on to other function. See the description
#' of each morpher for details.
#'
#' @param subset_by Whether to create subgraphs based on nodes or edges.
#'
#' @return A list of \code{\link{sfnetwork}} objects.
#'
#' @name spatial_morphers
NULL

#' @describeIn spatial_morphers Store the spatial coordinates of the nodes in an 
#' 'X' and Y' column, instead of an \code{\link[sf]{sfc}} geometry list column.
#' @importFrom sf st_coordinates
#' @importFrom tidygraph mutate
#' @export
to_spatial_coordinates = function(graph) {
  # Create X and Y coordinate columns for the nodes.
  nodes_graph = activate(graph, "nodes")
  coords = st_coordinates(nodes_graph)
  coords_graph = tidygraph::mutate(
    nodes_graph, 
    X = coords[, "X"], 
    Y = coords[, "Y"]
  )
  # Drop edge geometries if present.
  if (has_spatially_explicit_edges(coords_graph)) {
    coords_graph = drop_geometry(coords_graph, "edges")
  }
  # Drop original node geometries.
  coords_graph = drop_geometry(coords_graph, "nodes")
  # Return in a list.
  list(
    coords_graph = coords_graph %preserve_active% graph
  )
}

#' @describeIn spatial_morphers Reconstruct the network by using all edge 
#' linestring points as nodes, instead of only the endpoints.
#' @importFrom lwgeom st_split
#' @importFrom sf st_cast st_collection_extract
#' @export
to_spatial_dense_graph = function(graph) {
  expect_spatially_explicit_edges(graph)
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
#' @importFrom sf st_geometry
#' @importFrom tidygraph convert reroute to_directed
#' @export
to_spatial_directed = function(graph) {
  expect_spatially_explicit_edges(graph)
  # Convert to a directed tbl_graph.
  # Force conversion to sfnetwork (i.e. without valid sfnetwork structure).
  graph = tidygraph::convert(as_tbl_graph(graph), to_directed)
  graph = tbg_to_sfn(graph)
  # Get boundary node indices.
  node_ids = get_boundary_node_indices(graph, out = "both")
  from_ids = node_ids[, 1]
  to_ids = node_ids[, 2]
  # Get nodes and edge geometries.
  nodes = sf::st_geometry(st_as_sf(graph, "nodes"))
  edges = sf::st_geometry(st_as_sf(graph, "edges"))
  # Get source node geometries of all edges.
  srcnodes = nodes[from_ids]
  # Get startpoint geometries of all edges.
  stpoints = lwgeom::st_startpoint(edges)
  # Retrieve the edges that don't have a matching source node and startpoint.
  invalid = which(!same_geometries(srcnodes, stpoints))
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
  args$graph = graph
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

#' @describeIn spatial_morphers Limit a graph to a single spatial subset. 
#' \code{...} is evaluated in the same manner as \code{\link{st_filter}}. 
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
    stop("Only possible to subset by nodes and edges")
  )
  list(
    sub_graph = sub_graph %preserve_active% graph
  )
}