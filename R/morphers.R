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
  coords = sf::st_coordinates(st_as_sf(graph, "nodes"))
  coords_graph = tidygraph::mutate(graph, X = coords[, "X"], Y = coords[, "Y"])
  # Drop edge geometries if present.
  if (has_spatially_explicit_edges(coords_graph)) {
    coords_graph = drop_geometry(coords_graph, "edges")
  }
  # Drop original node geometries.
  coords_graph = drop_geometry(coords_graph, "nodes")
  # Return in a list.
  list(
    coords_graph = coords_graph
  )
}

#' @describeIn spatial_morphers Reconstruct the network by using all edge 
#' linestring points as nodes, instead of only the endpoints.
#' @importFrom lwgeom st_split
#' @importFrom sf st_cast st_collection_extract
#' @export
to_spatial_dense_graph = function(graph) {
  # Retrieve the edges from the network, without the to and from columns.
  if (! has_spatially_explicit_edges(graph)) {
    stop("This call requires spatially explicit edges")
  }
  edges = st_as_sf(graph, "edges")
  edges[, c("from", "to")] = NULL
  # Split the edges by the points they are composed of.
  splitted_edges = lwgeom::st_split(edges, sf::st_cast(edges, "POINT"))
  # The result are for each original edge a geometrycollection of sub-edges.
  # These sub-edges need to be extracted as linestrings to form the new edges.
  new_edges = sf::st_collection_extract(splitted_edges, "LINESTRING")
  # Reconstruct the network with the new edges.
  list(
    dense_graph = as_sfnetwork(new_edges)
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
    tidygraph::slice(activate(epath, "nodes"), as.integer(paths$vpath[[i]]))
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
  list(
    sub_graph = switch(
      subset_by,
      nodes = st_filter(activate(graph, "nodes"), ...),
      edges = st_filter(activate(graph, "edges"), ...),
      stop("Only possible to subset by nodes and edges")
    )
  )
}