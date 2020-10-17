#' Update geometries to their nearest point on the network
#'
#' Implementation of snapping techniques that find the nearest point on a
#' spatial network to a given set of input geometries.
#'
#' @param x The spatial features to be snapped, either as object of class
#' \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @param graph The network to be snapped to as object object of class
#' \code{\link{sfnetwork}}.
#'
#' @param method The snapping technique to be used. See details. Defaults to
#' \code{'nearest_node'}.
#'
#' @details Implemented snapping techniques are.
#' * \code{'nearest_node'}: Find the nearest node to the given input geometry.
#' * \code{'nearest_point_on_edge'}: Finds the nearest point on the nearest
#' edge to the given input geometry, no matter if this point is a node in the
#' network or a line vertex in the linestring geometry of the edge. This
#' technique requires spatially explicit edges.
#' @md
#' 
#' @return An object of class \code{\link[sf]{sfc}}, containing the snapped 
#' geometries of x.
#' 
#' @export
st_snap_to_network = function(x, graph, method = "nearest_node") {
  switch(
    method,
    nearest_node = nearest_node(x, graph),
    nearest_point_on_edge = nearest_point_on_edge(x, graph),
    stop("Unknown snapping technique: ", method, call. = FALSE)
  )
}

#' @importFrom sf st_nearest_feature
nearest_node = function(x, graph) {
  # Extract node geometries from the given network.
  nodes = st_geometry(graph, "nodes")
  # Return the geometries of the nearest nodes.
  nodes[sf::st_nearest_feature(x, nodes)]
}

#' @importFrom sf st_boundary st_cast st_crs st_geometry 
#' st_nearest_feature st_nearest_points st_sfc
nearest_point_on_edge = function(x, graph) {
  require_spatially_explicit_edges(graph)
  # Extract edge geometries from the given network.
  edges = st_geometry(graph, "edges")
  # Function to find nearest point on edge for a single sfg object.
  find_nearest_point_on_edge = function(p) {
    # Convert sfg to sfc.
    p = sf::st_sfc(p, crs = sf::st_crs(x))
    # Find out which is the nearest edge to p.
    l = edges[sf::st_nearest_feature(p, edges)]
    # Run st_nearest_points with p and its nearest edge.
    # This returns linestring from p to the nearest point on the edge.
    # The point we are interested in is the last point of thet line.
    sf::st_cast(sf::st_boundary(sf::st_nearest_points(p, l)), "POINT")[2]
  }
  # Run for all given input points.
  do.call("c", lapply(sf::st_geometry(x), find_nearest_point_on_edge))
}