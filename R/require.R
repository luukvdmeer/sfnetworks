#' Proceed only when edges are active
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return Nothing when the edges of x are activated, an error message
#' otherwise.
#'
#' @noRd
require_active_edges = function(x) {
  if (attr(x, "active") == "nodes") {
    stop(
      "This call requires the edges to be active", 
      call. = FALSE
    )
  }
}

#' Proceed only when nodes are active
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return Nothing when the nodes of x are activated, an error message
#' otherwise.
#'
#' @noRd
require_active_nodes = function(x) {
  if (attr(x, "active") == "edges") {
    stop(
      "This call requires the nodes to be active", 
      call. = FALSE
    )
  }
}

#' Proceed only when edges are spatially explicit
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return Nothing when the edges of x are spatially explicit, an error message
#' otherwise.
#'
#' @details Require will be called when the function needs spatially explicit
#' edges no matter which element is active. Expect will be called when the
#' function needs spatially explicit edges when applied to the edges, but will
#' still work when applied to the nodes. 
#'
#' @name check_explicitness
#' @noRd
require_spatially_explicit_edges = function(x) {
  if (! has_spatially_explicit_edges(x)) {
    stop(
      "This call requires spatially explicit edges", 
      call. = FALSE
    )
  }
}

#' @name check_explicitness
#' @noRd
expect_spatially_explicit_edges = function(x) {
  if (! has_spatially_explicit_edges(x)) {
    stop(
      "Edges are spatially implicit. Activate nodes first?", 
      call. = FALSE
    )
  }
}

#' Proceed only when the network has a valid sfnetwork structure
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param message Should a message be printed before and after the validation?
#' Default to \code{FALSE}.
#'
#' @return Nothing when the network has a valid sfnetwork structure, an error 
#' message otherwise.
#'
#' @details A valid sfnetwork structure means that all nodes have \code{POINT} 
#' geometries, and - when edges are spatially explicit - all edges have 
#' \code{LINESTRING} geometries, nodes and edges have the same CRS and 
#' coordinates of edge boundaries match coordinates of their corresponding
#' nodes.
#'
#' @noRd
require_valid_network_structure = function(x, message = FALSE) {
  if (message) message("Checking if spatial network structure is valid...")
  validate_nodes(x)  
  if (has_spatially_explicit_edges(x)) {    
  	validate_edges(x)    
  }
  if (message) message("Spatial network structure is valid")
}

#' @importFrom sf st_as_sf
validate_nodes = function(x) {
  nodes = nodes_as_sf(x)
  # --> Are all node geometries points?
  if (! has_single_geom_type(nodes, "POINT")) {
    stop(
      "Not all nodes have geometry type POINT", 
      call. = FALSE
    )
  }
}

#' @importFrom igraph is_directed
#' @importFrom sf st_as_sf
validate_edges = function(x) {
  nodes = nodes_as_sf(x)
  edges = edges_as_sf(x)
  # --> Are all edge geometries linestrings?
  if (! has_single_geom_type(edges, "LINESTRING")) {
    stop(
      "Not all edges have geometry type LINESTRING", 
      call. = FALSE
    )
  }
  # --> Is the CRS of the edges the same as of the nodes?
  if (! have_equal_crs(nodes, edges)) {
    stop(
      "Nodes and edges do not have the same CRS", 
      call. = FALSE
    )
  }
  # --> Do the edge boundary points match their corresponding nodes?
  if (is_directed(x)) {
    # Start point should match start node.
    # End point should match end node.
    if (! all(nodes_match_edge_boundaries(x))) {
      stop(
        "Edge boundaries do not match their corresponding nodes", 
        call. = FALSE
      )
    }
  } else {
    # Start point should match either start or end node.
    # End point should match either start or end node.
    if (! all(nodes_in_edge_boundaries(x))) {
      stop(
        "Edge boundaries do not match their corresponding nodes", 
        call. = FALSE
      )
    }
  }
}
