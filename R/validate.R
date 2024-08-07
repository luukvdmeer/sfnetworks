#' Validate the structure of a sfnetwork object
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param message Should messages be printed during validation? Defaults to
#' \code{TRUE}.
#'
#' @return Nothing when the network is valid. Otherwise, an error is thrown.
#'
#' @details A valid sfnetwork structure means that all nodes have \code{POINT}
#' geometries, and - when edges are spatially explicit - all edges have
#' \code{LINESTRING} geometries, nodes and edges have the same coordinate
#' reference system and the same coordinate precision, and coordinates of
#' edge boundaries match coordinates of their corresponding nodes.
#'
#' @importFrom cli cli_abort cli_alert cli_alert_success
#' @export
validate_network = function(x, message = TRUE) {
  nodes = pull_node_geom(x)
  # Check 1: Are all node geometries points?
  if (message) cli_alert("Checking node geometry types ...")
  if (! has_single_geom_type(nodes, "POINT")) {
    cli_abort("Not all nodes have geometry type POINT")
  }
  if (message) cli_alert_success("All nodes have geometry type POINT")
  if (has_explicit_edges(x)) {
    edges = pull_edge_geom(x)
    # Check 2: Are all edge geometries linestrings?
    if (message) cli_alert("Checking edge geometry types ...")
    if (! has_single_geom_type(edges, "LINESTRING")) {
      cli_abort("Not all edges have geometry type LINESTRING")
    }
    if (message) cli_alert_success("All edges have geometry type LINESTRING")
    # Check 3: Is the CRS of the edges the same as of the nodes?
    if (message) cli_alert("Checking coordinate reference system equality ...")
    if (! have_equal_crs(nodes, edges)) {
      cli_abort("Nodes and edges do not have the same coordinate reference system")
    }
    if (message) cli_alert_success("Nodes and edges have the same crs")
    # Check 4: Is the precision of the edges the same as of the nodes?
    if (message) cli_alert("Checking coordinate precision equality ...")
    if (! have_equal_precision(nodes, edges)) {
      cli_abort("Nodes and edges do not have the same coordinate precision")
    }
    if (message) cli_alert_success("Nodes and edges have the same precision")
    # Check 5: Do the edge boundary points match their corresponding nodes?
    if (message) cli_alert("Checking if geometries match ...")
    if (is_directed(x)) {
      # Start point should match start node.
      # End point should match end node.
      if (! all(nodes_match_edge_boundaries(x))) {
        cli_abort("Node locations do not match edge boundaries")
      }
    } else {
      # Start point should match either start or end node.
      # End point should match either start or end node.
      if (! all(nodes_in_edge_boundaries(x))) {
        cli_abort("Node locations do not match edge boundaries")
      }
    }
    if (message) cli_alert_success("Node locations match edge boundaries")
  }
  if (message) cli_alert_success("Spatial network structure is valid")
}
