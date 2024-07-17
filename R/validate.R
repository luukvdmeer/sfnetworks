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
#' @importFrom cli cli_alert cli_alert_success
#' @export
validate_network = function(x, message = TRUE) {
  nodes = pull_node_geom(x)
  # Check 1: Are all node geometries points?
  if (message) cli_alert("Checking node geometry types ...")
  if (! has_single_geom_type(nodes, "POINT")) {
    stop(
      "Not all nodes have geometry type POINT",
      call. = FALSE
    )
  }
  if (message) cli_alert_success("All nodes have geometry type POINT")
  if (has_explicit_edges(x)) {
    edges = pull_edge_geom(x)
    # Check 2: Are all edge geometries linestrings?
    if (message) cli_alert("Checking edge geometry types ...")
    if (! has_single_geom_type(edges, "LINESTRING")) {
      stop(
        "Not all edges have geometry type LINESTRING",
        call. = FALSE
      )
    }
    if (message) cli_alert_success("All edges have geometry type LINESTRING")
    # Check 3: Is the CRS of the edges the same as of the nodes?
    if (message) cli_alert("Checking coordinate reference system equality ...")
    if (! have_equal_crs(nodes, edges)) {
      stop(
        "Nodes and edges do not have the same coordinate reference system",
        call. = FALSE
      )
    }
    if (message) cli_alert_success("Nodes and edges have the same crs")
    # Check 4: Is the precision of the edges the same as of the nodes?
    if (message) cli_alert("Checking coordinate precision equality ...")
    if (! have_equal_precision(nodes, edges)) {
      stop(
        "Nodes and edges do not have the same coordinate precision",
        call. = FALSE
      )
    }
    if (message) cli_alert_success("Nodes and edges have the same precision")
    # Check 5: Do the edge boundary points match their corresponding nodes?
    if (message) cli_alert("Checking if geometries match ...")
    if (is_directed(x)) {
      # Start point should match start node.
      # End point should match end node.
      if (! all(nodes_match_edge_boundaries(x))) {
        stop(
          "Node locations do not match edge boundaries",
          call. = FALSE
        )
      }
    } else {
      # Start point should match either start or end node.
      # End point should match either start or end node.
      if (! all(nodes_in_edge_boundaries(x))) {
        stop(
          "Node locations do not match edge boundaries",
          call. = FALSE
        )
      }
    }
    if (message) cli_alert_success("Node locations match edge boundaries")
  }
  if (message) cli_alert_success("Spatial network structure is valid")
}

#' Proceed only when a given network element is active
#'
#' @details These function are meant to be called in the context of an
#' operation in which the network that is currently being worked on is known
#' and thus not needed as an argument to the function.
#'
#' @return Nothing when the expected network element is active, an error
#' message otherwise.
#'
#' @name require_active
#' @importFrom tidygraph .graph_context
#' @noRd
require_active_nodes <- function() {
  if (!.graph_context$free() && .graph_context$active() != "nodes") {
    stop(
      "This call requires nodes to be active",
      call. = FALSE
    )
  }
}

#' @name require_active
#' @importFrom tidygraph .graph_context
#' @noRd
require_active_edges <- function() {
  if (!.graph_context$free() && .graph_context$active() != "edges") {
    stop(
      "This call requires edges to be active",
      call. = FALSE
    )
  }
}

#' Proceed only when edges are spatially explicit
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param hard Is it a hard requirement, meaning that edges need to be
#' spatially explicit no matter which network element is active? Defaults to
#' \code{FALSE}, meaning that the error message will suggest to activate nodes
#' instead.
#'
#' @return Nothing when the edges of x are spatially explicit, an error message
#' otherwise.
#'
#' @noRd
require_explicit_edges = function(x, hard = FALSE) {
  if (! has_explicit_edges(x)) {
    if (hard) {
      stop(
        "This call requires spatially explicit edges",
        call. = FALSE
      )
    } else{
      stop(
        "This call requires spatially explicit edges when applied to the ",
        "edges table. Activate nodes first?",
        call. = FALSE
      )
    }
  }
}

#' Proceed only when the network has a valid sfnetwork structure
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param message Should messages be printed during validation? Defaults to
#' \code{TRUE}.
#'
#' @return Nothing when the network has a valid sfnetwork structure, an error
#' message otherwise.
#'
#' @noRd
require_valid_network_structure = function(x, message = FALSE) {
  validate_network(x, message)
}
