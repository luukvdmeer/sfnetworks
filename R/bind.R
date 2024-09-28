#' Add nodes or edges to a spatial network.
#'
#' These functions are the spatially aware versions of tidygraph's
#' \code{\link[tidygraph]{bind_nodes}} and \code{\link[tidygraph]{bind_edges}}
#' that allow you to add rows to the nodes or edges tables in a
#' \code{\link{sfnetwork}} object. As with \code{\link[dplyr]{bind_rows}}
#' columns are matched by name and filled with \code{NA} if the column does not
#' exist in some instances.
#'
#' @param .data An object of class \code{\link{sfnetwork}}.
#'
#' @param ... One or more objects of class \code{\link[sf]{sf}} containing the
#' nodes or edges to be added.
#'
#' @param node_key The name of the column in the nodes table that character
#' represented \code{to} and \code{from} columns should be matched against. If
#' \code{NA}, the first column is always chosen. This setting has no effect if
#' \code{to} and \code{from} are given as integers. Defaults to \code{'name'}.
#'
#' @param force Should network validity checks be skipped? Defaults to
#' \code{FALSE}, meaning that network validity checks are executed after binding
#' edges, making sure that boundary points of edges match their corresponding
#' node coordinates.
#'
#' @returns An object of class \code{\link{sfnetwork}} with added nodes or
#' edges.
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(dplyr, quietly = TRUE)
#'
#' net = roxel |>
#'   slice(c(1:2)) |>
#'   st_transform(3035) |>
#'   as_sfnetwork()
#'
#' pts = roxel |>
#'   slice(c(3:4)) |>
#'   st_transform(3035) |>
#'   st_centroid()
#'
#' bind_spatial_nodes(net, pts)
#'
#' @name bind_spatial
#' @importFrom cli cli_abort
#' @importFrom sf st_drop_geometry st_geometry
#' @importFrom tidygraph bind_nodes
#' @export
bind_spatial_nodes = function(.data, ...) {
  # Bind geometries
  net_geom = list(pull_node_geom(.data))
  add_geom = lapply(list(...), st_geometry)
  new_geom = do.call("c", c(net_geom, add_geom))
  # Validate if binded nodes are points.
  if (! are_points(new_geom)) {
    cli_abort("Not all nodes have geometry type {.cls POINT}")
  }
  # Bind other data.
  net = drop_node_geom(.data)
  add = lapply(list(...), st_drop_geometry)
  new_net = bind_nodes(net, add)
  # Add geometries back to the network.
  new_net = mutate_node_geom(new_net, new_geom)
  new_net
}

#' @name bind_spatial
#' @importFrom cli cli_abort
#' @importFrom igraph is_directed
#' @importFrom sf st_drop_geometry st_geometry
#' @importFrom tidygraph bind_edges
#' @export
bind_spatial_edges = function(.data, ..., node_key = "name", force = FALSE) {
  # If edges are not spatially explicit.
  # We can simply use tidygraphs bind_edges function without any additions.
  if (! has_explicit_edges(.data)) {
    if (any(do.call("c", lapply(list(...), has_sfc)))) {
      cli_abort(c(
        "Can not bind spatially explicit edges to spatially implicit edges.",
        "i" = "Use {.fn sfnetworks::to_spatial_explicit} to explicitize edges."
      ))
    }
    return (bind_edges(.data, ..., node_key = node_key))
  }
  # Bind geometries.
  net_geom = list(pull_edge_geom(.data))
  add_geom = lapply(list(...), st_geometry)
  new_geom = do.call("c", c(net_geom, add_geom))
  # Validate if binded edges are lines.
  if (! are_linestrings(new_geom)) {
    cli_abort("Not all edges have geometry type {.cls LINESTRING}")
  }
  # Bind other data.
  net = drop_edge_geom(.data)
  add = lapply(list(...), st_drop_geometry)
  new_net = bind_edges(net, add, node_key = node_key)
  # Add geometries back to the network.
  new_net = mutate_edge_geom(new_net, new_geom)
  # Validate if binded edges meet the valid spatial network structure.
  if (! force) {
    if (is_directed(x)) {
      # Start point should equal start node.
      # End point should equal end node.
      if (! all(nodes_equal_edge_boundaries(x))) {
        cli_abort("Node locations do not match edge boundaries")
      }
    } else {
      # Start point should equal either start or end node.
      # End point should equal either start or end node.
      if (! all(nodes_in_edge_boundaries(x))) {
        cli_abort("Node locations do not match edge boundaries")
      }
    }
  }
  new_net
}