#' Create a spatial network from linestring geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \\code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @details It is assumed that the given lines geometries form the edges in the
#' network. Nodes are created at the boundary points of the edges. Boundary
#' points at equal locations become the same node.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @importFrom sf st_as_sf st_sf
#' @export
create_from_spatial_lines = function(x, ...) {
  # The provided lines will form the edges of the network.
  edges = st_as_sf(x)
  # Get the boundary points of the edges.
  nodes = linestring_boundary_points(edges)
  # Give each unique location a unique ID.
  indices = st_match(nodes)
  # Define for each endpoint if it is a source or target node.
  is_source = rep(c(TRUE, FALSE), length(nodes) / 2)
  # Define for each edge which node is its source and target node.
  if ("from" %in% colnames(edges)) raise_overwrite("from")
  edges$from = indices[is_source]
  if ("to" %in% colnames(edges)) raise_overwrite("to")
  edges$to = indices[!is_source]
  # Remove duplicated nodes from the nodes table.
  nodes = nodes[!duplicated(indices)]
  # Convert to sf object
  nodes = st_sf(geometry = nodes)
  # Use the same sf column name in the nodes as in the edges.
  geom_colname = attr(edges, "sf_column")
  if (geom_colname != "geometry") {
    names(nodes)[1] = geom_colname
    attr(nodes, "sf_column") = geom_colname
  }
  # Use the same class for the nodes as for the edges.
  # This mainly affects the "lower level" classes.
  # For example an sf tibble instead of a sf data frame.
  class(nodes) = class(edges)
  # Create a network out of the created nodes and the provided edges.
  # The ... arguments are forwarded to the sfnetwork construction function.
  # Force to skip network validity tests because we already know they pass.
  sfnetwork(nodes, edges, force = TRUE, ...)
}

#' Create a spatial network from point geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \\code{\link[sf]{sfc}}
#' with \code{POINT} geometries.
#'
#' @param method The method used to connect the given point geometries to each
#' other.
#'
#' @details It is assumed that the given points form the nodes in the network.
#' How those nodes are connected by edges depends on the selected method.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @export
create_from_spatial_points = function(x, method = "sequence", ...) {
  switch(
    method,
    sequence = create_spatial_sequence(x, ...),
    raise_unknown_input(method)
  )
}

#' Create as spatial network as sequentially connected nodes
#'
#' @param x An object of class \code{\link[sf]{sf}} or \\code{\link[sf]{sfc}}
#' with \code{POINT} geometries.
#'
#' @details It is assumed that the given points form the nodes in the network.
#' The nodes are sequentially connected by edges.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @importFrom sf st_as_sf st_geometry st_sf
#' @noRd
create_spatial_sequence = function(x, ...) {
  # The provided points will form the nodes of the network.
  nodes = st_as_sf(x)
  # Define indices for source and target nodes.
  source_ids = 1:(nrow(nodes) - 1)
  target_ids = 2:nrow(nodes)
  # Create separate tables for source and target nodes.
  sources = nodes[source_ids, ]
  targets = nodes[target_ids, ]
  # Create linestrings between the source and target nodes.
  edges = st_sf(
    from = source_ids,
    to = target_ids,
    geometry = draw_lines(st_geometry(sources), st_geometry(targets))
  )
  # Use the same sf column name in the edges as in the nodes.
  geom_colname = attr(nodes, "sf_column")
  if (geom_colname != "geometry") {
    names(edges)[3] = geom_colname
    attr(edges, "sf_column") = geom_colname
  }
  # Use the same class for the edges as for the nodes.
  # This mainly affects the "lower level" classes.
  # For example an sf tibble instead of a sf data frame.
  class(nodes) = class(edges)
  # Create a network out of the created nodes and the provided edges.
  # The ... arguments are forwarded to the sfnetwork construction function.
  # Force to skip network validity tests because we already know they pass.
  sfnetwork(nodes, edges, force = TRUE, ...)
}