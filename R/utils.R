#' Preserve the active element of the original graph
#'
#' @param new_graph An object of class \code{\link{sfnetwork}}.
#'
#' @param orig_graph An object of class \code{\link{sfnetwork}}.
#'
#' @noRd
`%preserve_active%` = function(new_graph, orig_graph) {
  switch(
    attr(orig_graph, "active"),
    nodes = activate(new_graph, "nodes"),
    edges = activate(new_graph, "edges")
  )
}

#' Print a string with a subtle style.
#'
#' @param ... A string to print.
#'
#' @return A printed string to console with subtle style.
#'
#' @importFrom crayon silver
#' @noRd
cat_subtle = function(...) {
  cat(crayon::silver(...))
}

#' Create edges from nodes
#'
#' @param nodes An object of class \code{\link[sf]{sf}} with \code{POINT}
#' geometries.
#'
#' @details It is assumed that the given POINT geometries form the nodes. Edges
#' need to be created as linestrings between those nodes. It is assumed that
#' the given nodes are connected sequentially.
#'
#' @return A list with the nodes as an object of class \code{\link[sf]{sf}}
#' with \code{POINT} geometries and the created edges as an object of class
#' \code{\link[sf]{sf}} with \code{LINESTRING} geometries.
#'
#' @importFrom sf st_sf
#' @noRd
create_edges_from_nodes = function(nodes) {
  # Define indices for source and target nodes.
  source_ids = 1:(nrow(nodes)-1)
  target_ids = 2:nrow(nodes)
  # Create separate tables for source and target nodes.
  sources = nodes[source_ids, ]
  targets = nodes[target_ids, ]
  # Create linestrings between the source and target nodes.
  edges = sf::st_sf(
    from = source_ids,
    to = target_ids,
    geometry = draw_lines(sources, targets)
  )
  # Use the same sf column name as in the nodes.
  nodes_sf_colname = attr(nodes, "sf_column")
  if (nodes_sf_colname != "geometry") {
    names(edges)[3] = nodes_sf_colname
    attr(edges, "sf_column") = nodes_sf_colname
  }
  # Return a list with both the nodes and edges.
  class(edges) = class(nodes)
  list(nodes = nodes, edges = edges)
}

#' Create nodes from edges
#'
#' @param edges An object of class \code{\link[sf]{sf}} with \code{LINESTRING}
#' geometries.
#'
#' @details It is assumed that the given LINESTRING geometries form the edges.
#' Nodes need to be created at the boundary points of the edges. Identical
#' boundary points should become the same node.
#'
#' @return A list with the edges as an object of class \code{\link[sf]{sf}}
#' with \code{LINESTRING} geometries and the created nodes as an object of
#' class \code{\link[sf]{sf}} with \code{POINT} geometries.
#'
#' @importFrom sf st_sf
#' @noRd
create_nodes_from_edges = function(edges) {
  # Get the boundary points of the edges.
  nodes = get_boundary_points(edges)
  # Give each unique location a unique ID.
  indices = match(nodes, unique(nodes))
  # Define for each endpoint if it is a source or target node.
  sources = rep(c(TRUE, FALSE), length(nodes) / 2)
  # Define for each edges which node is its source and target node.
  if ("from" %in% colnames(edges)) {
    warning("Overwriting column 'from'")
  }
  edges$from = indices[sources]

  if ("to" %in% colnames(edges)) {
    warning("Overwriting column 'to'")
  }
  edges$to = indices[!sources]
  # Remove duplicated nodes from the nodes table.
  nodes = nodes[!duplicated(indices)]
  # Convert to sf object
  nodes = sf::st_sf(geometry = nodes)
  # Use the same sf column name as in the edges.
  edges_sf_colname = attr(edges, "sf_column")
  if (edges_sf_colname != "geometry") {
    names(nodes)[1] = edges_sf_colname
    attr(nodes, "sf_column") = edges_sf_colname
  }
  # Return a list with both the nodes and edges.
  class(nodes) = class(edges)
  list(nodes = nodes, edges = edges)
}

#' Draw lines between two sf objects, row-wise
#'
#' @param x An object of class \code{\link[sf]{sf}} with \code{POINT}
#' geometries, representing the points where lines need to start from.
#'
#' @param y An object of class \code{\link[sf]{sf}} with \code{POINT}
#' geometries, representing the points where lines need to end at.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{LINESTRING}
#' geometries.
#'
#' @details Lines are drawn row-wise. That is, between point 1 in x and point 1
#' in y, point 2 in x and point 2 in y, et cetera.
#'
#' @importFrom sf st_crs st_geometry st_sfc
#' @noRd
draw_lines = function(x, y) {
  sf::st_sfc(
    mapply(
      function (a,b) points_to_line(a,b),
      sf::st_geometry(x),
      sf::st_geometry(y),
      SIMPLIFY = FALSE
    ),
    crs = sf::st_crs(x)
  )
}

#' Get the geometries of the boundary nodes of given edges.
#'
#' @param nodes Nodes element of an \code{\link{sfnetwork}}.
#'
#' @param edges Edges element of an \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries.
#'
#' @details Boundary nodes differ from boundary points in the sense that
#' boundary points are retrieved by taking the endpoints of the
#' \code{LINESTRING} geometries of edges, while boundary nodes are retrieved
#' by querying the nodes table of a network with the `to` and `from` columns
#' in the edges table. In a valid network structure, boundary nodes should be
#' equal to boundary points.
#'
#' @importFrom sf st_geometry
#' @noRd
get_boundary_nodes = function(nodes, edges) {
  # Get the node indices of all startpoints of the edges.
  start_ids = edges$from
  # Get the node indices of all endpoints of the edges.
  end_ids = edges$to
  # Order those as [start_edge1, end_edge1, start_edge2, end_edge2, etc].
  all_ids = do.call(
    "c",
    mapply(
      function(x,y) c(x, y),
      start_ids,
      end_ids,
      SIMPLIFY = FALSE
    )
  )
  # Select the nodes corresponding to the edge boundary geometries.
  sf::st_geometry(nodes[all_ids, ])
}

#' Get the indices of the boundary nodes of all edges in the network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param out Which node indices to return. Either 'sources' to return only the
#' indices of source nodes, 'targets' to return only the indices of target
#' nodes, or 'both' to return both of them.
#'
#' @return A vector of indices when out is 'sources' or 'targets'. A two-column
#' matrix when out is 'both', where the source node indices are in the first
#' column, and the target node indices are in the second column.
#'
#' @importFrom igraph E ends
#' @noRd
get_boundary_node_indices = function(x, out = "both") {
  ids = igraph::ends(x, igraph::E(x))
  switch(
    out,
    both = ids,
    sources = ids[, 1],
    targets = ids[, 2],
    stop("Unknown output", out)
  )
}

#' Get the boundary points of LINESTRING geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} with \code{LINESTRING}
#' geometries.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries.
#'
#' See #59 for a discussion on this function.
#'
#' @importFrom sf st_coordinates st_cast st_sfc
#' @noRd
get_boundary_points = function(x) {
  # 1a. extract coordinates
  x_coordinates <- sf::st_coordinates(x)

  # 1b. Find index of L1 column
  L1_index <- ncol(x_coordinates)

  # 1c. Remove colnames
  x_coordinates <- unname(x_coordinates)

  # 2. Find idxs of first and last coordinate (i.e. the boundary points)
  first_pair <- !duplicated(x_coordinates[, L1_index])
  last_pair <- !duplicated(x_coordinates[, L1_index], fromLast = TRUE)
  idxs <- first_pair | last_pair

  # 3. Extract idxs and rebuild sfc
  x_pairs <- x_coordinates[idxs, ]
  x_nodes <- sf::st_cast(
    sf::st_sfc(
      sf::st_multipoint(x_pairs[, -L1_index]),
      crs = sf::st_crs(x)
    ),
   "POINT"
  )
  x_nodes
}

#' Directly extract the edges from an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link[sf]{sf}} when x has spatially
#' explicit edges, and object of class \code{\link[tibble]{tbl_df}} otherwise.
#'
#' @noRd
get_edges = function(x) {
  as_tibble(x, "edges")
}

#' Directly extract the nodes of an sfnetwork.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link[sf]{sf}}.
#'
#' @noRd
get_nodes = function(x) {
  as_sf(x, "nodes")
}

#' Draw a line between two points
#'
#' @param x A \code{POINT} geometry.
#'
#' @param y A \code{POINT} geometry.
#'
#' @return A \code{LINESTRING} geometry.
#'
#' @importFrom sf st_cast st_union
#' @noRd
points_to_line = function(x, y) {
  sf::st_cast(sf::st_union(x, y), "LINESTRING")
}

#' Convert spatially implicit edges to spatially explicit edges
#'
#' @param x An object of class \code{\link{sfnetwork}} with spatially implicit
#' edges.
#'
#' @return An object of class \code{\link{sfnetwork}} with spatially explicit
#' edges.
#'
#' @importFrom igraph edge_attr_names
#' @importFrom rlang !! :=
#' @importFrom sf NA_agr_
#' @importFrom tidygraph mutate
#' @noRd
explicitize_edges = function(x) {
  if (has_spatially_explicit_edges(x)) {
    x
  } else {
    # Extract the nodes from the network.
    nodes = st_as_sf(x, "nodes")
    # Get the indices of the boundary nodes of each edge.
    # Returns a matrix with source ids in column 1 and target ids in column 2.
    ids = get_boundary_node_indices(x, out = "both")
    # Get the boundary node geometries of each edge.
    from_nodes = nodes[ids[, 1], ]
    to_nodes = nodes[ids[, 2], ]
    # Draw linestring geometries between the boundary nodes of each edge.
    edge_geoms = draw_lines(from_nodes, to_nodes)
    # Use the same geometry column name as the geometry column of the nodes.
    col = attr(nodes, "sf_column")
    # Set the sf attributes.
    sf_attr(x, "sf_column", "edges") = col
    sf_attr(x, "agr", "edges") = empty_agr(x, "edges")
    # Add the geometries as a column.
    x_new = tidygraph::mutate(activate(x, "edges"), !!col := edge_geoms)
    # Return x_new.
    x_new %preserve_active% x
  }
}

#' Convert spatially explicit edges to spatially implicit edges
#'
#' @param x An object of class \code{\link{sfnetwork}} with spatially explicit
#' edges.

#' @return An object of class \code{\link{sfnetwork}} with spatially implicit
#' edges.
#'
#' @noRd
implicitize_edges = function(x) {
  if (has_spatially_explicit_edges(x)) {
    drop_geometry(x, "edges")
  } else {
    x
  }
}
