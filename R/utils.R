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
  nodes_geom_colname = attr(nodes, "sf_column")
  if (nodes_geom_colname != "geometry") {
    names(edges)[3] = nodes_geom_colname
    attr(edges, "sf_column") = nodes_geom_colname
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
  nodes = linestring_boundary_points(edges)
  # Give each unique location a unique ID.
  indices = match(nodes, unique(nodes))
  # Define for each endpoint if it is a source or target node.
  sources = rep(c(TRUE, FALSE), length(nodes) / 2)
  # Define for each edges which node is its source and target node.
  if ("from" %in% colnames(edges)) {
    warning("Overwriting column 'from'", call. = FALSE)
  }
  edges$from = indices[sources]
  if ("to" %in% colnames(edges)) {
    warning("Overwriting column 'to'", call. = FALSE)
  }
  edges$to = indices[!sources]
  # Remove duplicated nodes from the nodes table.
  nodes = nodes[!duplicated(indices)]
  # Convert to sf object
  nodes = sf::st_sf(geometry = nodes)
  # Use the same sf column name as in the edges.
  edges_geom_colname = attr(edges, "sf_column")
  if (edges_geom_colname != "geometry") {
    names(nodes)[1] = edges_geom_colname
    attr(nodes, "sf_column") = edges_geom_colname
  }
  # Return a list with both the nodes and edges.
  class(nodes) = class(edges)
  list(nodes = nodes, edges = edges)
}

#' Draw lines between two sets of points, row-wise
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} 
#' with \code{POINT} geometries, representing the points where lines need to
#' start from.
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} 
#' with \code{POINT} geometries, representing the points where lines need to
#' end at.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{LINESTRING}
#' geometries.
#'
#' @details Lines are drawn row-wise. That is, between the first point in x
#' and the first point in y, the second point in x and the second point in y, 
#' et cetera.
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

#' Get the geometries of the boundary nodes of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, of length equal to twice the number of edges in x, and ordered 
#' as [start of edge 1, end of edge 1, start of edge 2, end of edge 2, ...].
#'
#' @details Boundary nodes differ from boundary points in the sense that
#' boundary points are retrieved by taking the boundary points of the
#' \code{LINESTRING} geometries of edges, while boundary nodes are retrieved
#' by querying the nodes table of a network with the `to` and `from` columns
#' in the edges table. In a valid network structure, boundary nodes should be
#' equal to boundary points.
#'
#' @importFrom igraph E ends
#' @importFrom sf st_geometry
#' @noRd
edge_boundary_nodes = function(x) {
  nodes = st_as_sf(x, "nodes")
  id_mat = igraph::ends(x, igraph::E(x), names = FALSE)
  id_vct = do.call("c", lapply(seq_len(nrow(id_mat)), function(i) id_mat[i, ]))
  sf::st_geometry(nodes[id_vct, ])
}

#' Get the indices of the boundary nodes of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return A two-column matrix, with the number of rows equal to the number
#' of edges in the network. The first column contains the indices of the source
#' nodes of the edges, the seconds column contains the indices of the target
#' nodes of the edges.
#'
#' @importFrom igraph E ends
#' @noRd
edge_boundary_node_indices = function(x) {
  igraph::ends(x, igraph::E(x), names = FALSE)
}

#' Get the geometries of the boundary points of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, of length equal to twice the number of edges in x, and ordered 
#' as [start of edge 1, end of edge 1, start of edge 2, end of edge 2, ...].
#'
#' @details Boundary points differ from boundary nodes in the sense that
#' boundary points are retrieved by taking the boundary points of the
#' \code{LINESTRING} geometries of edges, while boundary nodes are retrieved
#' by querying the nodes table of a network with the `to` and `from` columns
#' in the edges table. In a valid network structure, boundary nodes should be
#' equal to boundary points.
#'
#' @noRd
edge_boundary_points = function(x) {
  edges = st_as_sf(x, "edges")
  linestring_boundary_points(edges)
}

#' Create an empty point geometry
#'
#' @param crs The CRS to assign to the empty point, as an object of class
#' \code{crs}. Defaults to \code{NA}, meaning that no CRS will be assigned.
#'
#' @return An object of class \code{\link[sf]{sfc}} containing a single
#' feature with an empty \code{POINT} geometry.
#'
#' @importFrom sf st_point st_sfc
#' @noRd
empty_point = function(crs = NA) {
  sf::st_sfc(sf::st_point(), crs = crs)
}

#' Make edges spatially explicit
#'
#' @param x An object of class \code{\link{sfnetwork}}.
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
    ids = edge_boundary_node_indices(x)
    # Get the boundary node geometries of each edge.
    from_nodes = nodes[ids[, 1], ]
    to_nodes = nodes[ids[, 2], ]
    # Draw linestring geometries between the boundary nodes of each edge.
    edge_geoms = draw_lines(from_nodes, to_nodes)
    # Use the same geometry column name as the geometry column of the nodes.
    col = attr(nodes, "sf_column")
    # Add the geometries as a column.
    x_new = tidygraph::mutate(activate(x, "edges"), !!col := edge_geoms)
    # Set the sf attributes.
    edge_geom_colname(x_new) = col
    edge_agr(x_new) = empty_edge_agr(x)
    # Return x_new.
    x_new %preserve_active% x
  }
}

#' Extend a line in the same direction by a given distance
#'
#' @param x The line to extend, as object of class \code{\link[sf]{sf}} or 
#' \code{\link[sf]{sfc}}, containing a single feature with \code{LINESTRING} 
#' geometry.
#'
#' @param d The distance to extend the line by, in the same units as the CRS 
#' of x.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{LINESTRING} 
#' geometry.
#'
#' @importFrom sf st_cast st_coordinates st_crs st_point st_sfc st_union
#' @noRd
extend_line = function(x, d) {
  x_xy = sf::st_coordinates(x)
  x_end_x = x_xy[nrow(x_xy), 1]
  x_end_y = x_xy[nrow(x_xy), 2]
  x_ext = sf::st_sfc(
    sf::st_point(c(x_end_x + d, x_end_y + d)), 
    crs = sf::st_crs(x)
  )
  x_pts = sf::st_cast(x, "POINT")
  sf::st_cast(sf::st_union(c(x_pts, x_ext)), "LINESTRING")
}

#' Make edges spatially implicit
#'
#' @param x An object of class \code{\link{sfnetwork}}.

#' @return An object of class \code{\link{sfnetwork}} with spatially implicit
#' edges.
#'
#' @noRd
implicitize_edges = function(x) {
  if (has_spatially_explicit_edges(x)) {
    drop_edge_geom(x)
  } else {
    x
  }
}

#' Get the boundary points of LINESTRING geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} 
#' with \code{LINESTRING} geometries.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, of length equal to twice the number of lines in x, and ordered 
#' as [start of line 1, end of line 1, start of line 2, end of line 2, ...].
#'
#' @details See issue #59 on the GitHub repo for a discussion on this function.
#'
#' @importFrom sf st_cast st_coordinates st_crs st_multipoint st_sfc
#' @noRd
linestring_boundary_points = function(x) {
  # Extract coordinates.
  x_coordinates = unname(sf::st_coordinates(x))
  # Find index of L1 column.
  # This column defines to which linestring each coordinate pair belongs.
  L1_index = ncol(x_coordinates)
  # Find row-indices of the first and last coordinate pair of each linestring.
  # These are the boundary points.
  first_pair = !duplicated(x_coordinates[, L1_index])
  last_pair = !duplicated(x_coordinates[, L1_index], fromLast = TRUE)
  idxs = first_pair | last_pair
  # Extract boundary points and rebuild sfc.
  x_pairs = x_coordinates[idxs, ]
  sf::st_cast(
    sf::st_sfc(sf::st_multipoint(x_pairs[, -L1_index]), crs = sf::st_crs(x)),
   "POINT"
  )
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

#' Split lines by other features
#'
#' @param x The lines to be splitted, as object of class \code{\link[sf]{sf}} 
#' or \code{\link[sf]{sfc}} with \code{LINESTRING} geometries.
#'
#' @param y The features to split with, as object of class \code{\link[sf]{sf}} 
#' or \code{\link[sf]{sfc}}.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{LINESTRING} 
#' geometries.
#'
#' @details Features in x will only be splitted if they intersect with a 
#' feature in y.
#'
#' @importFrom lwgeom st_split
#' @importFrom sf st_collection_extract
#' @noRd
split_lines = function(x, y) {
  sf::st_collection_extract(lwgeom::st_split(x, y), "LINESTRING")
}