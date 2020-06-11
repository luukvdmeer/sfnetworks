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
#' @importFrom sf st_as_sf st_crs
#' @noRd
create_edges_from_nodes = function(nodes) {
  # Create separate tables for source and target nodes.
  sources = nodes[1:(nrow(nodes)-1), ]
  targets = nodes[2:nrow(nodes), ]
  # Create linestrings between the source and target nodes.
  edges = sf::st_as_sf(
    data.frame(
      from = 1:(nrow(nodes)-1),
      to = 2:nrow(nodes),
      geometry = draw_lines(sources, targets)
    ),
    crs = sf::st_crs(nodes)
  )
  # Use the same sf column name as in the nodes.
  nodes_geometry_colname = get_geometry_colname(nodes)
  if (nodes_geometry_colname != "geometry") {
    edges = set_geometry_colname(edges, nodes_geometry_colname)
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
#' @importFrom sf st_as_sf st_crs st_geometry
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
  edges_geometry_colname = get_geometry_colname(edges)
  if (edges_geometry_colname != "geometry") {
    nodes = set_geometry_colname(edges, edges_geometry_colname)
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

#' Drop the geometry of a network component
#'
#' @param x An object of class \code{\link{sfnetwork}}
#'
#' @param active Either 'nodes' or 'edges'. If NULL, the active component of x
#' will be used.
#'
#' @return An object of class \code{\link{sfnetwork}} when what = 'edges', and
#' an object of class \code{\link[tidygraph]{tbl_graph}} when what = 'nodes'.
#'
#' @importFrom rlang !! :=
#' @importFrom tidygraph mutate
#' @noRd
drop_geometry = function(x, active = NULL) {
  current_active = active(x)
  if (is.null(active)) {
    active = current_active
  } else {
    if (active != current_active) {
      x = switch(
        active,
        nodes = activate(x, "nodes"),
        edges = activate(x, "edges"),
        stop("Unknown active element: ", active, ". Only nodes and edges supported")
      )
    }
  }
  xnew = tidygraph::mutate(as_tbl_graph(x), !!get_geometry_colname(as_sf(x)) := NULL)
  if (active == "edges") {
    xnew = tbg_to_sfn(xnew)
  }
  if (active(xnew) != current_active) {
    xnew = switch(
      current_active,
      nodes = activate(xnew, "nodes"),
      edges = activate(xnew, "edges")
    )
  }
  xnew
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
#' @importFrom sf st_boundary st_cast st_geometry
#' @noRd
get_boundary_points = function(x) {
  sf::st_cast(sf::st_boundary(sf::st_geometry(x)), "POINT")
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

#' Retrieve the name of the geometry list column in an sf object
#'
#' @param x An object of class \code{\link[sf]{sf}}.
#'
#' @return The name of the geometry list column as a character.
#'
#' @noRd
get_geometry_colname = function(x) {
  attr(x, "sf_column")
}

#' Check if an sfnetwork has spatially explicit edges
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return \code{TRUE} if the network has spatially explicit edges, \code{FALSE}
#' otherwise.
#'
#' @noRd
has_spatially_explicit_edges = function(x) {
  is.sf(get_edges(x))
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

#' Check if a graph is directed.
#'
#' @param x An object of \code{\link{sfnetwork}} or \code{tbl_graph}.
#'
#' @return \code{TRUE} when the given graph is directed, \code{FALSE} otherwise.
#'
#' @importFrom tidygraph graph_is_directed with_graph
#' @noRd
is_directed = function(x) {
  tidygraph::with_graph(x, tidygraph::graph_is_directed())
}

#' Check if a table has spatial information stored in a geometry list column
#'
#' @param x Object to check for spatial explicitness.
#'
#' @return \code{TRUE} if the table has a geometry list column, \code{FALSE}
#' otherwise.
#'
#' @noRd
is_spatially_explicit = function(x) {
  any(sapply(x, function(y) inherits(y, "sfc")), na.rm = TRUE)
}

#' Check if the output of an st_join operation has multiple matches
#'
#' @param x The output of an st_join(a,b) operation where the original row
#' indices of a are stored in a column named \code{.sfnetwork_index}.
#'
#' @return \code{TRUE} when there where multiple matches, \code{FALSE}
#' otherwise.
#'
#' @noRd
multiple_matches = function(x) {
  any(table(x$.sfnetwork_index) > 1)
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

#' Replace a geometry list column with another geometry list column
#'
#' @param x An object of class \code{\link{sfnetwork}} \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @param y An object of class \code{\link[sf]{sfc}}.
#'
#' @return An object of the same class as x, but with a replaced geometry list
#' column
#'
#' @importFrom rlang !! :=
#' @importFrom tidygraph mutate
#' @noRd
replace_geometry = function(x, y) {
  tidygraph::mutate(x, !!get_geometry_colname(as_sf(x)) := y)
}

#' Check if the CRS of two objects are the same
#'
#' @param x An object of class \code{\link{sfnetwork}}, \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @param y An object of class \code{\link{sfnetwork}}, \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @return \code{TRUE} when the CRS of x and y are the same, \code{FALSE}
#' otherwise.
#'
#' @importFrom sf st_crs
#' @noRd
same_crs = function(x, y) {
  st_crs(x) == st_crs(y)
}

#' Check if two sf objects have the same LINESTRING boundary points
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @param y An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @return \code{TRUE} when the boundary points are the same, \code{FALSE}
#' otherwise.
#'
#' @details This is a pairwise check. Each row in x is compared to its
#' corresponding row in y. Hence, x and y should be of the same length.
#'
#' @noRd
same_boundary_points = function(x, y) {
  same_geometries(get_boundary_points(x), get_boundary_points(y))
}

#' Check if two sf objects have the same geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @param y An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return \code{TRUE} when the geometries are the same, \code{FALSE}
#' otherwise.
#'
#' @details This is a pairwise check. Each row in x is compared to its
#' corresponding row in y. Hence, x and y should be of the same length.
#'
#' @importFrom sf st_equals
#' @noRd
same_geometries = function(x, y) {
  all(diag(sf::st_equals(x, y, sparse = FALSE)))
}

#' Rename the geometry list column in an sf object.
#'
#' @param x An object of class \code{\link[sf]{sf}}.
#'
#' @param name New name of the geometry list column as a character.
#'
#' @return The same object as \code{x} but with a renamed geometry list column.
#'
#' @noRd
set_geometry_colname = function(x, name) {
  names(x)[which(names(x) == get_geometry_colname(x))] = name
  attr(x, "sf_column") = name
  x
}

#' Check if any of the edge boundary points is equal to any of its boundary nodes
#'
#' @param nodes Nodes element of an \code{\link{sfnetwork}}.
#'
#' @param edges Edges element of an \code{\link{sfnetwork}}.
#'
#' @importFrom sf st_equals
#' @noRd
nodes_in_edge_boundaries = function(nodes, edges) {
  # Get geometries of all edge boundary points.
  edge_boundary_geoms = get_boundary_points(edges)
  # Get geometries of all edge boundary nodes.
  edge_boundary_nodes = get_boundary_nodes(nodes, edges)
  # Test for each edge :
  # Does one of the boundary points equals at least one of the boundary nodes.
  mat = sf::st_equals(edge_boundary_geoms, edge_boundary_nodes, sparse = FALSE)
  all(
    sapply(
      seq(1, nrow(mat), by = 2), 
      function(x) sum(mat[x:(x + 1), x:(x + 1)]) > 1
    )
  )
}

#' Check if edge boundaries of are equal to their corresponding nodes
#'
#' @param nodes Nodes element of an \code{\link{sfnetwork}}.
#'
#' @param edges Edges element of an \code{\link{sfnetwork}}.
#'
#' @noRd
nodes_match_edge_boundaries = function(nodes, edges) {
  # Get geometries of all edge boundary points.
  edge_boundary_geoms = get_boundary_points(edges)
  # Get geometries of all edge boundary nodes.
  edge_boundary_nodes = get_boundary_nodes(nodes, edges)
  # Test if the boundary geometries are equal to their corresponding nodes.
  same_geometries(edge_boundary_geoms, edge_boundary_nodes)
}

#' Check if the geometries of an sf object are all of a specific type
#'
#' @param x An object of class \code{\link[sf]{sf}}.
#'
#' @param type The geometry type to check for, as a string.
#'
#' @return \code{TRUE} when all geometries are of the given type, \code{FALSE}
#' otherwise.
#'
#' @importFrom sf st_is
#' @noRd
st_is_all = function(x, type) {
  all(sf::st_is(x, type))
}

#' Convert spatially implicit edges to spatially explicit edges
#'
#' @param x An object of class \code{\link{sfnetwork}} with spatially implicit
#' edges.
#'
#' @param sf_column_name Name of the sf geometry column that will be created
#' in the edges table. Defaults to "geometry".
#'
#' @return An object of class \code{\link{sfnetwork}} with spatially explicit
#' edges.
#'
#' @importFrom rlang !! :=
#' @importFrom tidygraph mutate
#' @noRd
explicitize_edges = function(x, sf_column_name = "geometry") {
  if (has_spatially_explicit_edges(x)) {
    x
  } else {
    # Extract the nodes from the network.
    nodes = st_as_sf(x, "nodes")
    # Extract the geom column name from the nodes, to use the same for the edges.
    col = get_geometry_colname(nodes)
    # Get the indices of the boundary nodes of each edge.
    # Returns a matrix with source ids in column 1 and target ids in column 2.
    ids = get_boundary_node_indices(x, out = "both")
    # Draw linestring geometries between the boundary nodes of each edge.
    edge_geoms = draw_lines(nodes[ids[, 1], ], nodes[ids[, 2], ])
    # Add the geometries as a column to the edges.
    xnew = tidygraph::mutate(activate(x, "edges"), !!col := edge_geoms)
    # Reset the active element if needed.
    switch(
      active(x),
      nodes = activate(xnew, "nodes"),
      edges = xnew
    )
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