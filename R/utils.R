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
#' @importFrom sf st_as_sf st_crs st_geometry st_sfc
create_edges_from_nodes = function(nodes) {
  # Create separate tables for source and target nodes.
  sources = nodes[1:(nrow(nodes)-1), ]
  targets = nodes[2:nrow(nodes), ]

  # Create linestrings between the source and target nodes.
  edges = sf::st_as_sf(
    data.frame(
      from = 1:(nrow(nodes)-1),
      to = 2:nrow(nodes),
      geometry = sf::st_sfc(
        mapply(
          function (a,b) points_to_lines(a,b),
          sf::st_geometry(sources),
          sf::st_geometry(targets),
          SIMPLIFY=FALSE
        )
      )
    ),
    crs = sf::st_crs(nodes)
  )

  class(edges) = class(nodes)
  list(nodes = nodes, edges = edges)
}

#' Create nodes from edges
#'
#' @param edges An object of class \code{\link[sf]{sf}} with \code{LINESTRING}
#' geometries.
#'
#' @details It is assumed that the given LINESTRING geometries form the edges.
#' Nodes need to be created at the endpoints of the edges. Identical endpoints
#' need to be the same node.
#'
#' @return A list with the edges as an object of class \code{\link[sf]{sf}}
#' with \code{LINESTRING} geometries and the created nodes as an object of
#' class \code{\link[sf]{sf}} with \code{POINT} geometries.
#'
#' @importFrom sf st_as_sf st_crs
create_nodes_from_edges = function(edges) {
  # Get the endpoints of the edges.
  nodes = get_endpoints(edges)

  # Give each unique coordinate combination a unique ID.
  nodes$XY = paste(nodes$X, nodes$Y)
  nodes$ID = match(nodes$XY, unique(nodes$XY))

  # Define for each endpoint if it is a source or target node.
  nodes$source = rep(c(TRUE, FALSE), nrow(nodes) / 2)

  # Define for each edges which node is its source and target node.
  if ("from" %in% colnames(edges)) {
    warning("Overwriting column 'from'")
  }
  edges$from = nodes[nodes$source, "ID"]

  if ("to" %in% colnames(edges)) {
    warning("Overwriting column 'to'")
  }
  edges$to = nodes[!nodes$source, "ID"]

  # Remove duplicated nodes from the nodes table.
  nodes = nodes[!duplicated(nodes$ID), ]

  # Convert nodes to sf and keep only necessary columns.
  nodes = sf::st_as_sf(
    nodes[, c("X", "Y")],
    coords = c("X", "Y"),
    crs = sf::st_crs(edges)
  )

  class(nodes) = class(edges)
  list(nodes = nodes, edges = edges)
}

#' Drop the geometry of a network element
#'
#' @param x An object of class \code{\link{sfnetwork}}
#'
#' @param what Either 'nodes' or 'edges'.
#'
#' @return An object of class \code{\link{sfnetwork}} when what = 'edges', and
#' an object of class \code{\link[tidygraph]{tbl_graph}} when what = 'nodes'.
#'
#' @importFrom rlang !! :=
#' @importFrom tidygraph mutate
drop_geometry = function(x, what) {
  x = tidygraph::mutate(as_tbl_graph(x), !!get_geometry_colname(as_sf(x)) := NULL)
  if (what == "edges") {
    x = as_sfnetwork(x)
  }
  x
}

#' Get the X or Y coordinates of geometries
#'
#' @param x An object of class \code{\link[sf]{sf}}.
#'
#' @param XY Which coordinates do you want to get? Either "X" or "Y".
#'
#' @importFrom sf st_coordinates
get_coords = function(x, XY) {
  coords = sf::st_coordinates(x)
  split(coords[, XY], f = as.factor(coords[, "L1"]))
}

#' Point to the edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link{sfnetwork}} with activated edges.
get_edges = function(x) {
  activate(x, "edges")
}

#' Get the X or Y coordinates of the endpoints of LINESTRING geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} with \code{LINESTRING}
#' geometries.
#'
#' @param XY Which coordinates do you want to get? Either "X" or "Y".
get_endpoint_coords = function(x, XY) {
  all_coords = get_coords(x, XY)
  endpoint_coords = do.call(
    "rbind",
    lapply(
      all_coords,
      function(x) data.frame(c(x[1], x[length(x)]))
    )
  )
  colnames(endpoint_coords) = XY
  endpoint_coords
}

#' Get the endpoints of LINESTRING geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} with \code{LINESTRING}
#' geometries.
#'
#' @return An object of class \code{data.frame} with an "X" and "Y" column,
#' containing respectively the X and Y coordinates of the endpoints.
get_endpoints = function(x) {
  cbind(get_endpoint_coords(x, "X"), get_endpoint_coords(x, "Y"))
}

#' Retrieve the name of the geometry list column in an sf object
#'
#' @param x An object of class \code{\link[sf]{sf}}.
#'
#' @return The name of the geometry list column as a string.
get_geometry_colname = function(x) {
  attr(x, "sf_column")
}

#' Check if an sfnetwork has spatially explicit edges
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return \code{TRUE} if the network has spatially explicit edges, \code{FALSE}
#' otherwise.
has_spatially_explicit_edges = function(x) {
  is.sf(as_tibble(x, "edges"))
}

#' Point to the nodes in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link{sfnetwork}} with activated nodes.
get_nodes = function(x) {
  activate(x, "nodes")
}

#' Check if a table has spatial information stored in a geometry list column
#'
#' @param x Object to check for spatial explicitness
#'
#' @return \code{TRUE} if the table has a geometry list column, \code{FALSE}
#' otherwise.
is_spatially_explicit = function(x) {
  any(sapply(x, function(y) inherits(y, "sfc")), na.rm = TRUE)
}

#' Check if the output of an st_join operation has multiple matches
#'
#' @param x The output of an st_join(a,b) operation where the original row
#' indices of a are stored in a column names \code{.sfnetwork_index}.
#'
#' @return \code{TRUE} when there where multiple matches, \code{FALSE}
#' otherwise.
multiple_matches = function(x) {
  any(table(x$.sfnetwork_index) > 1)
}

#' Draw lines between points
#'
#' @param sources An object of class \code{\link[sf]{sf}} with \code{POINT}
#' geometries, representing the points where lines need to start from.
#'
#' @param targets An object of class \code{\link[sf]{sf}} with \code{POINT}
#' geometries, representing the points where lines need to end at.
#'
#' @return An object of class \code{\link[sf]{sf}} with \code{LINESTRING}
#' geometries.
#'
#' @importFrom sf st_cast st_union
points_to_lines = function(sources, targets) {
  sf::st_cast(sf::st_union(sources, targets), "LINESTRING")
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
same_crs = function(x, y) {
  st_crs(x) == st_crs(y)
}

#' Check if two sf objects have the same LINESTRING endpoints
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @param y An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @return \code{TRUE} when the endpoints are the same, \code{FALSE} otherwise.
#'
#' @details This is a pairwise check. Each row in x is compared to its
#' corresponding row in y. Hence, x and y should be of the same length.
same_endpoints = function(x, y) {
  identical(get_endpoints(x), get_endpoints(y))
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
st_is_all = function(x, type) {
  all(sf::st_is(x, type))
}
