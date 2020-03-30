#' Represent any geometry as a point
#'
#' @param x An object of class \code{\link[sf]{sf}}.
#'
#' @details Centroids will be calculated for geometries that are not points.
#'
#' @importFrom sf st_centroid st_geometry
as_points = function(x) {
  if (class(sf::st_geometry(x))[1] == "sfc_POINT") {
    return(x)
  } else {
    sf::st_centroid(x)
  }
}

#' Create edges from nodes
#'
#' @param nodes An object of class \code{\link[sf]{sf}}.
#'
#' @details It is assumed that the given geometries form the nodes. Edges need
#' to be created as linestrings between those nodes. It is assumed that the
#' given nodes are connected sequentially. Centroids will be used as nodes if
#' non-point geometries are given.
#'
#' @return A list with the nodes as an object of class \code{\link[sf]{sf}}
#' and the created edges as an object of class \code{\link[sf]{sf}} with
#' \code{LINESTRING} geometries.
#'
#' @importFrom sf st_as_sf st_crs st_geometry st_sfc
create_edges_from_nodes = function(nodes) {
  # Calculate centroids if geometries are not points.
  node_pts = as_points(nodes)

  # Create separate tables for source and target nodes.
  sources = node_pts[1:(nrow(node_pts)-1), ]
  targets = node_pts[2:nrow(node_pts), ]

  # Create linestrings between the source and target nodes.
  edges = sf::st_as_sf(
    data.frame(
      from = 1:(nrow(node_pts)-1),
      to = 2:nrow(node_pts),
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

#' Check if a table has spatial information stored in a geometry list column
#'
#' @param x Object to check for spatial explicitness
#'
#' @export
is_spatially_explicit = function(x) {
  any(sapply(x, function(y) inherits(y, "sfc")), na.rm = TRUE)
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

#' Check if the CRS of two sf objects are the same
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @param y An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return \code{TRUE} when the CRS of x and y are the same, \code{FALSE}
#' otherwise.
#'
#' @importFrom sf st_crs
same_crs = function(x, y) {
  sf::st_crs(x) == sf::st_crs(y)
}
