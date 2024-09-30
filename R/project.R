#' Project a spatial point on the network
#'
#' @param x The spatial features to be blended, either as object of class
#' \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}, with \code{POINT} geometries.
#'
#' @param network An object of class \code{\link{sfnetwork}}.
#'
#' @param on On what component of the network should the points be projected?
#' Setting it to \code{'edges'} (the default) will find the nearest point on
#' the nearest edge to each point in \code{y}. Setting it to \code{'nodes'} 
#' will find the nearest node to each point in \code{y}.
#'
#' @details This function used \code{\link[sf]{st_nearest_feature}} to find
#' the nearest edge or node to each feature in \code{y}. When projecting on
#' edges, it then finds the nearest point on the nearest edge by calling
#' \code{\link[sf]{st_nearest_points}} in a pairwise manner.
#'
#' @note Due to internal rounding of rational numbers, even a point projected
#' on an edge may not be evaluated as actually intersecting that edge when
#' calling \code{\link[sf]{st_intersects}}.
#'
#' @returns The same object as \code{y} but with its geometries replaced by the
#' projected points.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#'
#' # Create a spatial network.
#' n1 = st_point(c(0, 0))
#' n2 = st_point(c(1, 0))
#' n3 = st_point(c(2, 0))
#'
#' e1 = st_sfc(st_linestring(c(n1, n2)), crs = 3857)
#' e2 = st_sfc(st_linestring(c(n2, n3)), crs = 3857)
#'
#' net = as_sfnetwork(c(e1, e2))
#'
#' # Create spatial points to project in.
#' p1 = st_sfc(st_point(c(0.25, 0.1)))
#' p2 = st_sfc(st_point(c(1, 0.2)))
#' p3 = st_sfc(st_point(c(1.75, 0.15)))
#'
#' pts = st_sf(foo = letters[1:3], geometry = c(p1, p2, p3), crs = 3857)
#'
#' # Project points to the edges of the network.
#' p1 = project_on_network(pts, net)
#'
#' plot(net)
#' plot(st_geometry(pts), pch = 20, col = "orange", add = TRUE)
#' plot(st_geometry(p1), pch = 4, col = "orange", add = TRUE)
#'
#' # Project points to the nodes of the network.
#' p2 = project_on_network(pts, net, on = "nodes")
#'
#' plot(net)
#' plot(st_geometry(pts), pch = 20, col = "orange", add = TRUE)
#' plot(st_geometry(p2), pch = 4, col = "orange", add = TRUE)
#'
#' par(oldpar)
#'
#' @export
project_on_network = function(x, network, on = "edges") {
  switch(
    on,
    edges = project_on_edges(x, network),
    nodes = project_on_nodes(x, network),
    raise_unknown_input("on", on, c("edges", "nodes"))
  )
}

#' @importFrom sf st_geometry<- st_nearest_feature st_nearest_points
#' @importFrom sfheaders sfc_cast
project_on_edges = function(x, y) {
  E = pull_edge_geom(y)
  # Find the nearest edge to each feature.
  nearest = st_nearest_feature(x, E)
  # Find the nearest point on the nearest edge to each close feature.
  # For this we can use sf::sf_nearest_points, which returns:
  # --> A straight line between feature and point if they are different.
  # --> A multipoint of feature and point if they are equal.
  # To make it easier for ourselves we cast all outputs to lines.
  # Then, the endpoint of that line is the location we are looking for.
  L = st_nearest_points(x, E[nearest], pairwise = TRUE)
  L = sfc_cast(L, "LINESTRING")
  P = linestring_end_points(L)
  # Replace geometry of y with the projected points.
  st_geometry(x) = P
  x
}

#' @importFrom sf st_geometry<- st_nearest_feature
project_on_nodes = function(x, y) {
  N = pull_node_geom(y)
  # Find the nearest node to each feature.
  nearest = st_nearest_feature(x, N)
  # Replace geometry of y with the nearest nodes.
  st_geometry(x) = N[nearest]
  x
}