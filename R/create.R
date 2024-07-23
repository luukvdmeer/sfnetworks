#' Create a spatial network from linestring geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @details It is assumed that the given lines geometries form the edges in the
#' network. Nodes are created at the boundary points of the edges. Boundary
#' points at equal locations become the same node.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' as_sfnetwork(roxel)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#'
#' plot(st_geometry(roxel))
#' plot(as_sfnetwork(roxel))
#'
#' par(oldpar)
#'
#' @importFrom sf st_as_sf st_sf
#' @export
create_from_spatial_lines = function(x, directed = TRUE) {
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
  sfnetwork_(nodes, edges, directed = directed)
}

#' Create a spatial network from point geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{POINT} geometries.
#'
#' @param connections How to connect the given point geometries to each other?
#' Can be specified either as an adjacency matrix, or as a character
#' describing a specific method to define the connections.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param edges_as_lines Should the created edges be spatially explicit, i.e.
#' have \code{LINESTRING} geometries stored in a geometry list column? Defaults
#' to \code{TRUE}.
#'
#' @param k The amount of neighbors to connect to if
#' \code{connections = 'knn'}. Defaults to \code{1}, meaning that nodes are
#' only connected to their nearest neighbor. Ignored for any other value of the
#' \code{connected} argument.
#'
#' @details It is assumed that the given points form the nodes in the network.
#' How those nodes are connected by edges depends on the \code{connections}
#' argument.
#'
#' The connections can be specified through an adjacency matrix A, which is an
#' n x n matrix with n being the number of nodes, and element Aij holding a
#' \code{TRUE} value if there is an edge from node i to node j, and a
#' \code{FALSE} value otherwise. In the case of undirected networks, the matrix
#' is not tested for symmetry, and an edge will exist between node i and node j
#' if either element Aij or element Aji is \code{TRUE}.
#'
#' Alternatively, the connections can be specified by providing the name of a
#' specific method that will create the adjacency matrix internally. Valid
#' options are:
#'
#' \itemize{
#'   \item \code{complete}: All nodes are directly connected to each other.
#'   \item \code{sequence}: The nodes are sequentially connected to each other,
#'   meaning that the first node is connected to the second node, the second
#'   node is connected to the third node, et cetera.
#'   \item \code{mst}: The nodes are connected by their spatial
#'   \href{https://en.wikipedia.org/wiki/Minimum_spanning_tree}{minimum
#'   spanning tree}, i.e. the set of edges with the minimum total edge length
#'   required to connect all nodes. Can also be specified as
#'   \code{minimum_spanning_tree}.
#'   \item \code{delaunay}: The nodes are connected by their
#'   \href{https://en.wikipedia.org/wiki/Delaunay_triangulation}{Delaunay
#'   triangulation}.
#'   Requires the \href{https://r-spatial.github.io/spdep/index.html}{spdep}
#'   package to be installed, and assumes planar coordinates.
#'   \item \code{gabriel}: The nodes are connected as a
#'   \href{https://en.wikipedia.org/wiki/Gabriel_graph}{Gabriel graph}.
#'   Requires the \href{https://r-spatial.github.io/spdep/index.html}{spdep}
#'   package to be installed, and assumes planar coordinates.
#'   \item \code{rn}: The nodes are connected as a
#'   \href{https://en.wikipedia.org/wiki/Relative_neighborhood_graph}{relative
#'   neighborhood graph}. Can also be specified as \code{relative_neighborhood}
#'   or \code{relative_neighbourhood}.
#'   Requires the \href{https://r-spatial.github.io/spdep/index.html}{spdep}
#'   package to be installed, and assumes planar coordinates.
#'   \item \code{knn}: Each node is connected to its k nearest neighbors, with
#'   \code{k} being specified through the \code{k} argument. By default,
#'   \code{k = 1}, meaning that the nodes are connected as a
#'   \href{https://en.wikipedia.org/wiki/Nearest_neighbor_graph}{nearest
#'   neighbor graph}. Can also be specified as \code{nearest_neighbors} or
#'   \code{nearest_neighbours}.
#'   Requires the \href{https://r-spatial.github.io/spdep/index.html}{spdep}
#'   package to be installed.
#' }
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#'
#' pts = roxel[seq(1, 100, by = 10),]) |>
#'   st_geometry() |>
#'   st_centroid() |>
#'   st_transform(3035)
#'
#' # Using an adjacency matrix
#' adj = matrix(c(rep(TRUE, 10), rep(FALSE, 90)), nrow = 10)
#' net = as_sfnetwork(pts, connections = adj)
#'
#' plot(net)
#'
#' # Using a adjacency matrix from a spatial predicate
#' dst = units::set_units(500, "m")
#' adj = st_is_within_distance(pts, dist = dst, sparse = FALSE)
#' net = as_sfnetwork(pts, connections = adj)
#'
#' plot(net)
#'
#' # Using pre-defined methods
#' cnet = as_sfnetwork(pts, connections = "complete")
#' snet = as_sfnetwork(pts, connections = "sequence")
#' mnet = as_sfnetwork(pts, connections = "mst")
#' dnet = as_sfnetwork(pts, connections = "delaunay")
#' gnet = as_sfnetwork(pts, connections = "gabriel")
#' rnet = as_sfnetwork(pts, connections = "rn")
#' nnet = as_sfnetwork(pts, connections = "knn")
#' knet = as_sfnetwork(pts, connections = "knn", k = 2)
#'
#' par(mar = c(1,1,1,1), mfrow = c(4,2))
#'
#' plot(cnet, main = "complete")
#' plot(snet, main = "sequence")
#' plot(mnet, main = "minimum spanning tree")
#' plot(dnet, main = "delaunay triangulation")
#' plot(gnet, main = "gabriel graph")
#' plot(rnet, main = "relative neighborhood graph")
#' plot(nnet, main = "nearest neighbor graph")
#' plot(knet, main = "k nearest neighbor graph (k = 2)")
#'
#' par(oldpar)
#'
#' @export
create_from_spatial_points = function(x, connections = "complete",
                                      directed = TRUE, edges_as_lines = TRUE,
                                      k = 1) {
  if (is_single_string(connections)) {
    switch(
      connections,
      complete = create_spatial_complete(x, directed, edges_as_lines),
      sequence = create_spatial_sequence(x, directed, edges_as_lines),
      mst = create_spatial_mst(x, directed, edges_as_lines),
      delaunay = create_spatial_delaunay(x, directed, edges_as_lines),
      gabriel = create_spatial_gabriel(x, directed, edges_as_lines),
      rn = create_spatial_rn(x, directed, edges_as_lines),
      knn = create_spatial_knn(x, k, directed, edges_as_lines),
      minimum_spanning_tree = create_spatial_mst(x, directed, edges_as_lines),
      relative_neighborhood = create_spatial_rn(x, directed, edges_as_lines),
      relative_neighbourhood = create_spatial_rn(x, directed, edges_as_lines),
      nearest_neighbors = create_spatial_knn(x, k, directed, edges_as_lines),
      nearest_neighbours = create_spatial_knn(x, k, directed, edges_as_lines),
      raise_unknown_input(connections)
    )
  } else {
    create_spatial_custom(x, connections, directed, edges_as_lines)
  }
}

create_spatial_custom = function(x, connections, directed = TRUE,
                                 edges_as_lines = TRUE) {
  nblist = adj2nb(connections)
  nb2net(nblist, x, directed, edges_as_lines)
}

#' @importFrom sf st_geometry
create_spatial_complete = function(x, directed = TRUE, edges_as_lines = TRUE) {
  n_nodes = length(st_geometry(x))
  # Create the adjacency matrix, with everything connected to everything.
  connections = matrix(TRUE, ncol = n_nodes, nrow = n_nodes)
  diag(connections) = FALSE # No loop edges.
  # Create the network from the adjacency matrix.
  create_spatial_custom(x, connections, directed, edges_as_lines)
}

#' @importFrom sf st_geometry
create_spatial_sequence = function(x, directed = TRUE, edges_as_lines = TRUE) {
  n_nodes = length(st_geometry(x))
  # Create the adjacency matrix.
  # Each node is connected to the next node.
  connections = matrix(FALSE, ncol = n_nodes - 1, nrow = n_nodes - 1)
  diag(connections) = TRUE
  connections = cbind(rep(FALSE, nrow(connections)), connections)
  connections = rbind(connections, rep(FALSE, ncol(connections)))
  # Create the network from the adjacency matrix.
  create_spatial_custom(x, connections, directed, edges_as_lines)
}

#' @importFrom igraph mst
#' @importFrom tidygraph with_graph
create_spatial_mst = function(x, directed = TRUE, edges_as_lines = TRUE) {
  complete_net = create_spatial_complete(x, directed, edges_as_lines)
  edge_lengths = with_graph(complete_net, edge_length())
  mst_net = mst(complete_net, weights = edge_lengths)
  tbg_to_sfn(as_tbl_graph(mst_net))
}

#' @importFrom sf st_geometry
#' @importFrom tibble tibble
create_spatial_delaunay = function(x, directed = TRUE, edges_as_lines = TRUE) {
  requireNamespace("spdep") # Package spdep is required for this function.
  nblist = tri2nb(st_geometry(x))
  nb2net(nblist, x, directed, edges_as_lines)
}

#' @importFrom sf st_geometry
#' @importFrom tibble tibble
create_spatial_gabriel = function(x, directed = TRUE, edges_as_lines = TRUE) {
  requireNamespace("spdep") # Package spdep is required for this function.
  nbgraph = spdep::gabrielneigh(st_geometry(x))
  nblist = spdep::graph2nb(nbgraph, sym = TRUE)
  nb2net(nblist, x, directed, edges_as_lines)
}

#' @importFrom sf st_geometry
create_spatial_rn = function(x, directed = TRUE, edges_as_lines = TRUE) {
  requireNamespace("spdep") # Package spdep is required for this function.
  nbgraph = spdep::relativeneigh(st_geometry(x))
  nblist = spdep::graph2nb(nbgraph, sym = TRUE)
  nb2net(nblist, x, directed, edges_as_lines)
}

#' @importFrom sf st_geometry
create_spatial_knn = function(x, k = 1, directed = TRUE, edges_as_lines = TRUE) {
  requireNamespace("spdep") # Package spdep is required for this function.
  nbmat = spdep::knearneigh(st_geometry(x), k = k)
  nblist = spdep::knn2nb(nbmat, sym = FALSE)
  nb2net(nblist, x, directed, edges_as_lines)
}

adj2nb = function(x) {
  apply(x, 1, which, simplify = FALSE)
}

#' @importFrom tibble tibble
nb2net = function(neighbors, nodes, directed = TRUE, edges_as_lines = TRUE) {
  from_ids = rep(c(1:length(neighbors)), lengths(neighbors))
  to_ids = do.call("c", neighbors)
  sfnetwork(
    nodes = nodes,
    edges = tibble(from = from_ids, to = to_ids),
    directed = directed,
    edges_as_lines = edges_as_lines,
    force = TRUE
  )
}
