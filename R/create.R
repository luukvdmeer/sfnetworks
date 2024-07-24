#' Create a spatial network from linestring geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param compute_length Should the geographic length of the edges be stored in
#' a column named \code{length}? Uses \code{\link[sf]{st_length}} to compute
#' the length of the edge geometries. If there is already a column named
#' \code{length}, it will be overwritten. Please note that the values in this
#' column are \strong{not} automatically recognized as edge weights. This needs
#' to be specified explicitly when calling a function that uses edge weights.
#' Defaults to \code{FALSE}.
#'
#' @details It is assumed that the given linestring geometries form the edges
#' in the network. Nodes are created at the line boundaries. Shared boundaries
#' between multiple linestrings become the same node.
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
create_from_spatial_lines = function(x, directed = TRUE,
                                     compute_length = FALSE) {
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
  # Force to skip network validity tests because we already know they pass.
  sfnetwork(nodes, edges,
    directed = directed,
    edges_as_lines = TRUE,
    compute_length = compute_length,
    force = TRUE
  )
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
#' @param compute_length Should the geographic length of the edges be stored in
#' a column named \code{length}? Uses \code{\link[sf]{st_length}} to compute
#' the length of the edge geometries when edges are spatially explicit, and
#' \code{\link[sf]{st_distance}} to compute the distance between boundary nodes
#' when edges are spatially implicit. If there is already a column named
#' \code{length}, it will be overwritten. Please note that the values in this
#' column are \strong{not} automatically recognized as edge weights. This needs
#' to be specified explicitly when calling a function that uses edge weights.
#' Defaults to \code{FALSE}.
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
#' if either element Aij or element Aji is \code{TRUE}. Non-logical matrices
#' are first converted into logical matrices using \code{\link{as.logical}}.
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
#'   required to connect all nodes. The tree is always constructed on an
#'   undirected network, regardless of the value of the \code{directed}.
#'   argument. If \code{directed = TRUE}, each edge is duplicated and reversed
#'   to ensure full connectivity of the network. Can also be specified as
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
#' pts = roxel[seq(1, 100, by = 10),] |>
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
                                      compute_length = FALSE, k = 1) {
  if (is_single_string(connections)) {
    nblist = switch(
      connections,
      complete = complete_neighbors(x),
      sequence = sequential_neighbors(x),
      mst = mst_neighbors(x),
      delaunay = delaunay_neighbors(x),
      gabriel = gabriel_neighbors(x),
      rn = relative_neighbors(x),
      knn = nearest_neighbors(x, k),
      minimum_spanning_tree = mst_neighbors(x),
      relative_neighborhood = relative_neighbors(x),
      relative_neighbourhood = relative_neighbors(x),
      nearest_neighbors = nearest_neighbors(x, k),
      nearest_neighbours = nearest_neighbors(x, k),
      raise_unknown_input(connections)
    )
  } else {
    nblist = custom_neighbors(x, connections)
  }
  nb2net(nblist, x, directed, edges_as_lines, compute_length)
}

custom_neighbors = function(x, connections) {
  adj2nb(connections)
}

#' @importFrom sf st_geometry
complete_neighbors = function(x) {
  n_nodes = length(st_geometry(x))
  # Create the adjacency matrix, with everything connected to everything.
  connections = matrix(TRUE, ncol = n_nodes, nrow = n_nodes)
  diag(connections) = FALSE # No loop edges.
  # Return as neighbor list.
  adj2nb(connections)
}

#' @importFrom sf st_geometry
sequential_neighbors = function(x) {
  # Each node in x is connected to the next node in x.
  n_nodes = length(st_geometry(x))
  lapply(c(1:(n_nodes - 1)), \(x) x + 1)
}

#' @importFrom igraph as_edgelist graph_from_adjacency_matrix igraph_opt
#' igraph_options mst as_adj_list
#' @importFrom sf st_distance st_geometry
mst_neighbors = function(x, directed = TRUE, edges_as_lines = TRUE) {
  # Change default igraph options.
  # This prevents igraph returns node or edge indices as formatted sequences.
  # We only need the "raw" integer indices.
  # Changing this option improves performance especially on large networks.
  default_igraph_opt = igraph_opt("return.vs.es")
  igraph_options(return.vs.es = FALSE)
  on.exit(igraph_options(return.vs.es = default_igraph_opt))
  # Create a complete graph.
  n_nodes = length(st_geometry(x))
  connections = upper.tri(matrix(FALSE, ncol = n_nodes, nrow = n_nodes))
  net = graph_from_adjacency_matrix(connections, mode = "undirected")
  # Compute distances between adjacent nodes for each edge in that graph.
  dists = st_distance(x)[as_edgelist(net, names = FALSE)]
  # Compute minimum spanning tree of the weighted complete graph.
  mst = mst(net, weights = dists)
  # Return as a neighbor list.
  as_adj_list(mst)
}

#' @importFrom sf st_geometry
delaunay_neighbors = function(x) {
  requireNamespace("spdep") # Package spdep is required for this function.
  tri2nb(st_geometry(x))
}

#' @importFrom sf st_geometry
gabriel_neighbors = function(x) {
  requireNamespace("spdep") # Package spdep is required for this function.
  spdep::graph2nb(spdep::gabrielneigh(st_geometry(x)), sym = TRUE)
}

#' @importFrom sf st_geometry
relative_neighbors = function(x) {
  requireNamespace("spdep") # Package spdep is required for this function.
  spdep::graph2nb(spdep::relativeneigh(st_geometry(x)), sym = TRUE)
}

#' @importFrom sf st_geometry
nearest_neighbors = function(x, k = 1) {
  requireNamespace("spdep") # Package spdep is required for this function.
  spdep::knn2nb(spdep::knearneigh(st_geometry(x), k = k), sym = FALSE)
}

#' Convert an adjacency matrix into a neighbor list
#'
#' Adjacency matrices of networks are n x n matrices with n being the number of
#' nodes, and element Aij holding a \code{TRUE} value if node i is adjacent to
#' node j, and a \code{FALSE} value otherwise. Neighbor lists are the sparse
#' version of these matrices, coming in the form of a list with one element per
#' node, holding the indices of the nodes it is adjacent to.
#'
#' @param x An adjacency matrix of class \code{\link{matrix}}. Non-logical
#' matrices are first converted into logical matrices using
#' \code{\link{as.logical}}.
#'
#' @return The sparse adjacency matrix as object of class \code{\link{list}}.
#'
#' @noRd
adj2nb = function(x) {
  if (! is.logical(x)) {
    apply(x, 1, \(x) which(as.logical(x)), simplify = FALSE)
  } else {
    apply(x, 1, which, simplify = FALSE)
  }
}

#' Convert a neighbor list into a sfnetwork
#'
#' Neighbor lists are sparse adjacency matrices that specify for each node to
#' which other nodes it is adjacent.
#'
#' @param neighbors A list with one element per node, holding the indices of
#' the nodes it is adjacent to.
#'
#' @param nodes The nodes themselves as an object of class \code{\link[sf]{sf}}
#' or \code{\link[sf]{sfc}} with \code{POINT} geometries.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param edges_as_lines Should the created edges be spatially explicit, i.e.
#' have \code{LINESTRING} geometries stored in a geometry list column? Defaults
#' to \code{TRUE}.
#'
#' @param compute_length Should the geographic length of the edges be stored in
#' a column named \code{length}? Defaults to \code{FALSE}.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @importFrom tibble tibble
#' @noRd
nb2net = function(neighbors, nodes, directed = TRUE, edges_as_lines = TRUE,
                  compute_length = FALSE) {
  # Define the edges by their from and to nodes.
  # An edge will be created between each neighboring node pair.
  edges = rbind(
    rep(c(1:length(neighbors)), lengths(neighbors)),
    do.call("c", neighbors)
  )
  if (! directed) {
    # If the network is undirected:
    # --> Edges i -> j and j -> i are the same.
    # --> We create the network only with unique edges.
    edges = unique(apply(edges, 2, sort), MARGIN = 2)
  }
  # Create the sfnetwork object.
  sfnetwork(
    nodes = nodes,
    edges = tibble(from = edges[1, ], to = edges[2, ]),
    directed = directed,
    edges_as_lines = edges_as_lines,
    compute_length = compute_length,
    force = TRUE
  )
}
