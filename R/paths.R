#' Find shortest paths between nodes in a spatial network
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param from The node where the paths should start. Evaluated by
#' \code{\link{evaluate_node_query}}. When multiple nodes are given, only the
#' first one is used.
#'
#' @param to The nodes where the paths should end. Evaluated by
#' \code{\link{evaluate_node_query}}. By default, all nodes in the network are
#' included.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Evaluated by \code{\link{evaluate_edge_spec}}. The default is
#' \code{\link{edge_length}}, which computes the geographic lengths of the
#' edges.
#'
#' @param all Should all shortest paths be returned for each pair of nodes? If
#' set to \code{FALSE}, only one shortest path is returned for each pair of
#' nodes, even if multiple shortest paths exist. Defaults to \code{FALSE}.
#'
#' @param k The number of paths to find. Setting this to any integer higher
#' than 1 returns not only the shortest path, but also the next k - 1 loopless
#' shortest paths, which may be longer than the shortest path. Currently, this
#' is only supported for one-to-one routing, meaning that both the from and to
#' argument should be of length 1. This argument is ignored if \code{all} is
#' set to \code{TRUE}.
#'
#' @param direction The direction of travel. Defaults to \code{'out'}, meaning
#' that the direction given by the network is followed and paths are found from
#' the node given as argument \code{from}. May be set to \code{'in'}, meaning
#' that the opposite direction is followed an paths are found towards the node
#' given as argument \code{from}. May also be set to \code{'all'}, meaning that
#' the network is considered to be undirected. This argument is ignored for
#' undirected networks.
#'
#' @param use_names If a column named \code{name} is present in the nodes
#' table, should these names be used to encode the nodes in a path, instead of
#' the node indices? Defaults to \code{TRUE}. Ignored when the nodes table does
#' not have a column named \code{name}.
#'
#' @param return_cost Should the total cost of each path be computed? Defaults
#' to \code{TRUE}.
#'
#' @param return_geometry Should a linestring geometry be constructed for each
#' path? Defaults to \code{TRUE}. The geometries are constructed by calling
#' \code{\link[sf]{st_line_merge}} on the linestring geometries of the edges in
#' the path. Ignored for networks with spatially implicit edges.
#'
#' @param ... Additional arguments passed on to the wrapped igraph functions.
#' Arguments \code{predecessors} and \code{inbound.edges} are ignored.
#' Instead of the \code{mode} argument, use the \code{direction} argument.
#'
#' @details For more details on the wrapped igraph functions see the
#' \code{\link[igraph]{distances}} and \code{\link[igraph]{k_shortest_paths}}
#' documentation pages.
#'
#' @seealso \code{\link{st_network_cost}}, \code{\link{st_network_travel}}
#'
#' @return An object of class \code{\link[sf]{sf}} with one row per requested
#' path. If \code{return_geometry = FALSE}, a \code{\link[tibble]{tbl_df}} is
#' returned instead. If a requested path could not be found, it is included in
#' the output as an empty path.
#'
#' Depending on the argument settings, the output may include the following
#' columns:
#'
#' \itemize{
#'   \item \code{from}: The index of the node at the start of the path.
#'   \item \code{to}: The index of the node at the end of the path.
#'   \item \code{nodes}: A vector containing the indices of all nodes on the
#'   path, in order of visit.
#'   \item \code{edges}: A vector containing the indices of all edges on the
#'   path, in order of visit.
#'   \item \code{path_found}: A boolean describing if the requested path exists.
#'   \item \code{cost}: The total cost of the path, obtained by summing the
#'   weights of all visited edges. Included if \code{return_cost = TRUE}.
#'   \item \code{geometry}: The geometry of the path, obtained by merging the
#'   geometries of all visited edges. Included if \code{return_geometry = TRUE}
#'   and the network has spatially explicit edges.
#' }
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#'
#' net = as_sfnetwork(roxel, directed = FALSE) |>
#'   st_transform(3035)
#'
#' # Compute the shortest path between two nodes.
#' # Note that geographic edge length is used as edge weights by default.
#' paths = st_network_paths(net, from = 495, to = 121)
#' paths
#'
#' plot(net, col = "grey")
#' plot(st_geometry(paths), col = "red", lwd = 1.5, add = TRUE)
#'
#' # Compute the shortest paths from one to multiple nodes.
#' # This will return a tibble with one row per path.
#' st_network_paths(net, from = 495, to = c(121, 131, 141))
#'
#' # Compute the shortest path between two spatial point features.
#' # These are snapped to their nearest node before finding the path.
#' p1 = st_geometry(net, "nodes")[495] + st_sfc(st_point(c(50, -50)))
#' st_crs(p1) = st_crs(net)
#' p2 = st_geometry(net, "nodes")[121] + st_sfc(st_point(c(-10, 100)))
#' st_crs(p2) = st_crs(net)
#'
#' paths = st_network_paths(net, from = p1, to = p2)
#' paths
#'
#' plot(net, col = "grey")
#' plot(c(p1, p2), col = "black", pch = 8, add = TRUE)
#' plot(st_geometry(paths), col = "red", lwd = 1.5, add = TRUE)
#'
#' # Use a node type query function to specify destinations.
#' st_network_paths(net, 1, node_is_adjacent(1))
#'
#' # Use a spatial edge measure to specify edge weights.
#' # By default edge_length() is used.
#' st_network_paths(net, p1, p2, weights = edge_displacement())
#'
#' # Use a column in the edges table to specify edge weights.
#' # This uses tidy evaluation.
#' net |>
#'   activate("edges") |>
#'   mutate(foo = runif(n(), min = 0, max = 1)) |>
#'   st_network_paths(p1, p2, weights = foo)
#'
#' # Compute the shortest paths without edge weights.
#' # This is the path with the fewest number of edges, ignoring space.
#' st_network_paths(net, p1, p2, weights = NULL)
#'
#' par(oldpar)
#'
#' @export
st_network_paths = function(x, from, to = node_ids(x),
                            weights = edge_length(), all = FALSE, k = 1,
                            direction = "out", use_names = TRUE,
                            return_cost = TRUE, return_geometry = TRUE, ...) {
  UseMethod("st_network_paths")
}

#' @importFrom rlang enquo
#' @export
st_network_paths.sfnetwork = function(x, from, to = node_ids(x),
                                      weights = edge_length(),
                                      all = FALSE, k = 1, direction = "out",
                                      use_names = TRUE, return_cost = TRUE,
                                      return_geometry = TRUE, ...) {
  # Evaluate the given from node query.
  from = evaluate_node_query(x, enquo(from))
  if (any(is.na(from))) raise_na_values("from")
  # Evaluate the given to node query.
  to = evaluate_node_query(x, enquo(to))
  if (any(is.na(to))) raise_na_values("to")
  # Evaluate the given weights specification.
  weights = evaluate_weight_spec(x, enquo(weights))
  # Compute the shortest paths.
  find_paths(
    x, from, to, weights,
    all = all,
    k = k,
    direction = direction,
    use_names = use_names,
    return_cost = return_cost,
    return_geometry = return_geometry,
    ...
  )
}

#' @importFrom igraph vertex_attr vertex_attr_names
#' @importFrom rlang has_name
#' @importFrom sf st_as_sf
find_paths = function(x, from, to, weights, all = FALSE, k = 1,
                      direction = "out", use_names = TRUE, return_cost = TRUE,
                      return_geometry = TRUE, ...) {
  # Find paths with the given router.
  paths = igraph_paths(x, from, to, weights, all, k, direction, ...)
  # Convert node indices to node names if requested.
  if (use_names && "name" %in% vertex_attr_names(x)) {
    nnames = vertex_attr(x, "name")
    paths$from = do.call("c", lapply(paths$from, \(x) nnames[x]))
    paths$to = do.call("c", lapply(paths$to, \(x) nnames[x]))
    paths$node_path = lapply(paths$node_path, \(x) nnames[x])
  }
  # Compute total cost of each path if requested.
  if (return_cost) {
    if (length(weights) == 1 && is.na(weights)) {
      costs = do.call("c", lapply(paths$edge_paths, length))
    } else {
      costs = do.call("c", lapply(paths$edge_paths, \(x) sum(weights[x])))
    }
    costs[!paths$path_found] = Inf
    paths$cost = costs
  }
  # Construct path geometries of requested.
  if (return_geometry && has_explicit_edges(x)) {
    egeom = pull_edge_geom(x)
    pgeom = do.call("c", lapply(paths$edge_paths, \(x) merge_lines(egeom[x])))
    paths$geometry = pgeom
    paths = st_as_sf(paths)
  }
  paths
}

#' @importFrom igraph all_shortest_paths shortest_paths k_shortest_paths
#' igraph_opt igraph_options
#' @importFrom methods hasArg
#' @importFrom tibble tibble
igraph_paths = function(x, from, to, weights, all = FALSE, k = 1,
                        direction = "out", ...) {
  # Change default igraph options.
  # This prevents igraph returns node or edge indices as formatted sequences.
  # We only need the "raw" integer indices.
  # Changing this option improves performance especially on large networks.
  default_igraph_opt = igraph_opt("return.vs.es")
  igraph_options(return.vs.es = FALSE)
  on.exit(igraph_options(return.vs.es = default_igraph_opt))
  # The direction argument is used instead of igraphs mode argument.
  # This means the mode argument should not be set.
  if (hasArg("mode")) raise_unsupported_arg("mode", replacement = "direction")
  # Any igraph paths function supports only a single from node.
  # If multiple from nodes are given we take only the first one.
  if (length(from) > 1) raise_multiple_elements("from"); from = from[1]
  # Call igraph paths calculation function depending on the settings.
  if (all) {
    # Call igraph::all_shortest_paths to obtain the requested paths.
    paths = all_shortest_paths(
      x, from, to,
      weights = weights,
      mode = direction,
      ...
    )
    # Extract the nodes and edges in each path.
    npaths = paths$vpaths
    epaths = paths$epaths
    # Define for each path where it starts and ends.
    starts = do.call("c", lapply(npaths, `[`, 1))
    ends = do.call("c", lapply(npaths, last_element))
    # Define for each path if the path was actually found or is empty.
    # When all = TRUE we return only paths that exist.
    # Hence each of the returned paths is found.
    path_found = rep(TRUE, length(starts))
  } else {
    k = as.integer(k)
    if (k == 1) {
      # Call igraph::shortest_paths to obtain the requested paths.
      paths = shortest_paths(
        x, from, to,
        weights = weights,
        output = "both",
        mode = direction,
        ...
      )
      # Extract the nodes and edges in each path.
      npaths = paths$vpath
      epaths = paths$epath
      # Define for each path where it starts and ends.
      starts = rep(from, length(to))
      ends = to
      # Define for each path if the path was actually found or is empty.
      path_found = lengths(epaths) > 0 | starts == ends
    } else {
      # For k shortest paths igraph only supports one-to-one routing.
      # Hence only a single to node is supported.
      # If multiple to nodes are given we take only the first one.
      if (length(to) > 1) raise_multiple_elements("to"); to = to[1]
      # Call igraph::k_shortest_paths to obtain the requested paths.
      paths = k_shortest_paths(
        x, from, to,
        k = k,
        weights = weights,
        mode = direction,
        ...
      )
      # Extract the nodes and edges in each path.
      npaths = paths$vpaths
      epaths = paths$epaths
      # We will always return k paths.
      # Even if that many paths do not exists.
      # Hence if the returned number of paths is smaller than k:
      # --> We add empty paths to the result.
      n = length(npaths)
      if (n < k) {
        npaths = c(npaths, rep(list(numeric(0)), k - n))
        epaths = c(epaths, rep(list(numeric(0)), k - n))
      }
      # Define for each path where it starts and ends.
      # Since we do one-to-one routing these are always the same nodes.
      starts = rep(from, k)
      ends = rep(to, k)
      # Define for each path if the path was actually found or is empty.
      path_found = c(rep(TRUE, n), rep(FALSE, k - n))
    }
  }
  # Return in a tibble.
  tibble(
    from = starts, to = ends,
    node_path = npaths, edge_path = epaths,
    path_found = path_found
  )
}
