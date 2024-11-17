#' Find shortest paths between nodes in a spatial network
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param from The node where the paths should start. Evaluated by
#' \code{\link{evaluate_node_query}}.
#'
#' @param to The nodes where the paths should end. Evaluated by
#' \code{\link{evaluate_node_query}}. By default, all nodes in the network are
#' included.
#'
#' @param weights The edge weights to be used in the shortest path calculation.
#' Evaluated by \code{\link{evaluate_weight_spec}}. The default is
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
#' @param router The routing backend to use for the shortest path computation.
#' Currently supported options are \code{'igraph'} and \code{'dodgr'}. See
#' Details.
#'
#' @param use_names If a column named \code{name} is present in the nodes
#' table, should these names be used to encode the nodes in a path, instead of
#' the node indices? Defaults to \code{FALSE}. Ignored when the nodes table does
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
#' @param ... Additional arguments passed on to the underlying function of the
#' chosen routing backend. See Details.
#'
#' @details The sfnetworks package does not implement its own routing algorithms
#' to find shortest paths. Instead, it relies on "routing backends", i.e. other
#' R packages that have implemented such algorithms. Currently two different
#' routing backends are supported.
#'
#' The default is \code{\link[igraph]{igraph}}. This package supports
#' one-to-many shortest path calculation with the
#' \code{\link[igraph]{shortest_paths}} function. Note that multiple from nodes
#' are not supported. If multiple from nodes are given, only the first one is
#' taken. The igraph router also supports the computation of all shortest path
#' (see the \code{all} argument) through the
#' \code{\link[igraph]{all_shortest_paths}} function and of k shortest paths
#' (see the \code{k} argument) through the
#' \code{\link[igraph]{k_shortest_paths}} function. In the latter case, only
#' one-to-one routing is supported, meaning that also only one to node should
#' be provided. The igraph router does not support dual-weighted routing.
#'
#' The second supported routing backend is \code{\link[dodgr]{dodgr}}. This
#' package supports many-to-many shortest path calculation with the
#' \code{\link[dodgr]{dodgr_paths}} function. It also supports dual-weighted
#' routing. The computation of all shortest paths and k shortest paths is
#' currently not supported by the dodgr router. The dodgr package is a
#' conditional dependency of sfnetworks. Using the dodgr router requires the
#' dodgr package to be installed.
#'
#' The default router can be changed by setting the \code{sfn_default_router}
#' option.
#'
#' @seealso \code{\link{st_network_cost}}, \code{\link{st_network_travel}}
#'
#' @return An object of class \code{\link[sf]{sf}} with one row per requested
#' path. If \code{return_geometry = FALSE} or edges are spatially implicit, a
#' \code{\link[tibble]{tbl_df}} is returned instead. If a requested path could
#' not be found, it is included in the output as an empty path.
#'
#' Depending on the argument settings, the output may include the following
#' columns:
#'
#' \itemize{
#'   \item \code{from}: The index of the node at the start of the path.
#'   \item \code{to}: The index of the node at the end of the path.
#'   \item \code{node_path}: A vector containing the indices of all nodes on
#'   the path, in order of visit.
#'   \item \code{edge_path}: A vector containing the indices of all edges on
#'   the path, in order of visit.
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
#' plot(st_geometry(net)[paths$from], pch = 20, cex = 2, add = TRUE)
#' plot(st_geometry(paths), col = "orange", lwd = 3, add = TRUE)
#'
#' # Compute the shortest paths from one to multiple nodes.
#' # This will return a tibble with one row per path.
#' paths = st_network_paths(net, from = 495, to = c(121, 131, 141))
#' paths
#'
#' plot(net, col = "grey")
#' plot(st_geometry(net)[paths$from], pch = 20, cex = 2, add = TRUE)
#' plot(st_geometry(paths), col = "orange", lwd = 3, add = TRUE)
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
#' plot(c(p1, p2), pch = 20, cex = 2, add = TRUE)
#' plot(st_geometry(net)[paths$from], pch = 4, cex = 2, add = TRUE)
#' plot(st_geometry(paths), col = "orange", lwd = 3, add = TRUE)
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
#' st_network_paths(net, p1, p2, weights = NA)
#'
#' # Use the dodgr router for many-to-many routing.
#' paths = st_network_paths(net,
#'   from = c(1, 2),
#'   to = c(10, 11),
#'   router = "dodgr"
#' )
#'
#' # Use the dodgr router for dual-weighted routing.
#' paths = st_network_paths(net,
#'   from = c(1, 2),
#'   to = c(10, 11),
#'   weights = dual_weights(edge_segment_count(), edge_length()),
#'   router = "dodgr"
#' )
#'
#' par(oldpar)
#'
#' @export
st_network_paths = function(x, from, to = node_ids(x),
                            weights = edge_length(), all = FALSE, k = 1,
                            direction = "out",
                            router = getOption("sfn_default_router", "igraph"),
                            use_names = FALSE, return_cost = TRUE,
                            return_geometry = TRUE, ...) {
  UseMethod("st_network_paths")
}

#' @importFrom methods hasArg
#' @importFrom rlang enquo
#' @export
st_network_paths.sfnetwork = function(x, from, to = node_ids(x),
                                      weights = edge_length(),
                                      all = FALSE, k = 1,
                                      direction = "out",
                                      router = getOption("sfn_default_router", "igraph"),
                                      use_names = FALSE, return_cost = TRUE,
                                      return_geometry = TRUE, ...) {
  # Deprecate the type argument.
  if (hasArg("type")) deprecate_type()
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
    router = router,
    use_names = use_names,
    return_cost = return_cost,
    return_geometry = return_geometry,
    ...
  )
}

#' @importFrom igraph vertex_attr vertex_attr_names
#' @importFrom sf st_as_sf
find_paths = function(x, from, to, weights, all = FALSE, k = 1,
                      direction = "out",
                      router = getOption("sfn_default_router", "igraph"),
                      use_names = FALSE, return_cost = TRUE,
                      return_geometry = TRUE, ...) {
  # Find paths with the given router.
  paths = switch(
    router,
    igraph = igraph_paths(x, from, to, weights, all, k, direction, ...),
    dodgr = dodgr_paths(x, from, to, weights, all, k, direction, ...),
    raise_unknown_input("router", router, c("igraph", "dodgr"))
  )
  # Convert node indices to node names if requested.
  if (use_names && "name" %in% vertex_attr_names(x)) {
    nnames = vertex_attr(x, "name")
    paths$from = do.call("c", lapply(paths$from, \(x) nnames[x]))
    paths$to = do.call("c", lapply(paths$to, \(x) nnames[x]))
    paths$node_path = lapply(paths$node_path, \(x) nnames[x])
  }
  # Define if the path was found.
  paths$path_found = lengths(paths$node_path) > 0
  # Compute total cost of each path if requested.
  if (return_cost) {
    if (inherits(weights, "dual_weights")) weights = weights$reported
    if (length(weights) == 1 && is.na(weights)) {
      costs = do.call("c", lapply(paths$edge_path, length))
    } else {
      costs = do.call("c", lapply(paths$edge_path, \(x) sum(weights[x])))
    }
    costs[!paths$path_found] = Inf
    paths$cost = costs
  }
  # Construct path geometries of requested.
  if (return_geometry && has_explicit_edges(x)) {
    egeom = pull_edge_geom(x)
    pgeom = do.call("c", lapply(paths$edge_path, \(x) merge_lines(egeom[x])))
    paths$geometry = pgeom
    paths = st_as_sf(paths)
  }
  paths
}

#' @importFrom cli cli_abort cli_warn
#' @importFrom igraph all_shortest_paths shortest_paths k_shortest_paths
#' igraph_opt igraph_options
#' @importFrom methods hasArg
#' @importFrom tibble tibble
#' @importFrom utils tail
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
  # Dual-weighted routing is not supported by igraph.
  if (inherits(weights, "dual_weights")) {
    cli_abort(c(
      "Router {.pkg igraph} does not support dual-weighted routing.",
      "i" = "Use the {.pkg dodgr} router for dual-weighted routing."
    ))
  }
  # Any igraph paths function supports only a single from node.
  # If multiple from nodes are given we take only the first one.
  if (length(from) > 1) {
    cli_warn(c(
      "Router {.pkg igraph} does not support multiple {.arg from} nodes.",
      "i" = "Only the first {.arg from} node is considered.",
      "i" = "Use the {.pkg dodgr} router for many-to-many routing."
    ))
    from = from[1]
  }
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
    ends = do.call("c", lapply(npaths, tail, 1))
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
    } else {
      # For k shortest paths igraph only supports one-to-one routing.
      # Hence only a single to node is supported.
      # If multiple to nodes are given we take only the first one.
      if (length(to) > 1) {
        cli_warn(c(
          paste(
            "Router {.pkg igraph} does not support multiple {.arg to}",
            "nodes for k shortest paths computation."
          ),
          "i" = "Only the first {.arg to} node is considered."
        ))
        to = to[1]
      }
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
    }
  }
  # Return in a tibble.
  tibble(
    from = starts, to = ends,
    node_path = npaths, edge_path = epaths
  )
}

#' @importFrom cli cli_abort
#' @importFrom igraph is_directed
#' @importFrom rlang check_installed
#' @importFrom tibble tibble
#' @importFrom utils tail
dodgr_paths = function(x, from, to, weights, all = FALSE, k = 1,
                      direction = "out", ...) {
  check_installed("dodgr") # Package dodgr is required for this function.
  # The dodgr router currently does not support:
  # --> Computing all shortest paths or k shortest path.
  if (all) {
    cli_abort(
      "Router {.pkg dodgr} does not support setting {.code all = TRUE}."
    )
  }
  if (k > 1) {
    cli_abort(
      "Router {.pkg dodgr} does not support setting {.code k > 1}."
    )
  }

  # Convert the network to dodgr format.
  x_dodgr = sfnetwork_to_minimal_dodgr(x, weights, direction)
  # Call dodgr::dodgr_paths to compute the requested paths.
  paths = dodgr::dodgr_paths(
    x_dodgr,
    from = as.character(from),
    to = as.character(to),
    vertices = FALSE,
    ...
  )
  # Unnest the nested list of edge indices.
  epaths = lapply(do.call(cbind, paths), \(x) x)
  # Infer the node paths from the edge paths.
  get_node_path = function(E) {
    N = c(x_dodgr$from[E], x_dodgr$to[tail(E, 1)])
    as.integer(N)
  }
  npaths = lapply(epaths, get_node_path)
  # Update the edge paths:
  # --> For undirected networks we duplicated and reversed all edges.
  # --> Paths that were not found should have numeric(0) as value.
  if (!is_directed(x) | direction == "all") {
    n = nrow(x_dodgr) / 2
    update_edge_path = function(E) {
      if (is.null(E) || all(is.na(E))) return (integer(0))
      is_added = E > n
      E[is_added] = E[is_added] - n
      as.integer(E)
    }
    epaths = lapply(epaths, update_edge_path)
  } else {
    update_edge_path = function(E) {
      if (is.null(E) || all(is.na(E))) return (integer(0))
      E
    }
    epaths = lapply(epaths, update_edge_path)
  }
  # Define for each path where it starts and ends.
  starts = rep(from, rep(length(to), length(from)))
  ends = rep(to, length(from))
  # Return in a tibble.
  tibble(
    from = starts, to = ends,
    node_path = npaths, edge_path = epaths
  )
}