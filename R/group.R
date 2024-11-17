#' Group nodes based on spatial distance
#'
#' These functions forms a spatial extension to the
#' \code{\link[tidygraph:group_graph]{grouping}} functions in tidygraph,
#' allowing to detect communities with spatial clustering algorithms.
#'
#' @param use_network_distance Should the distance between nodes be computed as
#' the distance over the network (using \code{\link{st_network_distance}}?
#' Defaults to \code{TRUE}. If set to \code{FALSE}, the straight-line distance
#' (using \code{\link[sf]{st_distance}}) is computed instead.
#'
#' @param ... Additional arguments passed on to the clustering algorithm.
#'
#' @details Just as with all grouping functions in tidygraph, spatial grouping
#' functions are meant to be called inside tidygraph verbs such as
#' \code{\link[tidygraph]{mutate}} or \code{\link[tidygraph]{filter}}, where
#' the network that is currently being worked on is known and thus not needed
#' as an argument to the function. If you want to use an algorithm outside of
#' the tidygraph framework you can use \code{\link[tidygraph]{with_graph}} to
#' set the context temporarily while the algorithm is being evaluated.
#'
#' @returns A numeric vector with the membership for each node in the network.
#' The enumeration happens in order based on group size progressing from the
#' largest to the smallest group.
#'
#' @examples
#' library(tidygraph, quietly = TRUE)
#'
#' play_geometric(10, 0.5) |>
#'   activate(nodes) |>
#'   mutate(group = group_spatial_dbscan(0.25))
#'
#' @name group_spatial
NULL

#' @describeIn group_spatial Uses density-based spatial clustering as
#' implemented in the \code{\link[dbscan]{dbscan}} function of the dbscan
#' package. This requires the dbscan package to be installed. Each node marked
#' as noise will form its own cluster.
#'
#' @param epsilon The value of the epsilon parameter for the DBSCAN clustering
#' algorithm, defining the radius of the neighborhood of a node.
#'
#' @param min_pts The value of the minPts parameter for the DBSCAN clustering
#' algorithm, defining the minimum number of points in the neighborhood to be
#' considered a core point.
#'
#' @importFrom rlang check_installed
#' @export
group_spatial_dbscan = function(epsilon, min_pts = 1,
                                use_network_distance = TRUE, ...) {
  check_installed("dbscan") # Package dbscan is required for this function.
  require_active_nodes()
  dists = make_distance_matrix(.G(), use_network_distance)
  groups = dbscan::dbscan(dists, eps = epsilon, minPts = min_pts, ...)$cluster
  desc_enumeration(groups)
}

#' @importFrom stats as.dist
make_distance_matrix = function(x, use_network_distance = TRUE) {
  if (use_network_distance) {
    as.dist(st_network_distance(x))
  } else {
    as.dist(st_distance(x))
  }
}

# From https://github.com/thomasp85/tidygraph/blob/main/R/group.R
# Take an integer vector and recode it so the most prevalent integer is 1, etc.
desc_enumeration = function(group) {
  match(group, as.integer(names(sort(table(group), decreasing = TRUE))))
}
