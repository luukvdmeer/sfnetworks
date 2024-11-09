#' Group nodes based on spatial distance
#'
#' This function forms a spatial extension to the
#' \code{\link[tidygraph:group_graph]{grouping}} functions in tidygraph,
#' allowing to detect communities with spatial clustering algorithms.
#'
#' @param dist The distance within which nodes are clustered together. Should
#' be specified in meters.
#'
#' @param algorithm The spatial clustering algorithm to use. See Details.
#'
#' @param use_network_distance Should the distance between nodes be computed as
#' the distance over the network (using \code{\link{st_network_distance}}?
#' Defaults to \code{TRUE}. If set to \code{FALSE}, the straight-line distance
#' (using \code{\link[sf]{st_distance}}) is computed instead.
#'
#' @param min_nodes The minimum number of nodes in each cluster. Defaults to 1.
#'
#' @param ... Additional arguments passed on to the clustering algorithm. See
#' Details.
#'
#' @details The currently supported spatial clustering algorithms are the
#' following:
#'
#' \itemize{
#'   \item \code{dbscan}: Uses density-based spatial clustering as implemented
#'   in the \code{\link[dbscan]{dbscan}} function of the dbscan package. This
#'   requires the dbscan package to be installed.
#' }
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
#'   mutate(group = group_spatial(0.25))
#'
#' @importFrom stats as.dist
#' @export
group_spatial = function(dist, algorithm = "dbscan",
                         use_network_distance = TRUE, min_nodes = 1, ...) {
  require_active_nodes()
  if (use_network_distance) {
    distmat = as.dist(st_network_distance(.G()))
  } else {
    distmat = as.dist(st_distance(.G()))
  }
  groups = switch(
    algorithm,
    dbscan = group_spatial_dbscan(distmat, dist, min_nodes, ...),
    raise_unknown_input("algorithm", algorithm, c("dbscan"))
  )
  desc_enumeration(groups)
}

#' @importFrom rlang check_installed
group_spatial_dbscan = function(x, dist, min_nodes, ...) {
  check_installed("dbscan") # Package dbscan is required for this function.
  dbscan::dbscan(x, eps = dist, minPts = min_nodes, ...)$cluster
}

# From https://github.com/thomasp85/tidygraph/blob/main/R/group.R
# Take an integer vector and recode it so the most prevalent integer is 1, etc.
desc_enumeration = function(group) {
  match(group, as.integer(names(sort(table(group), decreasing = TRUE))))
}
