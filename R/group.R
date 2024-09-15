#' Group nodes based on spatial distance
#'
#' @param dist distance within which nodes are clustered to each other
#'
#' @param algorithm the grouping algorithm used to perform spatial clustering.
#' Defaults to the `dbscan::dbscan()` algorithm.
#'
#' @param network_distance should the distance be based on the network distance
#' (default, uses `st_network_distance()` internally) or euclidean distance
#' (uses `sf::st_distance()` internally)?
#'
#' @param min_nodes minimum number of nodes assigned to each cluster. Defaults
#' to 1 so that every node is assigned a cluster even if it is the only member
#' of that cluster.
#'
#' @param ... other arguments passed onto the algorithm,
#' e.g. `dbscan::dbscan()`
#'
#' @importFrom dbscan dbscan
#' @importFrom stats as.dist
#' @export
group_spatial = function(dist, algorithm = "dbscan",
                         network_distance = TRUE,
                         min_nodes = 1,
                         ...) {
  require_active_nodes()
  if(algorithm == "dbscan") {
    if (network_distance) {
      distmat = as.dist(st_network_distance(.G()))
    } else {
      distmat = as.dist(st_distance(.G()))
    }
    group = dbscan(distmat, eps = dist, minPts = min_nodes, ...)$cluster
  }
  desc_enumeration(group)
}


# HELPERS -----------------------------------------------------------------

# From https://github.com/thomasp85/tidygraph/blob/main/R/group.R
# Take an integer vector and recode it so the most prevalent integer is 1 and so
# forth
desc_enumeration <- function(group) {
  match(group, as.integer(names(sort(table(group), decreasing = TRUE))))
}
