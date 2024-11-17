#' Compute spatial centrality measures
#'
#' These functions are a collection of centrality measures that are specific
#' for spatial networks, and form a spatial extension to
#' \code{\link[tidygraph:centrality]{centrality measures}} in tidygraph.
#'
#' @param ... Additional arguments passed on to other functions.
#'
#' @details Just as with all centrality functions in tidygraph, these functions
#' are meant to be called inside tidygraph verbs such as
#' \code{\link[tidygraph]{mutate}} or \code{\link[tidygraph]{filter}}, where
#' the network that is currently being worked on is known and thus not needed
#' as an argument to the function. If you want to use an algorithm outside of
#' the tidygraph framework you can use \code{\link[tidygraph]{with_graph}} to
#' set the context temporarily while the algorithm is being evaluated.
#'
#' @return A numeric vector of the same length as the number of nodes in the
#' network.
#'
#' @name spatial_centrality
NULL

#' @describeIn spatial_centrality The straightness centrality of node i is the
#' average ratio of Euclidean distance and network distance between node i and
#' all other nodes in the network. \code{...} is forwarded to
#' \code{\link{st_network_distance}} to compute the network distance matrix.
#' Euclidean distances are computed using \code{\link[sf]{st_distance}}.
#'
#' @examples
#' library(tidygraph, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel, directed = FALSE)
#'
#' net |>
#'   activate(nodes) |>
#'   mutate(sc = centrality_straightness())
#'
#' @importFrom sf st_distance
#' @export
centrality_straightness = function(...) {
  require_active_nodes()
  x = .G()
  # Compute network distances.
  ndists = st_network_distance(
    x,
    from = node_ids(x),
    to = node_ids(x),
    Inf_as_NaN = TRUE,
    ...
  )
  # Compute Euclidean distances.
  sdists = st_distance(pull_node_geom(x, focused = TRUE))
  # Compute ratios.
  ratios = sdists / ndists
  # Compute average of ratios per node.
  apply(ratios, 1, mean, na.rm = TRUE)
}
