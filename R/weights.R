#' Specify edge weights in a spatial network
#'
#' This function is not meant to be called directly, but used inside other
#' functions that accept the specification of edge weights.
#'
#' @param data An object of class \code{\link{sfnetwork}}.
#'
#' @param spec The specification that defines how to compute or extract edge
#' weights defused into a \code{\link[rlang:topic-quosure]{quosure}}. See
#' Details for the different ways in which edge weights can be specified.
#'
#' @details There are multiple ways in which edge weights can be specified in
#' sfnetworks. The specification can be formatted as follows:
#'
#' \itemize{
#'   \item As edge measure function: A
#'   \link[=spatial_edge_measures]{spatial edge measure function} computes a
#'   given measure for each edge, which will then be used as edge weights.
#'   \item As column name: A column in the edges table of the network that
#'   contains the edge weights. Note that tidy evaluation is used and hence the
#'   column name should be unquoted.
#'   \item As a numeric vector: This vector should be of the same length as the
#'   number of edges in the network, specifying for each edge what its weight
#'   is.
#'   \item As dual weights: Dual weights can be specified by the
#'   \code{\link{dual_weights}} function. This allows to use a different set of
#'   weights for shortest paths computation and for reporting the total cost of
#'   those paths. Note that not every routing backend support dual-weighted
#'   routing.
#' }
#'
#' If the weight specification is \code{NULL} or \code{NA}, this means that no
#' edge weights are used. For shortest path computation, this means that the
#' shortest path is simply the path with the fewest number of edges.
#'
#' @note For backward compatibility it is currently also still possible to
#' format the specification as a quoted column name, but this may be removed in
#' future versions.
#'
#' Also note that many shortest path algorithms require edge weights to be
#' positive.
#'
#' @return A numeric vector of edge weights.
#'
#' @importFrom cli cli_abort
#' @importFrom rlang eval_tidy expr
#' @importFrom tidygraph .E .register_graph_context
#' @export
evaluate_weight_spec = function(data, spec) {
  .register_graph_context(data, free = TRUE)
  weights = eval_tidy(spec, .E())
  if (inherits(weights, "dual_weights")) {
    weights = lapply(weights, \(x) evaluate_weight_spec(data, x))
    class(weights) = c("dual_weights", "list")
    return (weights)
  }
  if (is_single_string(weights)) {
    # Allow character values for backward compatibility.
    deprecate_weights_is_string()
    weights = eval_tidy(expr(.data[[weights]]), .E())
  }
  if (is.null(weights)) {
    # Convert NULL to NA to align with tidygraph instead of igraph.
    deprecate_weights_is_null()
    weights = NA
  }
  n = length(weights)
  if (!(n == 1 && is.na(weights)) && n != n_edges(data)) {
    cli_abort(c(
      "Failed to evaluate the edge weight specification.",
      "x" = "The amount of weights does not equal the number of edges."
    ))
  }
  weights
}

#' Specify dual edge weights
#'
#' Dual edge weights are two sets of edge weights, one (the actual weight) to
#' determine the shortest path, and the other (the reported weight) to report
#' the cost of that path.
#'
#' @param reported The edge weights to be reported. Evaluated by
#' \code{\link{evaluate_weight_spec}}.
#'
#' @param actual The actual edge weights to be used to determine shortest paths.
#' Evaluated by \code{\link{evaluate_weight_spec}}.
#'
#' @details Dual edge weights enable dual-weighted routing. This is supported
#' by the \code{\link[dodgr]{dodgr}} routing backend.
#'
#' @returns An object of class \code{dual_weights}.
#'
#' @importFrom rlang enquo
#' @export
dual_weights = function(reported, actual) {
  out = list(reported = enquo(reported), actual = enquo(actual))
  class(out) = c("dual_weights", "list")
  out
}