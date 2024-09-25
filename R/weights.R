#' Specify edge weights in a spatial network
#'
#' This function is not meant to be called directly, but used inside other
#' functions that accept the specification of edge weights.
#'
#' @param data An object of class \code{\link{sfnetwork}}.
#'
#' @param spec The specification that defines how to compute or extract edge
#' weights defused into a \code{\link[dplyr:topic-quosure]{quosure}}. See
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
  if (length(weights) != n_edges(data)) {
    cli_abort(c(
      "Failed to evaluate the edge weight specification.",
      "x" = "The amount of weights does not equal the number of edges."
    ))
  }
  weights
}