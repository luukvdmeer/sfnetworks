#' @importFrom rlang enquo eval_tidy expr
#' @importFrom tidygraph .E .register_graph_context
evaluate_weight_spec = function(data, weights) {
  .register_graph_context(data, free = TRUE)
  weights = eval_tidy(enquo(weights), .E())
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
  weights
}