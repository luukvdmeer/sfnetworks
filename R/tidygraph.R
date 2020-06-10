#' @importFrom tidygraph activate
#' @export
tidygraph::activate

#' @importFrom tidygraph active
#' @export
tidygraph::active

#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.sfnetwork = function(x, ...) {
  class(x) = setdiff(class(x), "sfnetwork")
  x
}

tbg_to_sfn = function(.data) {
  class(.data) = c("sfnetwork", class(.data))
  .data
}

#' @importFrom tidygraph morph
#' @export
morph.sfnetwork = function(.data, .f, ...) {
  # Morph using tidygraphs morphing functionality.
  # First convert to tbl_graph if sfnetwork object gives errors.
  morphed = tryCatch(
    NextMethod(),
    error = function(e) tidygraph::morph(as_tbl_graph(.data), .f, ...)
  )
  # Try to convert morphed elements into sfnetwork objects.
  # If not possible, simply return as morphed_tbl_graph.
  tryCatch(
    {morphed_sfn = lapply(morphed, function(x) as_sfnetwork(x, force = TRUE))},
    error = function(e) return(morphed)
  )
  # Return as object of class morphed_sfnetwork.
  structure(
    morphed,
    class = c("morphed_sfnetwork", class(morphed)),
    .orig_graph = as_sfnetwork(attr(morphed, ".orig_graph"), force = TRUE),
    .morpher = attr(morphed, ".morpher")
  )
}