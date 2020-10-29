#' @importFrom tidygraph activate
#' @export
tidygraph::activate

#' @importFrom tidygraph active
#' @export
tidygraph::active

#' @importFrom tidygraph %>%
#' @export
tidygraph::`%>%`

#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.sfnetwork = function(x, ...) {
  class(x) = setdiff(class(x), "sfnetwork")
  x
}

#' @importFrom tidygraph morph
#' @export
morph.sfnetwork = function(.data, ...) {
  # Morph using tidygraphs morphing functionality:
  # --> First try to morph the sfnetwork object directly.
  # --> If this gives errors, convert to tbl_graph and then morph.
  # --> If that also gives errors, return the first error found.
  morphed_data = tryCatch(
    NextMethod(),
    error = function(e1) {
      tryCatch(
        tidygraph::morph(as_tbl_graph(.data), ...),
        error = function(e2) stop(e1)
      )
    }
  )
  # Try to convert morphed elements into sfnetwork objects.
  # If not possible, simply return as morphed_tbl_graph.
  tryCatch(
    as_morphed_sfn(morphed_data),
    error = function(e) morphed_data
  )
}

as_morphed_sfn = function(x) {
  structure(
    suppressMessages(lapply(x, as_sfnetwork)),
    class = c("morphed_sfnetwork", class(x)),
    .orig_graph = tbg_to_sfn(as_tbl_graph(attr(x, ".orig_graph"))),
    .morpher = attr(x, ".morpher")
  )
}