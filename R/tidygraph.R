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

#' @importFrom tidygraph as_tbl_graph morph
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
        morph(as_tbl_graph(.data), ...),
        error = function(e2) stop(e1)
      )
    }
  )
  # If morphed data still consist of valid sfnetworks:
  # --> Convert the morphed_tbl_graph into a morphed_sfnetwork.
  # --> Otherwise, just return the morphed_tbl_graph.
  if (has_spatial_nodes(morphed_data[[1]])) {
    morphed_data = morphed_tbg_to_morphed_sfn(morphed_data)
  }
  morphed_data
}

morphed_tbg_to_morphed_sfn = function(x) {
  structure(
    lapply(x, tbg_to_sfn),
    class = c("morphed_sfnetwork", class(x)),
    .orig_graph = attr(x, ".orig_graph"),
    .morpher = attr(x, ".morpher")
  )
}