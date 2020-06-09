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

#' @importFrom tidygraph morph
#' @export
morph.sfnetwork = function(.data, .f, ...) {
  morphed_tbg = tidygraph::morph(as_tbl_graph(.data), .f, ...)
  tryCatch(
    morphed_tbg_to_morphed_sfn(morphed_tbg),
    error = function(e) return(morphed_tbg)
  )
}

#' @importFrom tidygraph unmorph
#' @export
unmorph.morphed_sfnetwork = function(.data) {
  morphed_tbg = morphed_sfn_to_morphed_tbg(.data)
  unmorphed_tbg = tidygraph::unmorph(morphed_tbg)
  tbg_to_sfn(unmorphed_tbg)
}

tbg_to_sfn = function(.data) {
  class(.data) = c("sfnetwork", class(.data))
  .data
}

morphed_sfn_to_morphed_tbg = function(.data) {
  morphed_tbg = lapply(.data, as_tbl_graph)
  structure(
    morphed_tbg,
    class = setdiff(class(.data), "morphed_sfnetwork"),
    .orig_graph = as_tbl_graph(attr(.data, ".orig_graph")),
    .morpher = attr(.data, ".morpher")
  )
}

morphed_tbg_to_morphed_sfn = function(.data) {
  morphed_sfn = lapply(.data, function(x) as_sfnetwork(x, force = TRUE))
  structure(
    morphed_sfn,
    class = c("morphed_sfnetwork", class(.data)),
    .orig_graph = as_sfnetwork(attr(.data, ".orig_graph"), force = TRUE),
    .morpher = attr(.data, ".morpher")
  )
}