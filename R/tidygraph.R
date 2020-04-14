#' @importFrom tidygraph activate
#' @export
tidygraph::activate

#' @importFrom tidygraph active
#' @export
tidygraph::active

#' @importFrom tidygraph .E
#' @export
tidygraph::`.E`

#' @importFrom tidygraph .G
#' @export
tidygraph::`.G`

#' @importFrom tidygraph .N
#' @export
tidygraph::`.N`

#' @importFrom tidygraph %>%
#' @export
tidygraph::`%>%`

#' @importFrom tidygraph as_tibble
#' @export
tidygraph::as_tibble

#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.sfnetwork = function(x, ...) {
  class(x) = setdiff(class(x), "sfnetwork")
  x
}
