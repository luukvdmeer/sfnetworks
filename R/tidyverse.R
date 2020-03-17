#' @importFrom tidygraph activate
#' @export
tidygraph::activate

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' @export
as_tibble.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = node_tibble(x),
    edges = edge_tibble(x),
    stop("Unknown active element: ", active, ". Only nodes and edges supported")
  )
}

#' @importFrom sf st_as_sf
node_tibble = function(x) {
  sf::st_as_sf(as_tibble(as_tbl_graph(x), active = "nodes"))
}

#' @importFrom sf st_as_sf
edge_tibble <- function(x) {
  edges = as_tibble(as_tbl_graph(x), active = "edges")

  tryCatch(
    expr = {
      sf::st_as_sf(edges)
    },
    error = function(e) {
      edges
    }
  )
}
