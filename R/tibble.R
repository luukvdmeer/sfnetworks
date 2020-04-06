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

node_tibble = function(x) {
  as_sf(x, active = "nodes")
}

edge_tibble = function(x) {
  tryCatch(
    expr = {
      as_sf(x, active = "edges")
    },
    error = function(e) {
      as_tibble(as_tbl_graph(x), active = "edges")
    }
  )
}