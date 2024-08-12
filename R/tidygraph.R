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
morph.sfnetwork = function(.data, .f, ...) {
  # Morph using tidygraphs morphing functionality.
  morphed = NextMethod()
  # If morphed data still consist of valid sfnetworks:
  # --> Convert the morphed_tbl_graph into a morphed_sfnetwork.
  # --> Otherwise, just return the morphed_tbl_graph.
  if (is_sfnetwork(morphed[[1]])) {
    structure(
      morphed,
      class = c("morphed_sfnetwork", class(morphed))
    )
  } else if (has_spatial_nodes(morphed[[1]])) {
    structure(
      lapply(morphed, tbg_to_sfn) %preserve_morphed_attrs% morphed,
      class = c("morphed_sfnetwork", class(morphed))
    )
  } else {
    morphed
  }
}

#' @importFrom igraph edge_attr vertex_attr
#' @importFrom tibble as_tibble
#' @importFrom tidygraph unmorph
#' @export
unmorph.morphed_sfnetwork = function(.data, ...) {
  # Unmorphing needs additional preparation for morphed sfnetworks when:
  # --> Features were merged and original data stored in a .orig_data column.
  # --> In this case tidygraph attempts to bind columns of two tibbles.
  # --> The sticky geometry of sf creates problems in that process.
  # --> We can work around this by making sure .orig_data has no sf objects.
  if (! is.null(vertex_attr(.data[[1]], ".orig_data"))) {
    orig_data_to_tibble = function(x) {
      vertex_attr(x, ".orig_data") = as_tibble(vertex_attr(x, ".orig_data"))
      x
    }
    .data = lapply(.data, orig_data_to_tibble) %preserve_morphed_attrs% .data
  }
  if (! is.null(edge_attr(.data[[1]], ".orig_data"))) {
    orig_data_to_tibble = function(x) {
      edge_attr(x, ".orig_data") = as_tibble(edge_attr(x, ".orig_data"))
      x
    }
    .data = lapply(.data, orig_data_to_tibble) %preserve_morphed_attrs% .data
  }
  # Call tidygraphs unmorph.
  NextMethod(.data, ...)
}
