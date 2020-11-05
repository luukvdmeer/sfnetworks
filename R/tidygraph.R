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

#' @importFrom igraph delete_edge_attr delete_vertex_attr edge_attr vertex_attr
#' edge_attr_names vertex_attr_names
#' @importFrom tidygraph unmorph
#' @export
unmorph.morphed_sfnetwork = function(.data, ...) {
  # Extract:
  # --> First graph in the morphed object.
  # --> Attributes of the morphed object.
  # --> Original graph before morphing.
  x1 = .data[[1]]
  xa = attributes(.data)
  xo = xa$.orig_graph
  # If some nodes and/or edges where merged during morphing:
  # --> This stores original node and/or edge indices in a list column.
  # --> However, this also keeps a geometry column for the combined features.
  # --> If a regular tidygraph morpher was used as a list of sfc objects.
  # --> If spatial morpher was used as single sfc object.
  # --> This gives problems when unmorphing.
  # --> Therefore, we need to remove these combined geometries first.
  # --> Original geometries are stored elsewhere and are not affected.
  n_idx = ".tidygraph_node_index"
  e_idx = ".tidygraph_edge_index"
  n_nms = vertex_attr_names(x1)
  e_nms = edge_attr_names(x1)
  if (n_idx %in% n_nms && class(vertex_attr(x1, n_idx)) == "list") {
    geom_col = node_geom_colname(xo)
    .data = lapply(.data, function(x) delete_vertex_attr(x, geom_col))
    attributes(.data) = xa
  }
  if (has_spatially_explicit_edges(xo)) {
    if (e_idx %in% e_nms && class(edge_attr(x1, e_idx)) == "list") {
      geom_col = edge_geom_colname(xo)
      .data = lapply(.data, function(x) delete_edge_attr(x, geom_col))
      attributes(.data) = xa
    }
  }
  NextMethod(.data, ...)
}