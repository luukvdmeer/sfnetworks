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
    morphed_sfn = lapply(morphed, tbg_to_sfn)
    attributes(morphed_sfn) = attributes(morphed)
    structure(
      morphed,
      class = c("morphed_sfnetwork", class(morphed))
    )
  } else {
    morphed
  }
}

#' @importFrom igraph delete_edge_attr delete_vertex_attr edge_attr vertex_attr
#' edge_attr_names vertex_attr_names
#' @importFrom tidygraph unmorph
#' @export
unmorph.morphed_sfnetwork = function(.data, ...) {
  # Unmorphing needs special treatment for morphed sfnetworks when:
  # --> Features were merged and original data stored in a .orig_data column.
  # --> A new geometry column exists next to this .orig_data column.
  # This new geometry is a geometry describing the merged features.
  # When unmorphing the merged features get unmerged again.
  # Hence, the geometry column for the merged features should not be preserved.
  x_first = .data[[1]] # Extract the first element to run checks on.
  # If nodes were merged:
  # --> Remove the geometry column of the merged features before proceeding.
  n_idxs = vertex_attr(x_first, ".tidygraph_node_index")
  e_idxs = vertex_attr(x_first, ".tidygraph_edge_index")
  if (is.list(n_idxs) || is.list(e_idxs)) {
    geom_colname = node_geom_colname(attr(.data, ".orig_graph"))
    if (geom_colname %in% vertex_attr_names(x_first)) {
      attrs = attributes(.data)
      .data = lapply(.data, delete_vertex_attr, geom_colname)
      attributes(.data) = attrs
    }
  }
  # If edges were merged:
  # --> Remove the geometry column of the merged features before proceeding.
  n_idxs = edge_attr(x_first, ".tidygraph_node_index")
  e_idxs = edge_attr(x_first, ".tidygraph_edge_index")
  if (is.list(e_idxs) || is.list(n_idxs)) {
    geom_colname = edge_geom_colname(attr(.data, ".orig_graph"))
    if (!is.null(geom_colname) && geom_colname %in% edge_attr_names(x_first)) {
      attrs = attributes(.data)
      .data = lapply(.data, delete_edge_attr, geom_colname)
      attributes(.data) = attrs
    }
  }
  # Call tidygraphs unmorph.
  NextMethod(.data, ...)
}
