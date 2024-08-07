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

#' @importFrom cli cli_abort
#' @importFrom rlang enquo quo_text
#' @importFrom tidygraph as_tbl_graph morph
#' @export
morph.sfnetwork = function(.data, .f, ...) {
  # Morph using tidygraphs morphing functionality:
  # --> First try to morph the sfnetwork object directly.
  # --> If this gives errors, convert to tbl_graph and then morph.
  # --> If that also gives errors, return the first error found.
  morphed_data = tryCatch(
    NextMethod(),
    error = function(e1) {
      tryCatch(
        morph(as_tbl_graph(.data), .f, ...),
        error = function(e) {
          morpher = quo_text(enquo(.f))
          cli_abort(c(
            "Failed to morph the {.cls sfnetwork} object using {.fn {morpher}}.",
            "x" = "The following error occured: {e1}"
          ), call = call("morph.sfnetwork"))
        }
      )
    }
  )
  # If morphed data still consist of valid sfnetworks:
  # --> Convert the morphed_tbl_graph into a morphed_sfnetwork.
  # --> Otherwise, just return the morphed_tbl_graph.
  if (is_sfnetwork(morphed_data[[1]])) {
    structure(
      morphed_data,
      class = c("morphed_sfnetwork", class(morphed_data))
    )
  } else if (has_spatial_nodes(morphed_data[[1]])) {
    attrs = attributes(morphed_data)
    morphed_data = lapply(morphed_data, tbg_to_sfn)
    attributes(morphed_data) = attrs
    structure(
      morphed_data,
      class = c("morphed_sfnetwork", class(morphed_data))
    )
  } else {
    morphed_data
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
