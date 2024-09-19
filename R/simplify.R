#' Simplify a spatial network
#'
#' Construct a simple version of the network. A simple network is defined as a
#' network without loop edges and multiple edges. A loop edge is an edge that
#' starts and ends at the same node. Multiple edges are different edges between
#' the same node pair.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param remove_multiple Should multiple edges be merged into one. Defaults
#' to \code{TRUE}.
#'
#' @param remove_loops Should loop edges be removed. Defaults to \code{TRUE}.
#'
#' @param summarise_attributes How should the attributes of merged multiple
#' edges be summarized? There are several options, see
#' \code{\link[igraph]{igraph-attribute-combination}} for details.
#'
#' @param store_original_ids For each group of merged multiple edges, should
#' the indices of the original edges be stored as an attribute of the new edge,
#' in a column named \code{.tidygraph_edge_index}? This is in line with the
#' design principles of \code{tidygraph}. Defaults to \code{FALSE}.
#'
#' @param store_original_data For each group of merged multiple edges, should
#' the data of the original edges be stored as an attribute of the new edge, in
#' a column named \code{.orig_data}? This is in line with the design principles
#' of \code{tidygraph}. Defaults to \code{FALSE}.
#'
#' @note When merging groups of multiple edges into a single edge, the geometry
#' of the first edge in each group is preserved. The order of the edges can be
#' influenced by calling \code{\link[dplyr]{arrange}} before simplifying.
#'
#' @returns The simple network as object of class \code{\link{sfnetwork}}.
#'
#' @importFrom igraph simplify
#' @importFrom sf st_as_sf st_crs st_crs<- st_precision st_precision<- st_sfc
#' @export
simplify_network = function(x, remove_multiple = TRUE, remove_loops = TRUE,
                            summarise_attributes = "first",
                            store_original_ids = FALSE,
                            store_original_data = FALSE) {
  # Add a index column if not present.
  if (! ".tidygraph_edge_index" %in% edge_attr_names(x)) {
    edge_attr(x, ".tidygraph_edge_index") = seq_len(1:n_edges(x))
  }
  ## ==================================================
  # STEP I: REMOVE LOOP EDGES AND MERGE MULTIPLE EDGES
  # For this we simply rely on igraphs simplify function
  ## ==================================================
  # Update the attribute summary instructions.
  # In the summarise attributes only real attribute columns were referenced.
  # On top of those, we need to include:
  # --> The geometry column, if present.
  # --> The tidygraph edge index column.
  if (! inherits(summarise_attributes, "list")) {
    summarise_attributes = list(summarise_attributes)
  }
  edge_geomcol = edge_geom_colname(x)
  if (! is.null(edge_geomcol)) summarise_attributes[edge_geomcol] = "first"
  summarise_attributes[".tidygraph_edge_index"] = "concat"
  # Simplify the network.
  x_new = simplify(
    x,
    remove.multiple = remove_multiple,
    remove.loops = remove_loops,
    edge.attr.comb = summarise_attributes
  ) %preserve_all_attrs% x
  ## ====================================
  # STEP II: RECONSTRUCT EDGE GEOMETRIES
  # Igraph does not know about geometry list columns:
  # --> Summarizing them results in a list of sfg objects.
  # --> We should reconstruct the sfc geometry list column out of that.
  ## ====================================
  if (! is.null(edge_geomcol)) {
    new_edges = edges_as_regular_tibble(x_new)
    new_edges[edge_geomcol] = list(st_sfc(new_edges[[edge_geomcol]]))
    new_edges = st_as_sf(new_edges, sf_column_name = edge_geomcol)
    st_crs(new_edges) = st_crs(x)
    st_precision(new_edges) = st_precision(x)
    st_agr(new_edges) = edge_agr(x)
    edge_data(x_new) = new_edges
  }
  ## ==================================
  # STEP III: POST-PROCESS AND RETURN
  ## ==================================
  # Store original data if requested.
  if (store_original_data) {
    x_new = add_original_edge_data(x_new, orig = edge_data(x, focused = FALSE))
  }
  # Remove original indices if requested.
  if (! store_original_ids) {
    x_new = delete_edge_attr(x_new, ".tidygraph_edge_index")
  }
  x_new
}