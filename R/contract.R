#' Contract groups of nodes in a spatial network
#'
#' Combine groups of nodes into a single node per group. The centroid such a
#' group will be used by default as new geometry of the contracted node. If
#' edges are spatially explicit, edge geometries are updated accordingly such
#' that the valid spatial network structure is preserved.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param groups A group index for each node in x.
#'
#' @param simplify Should the network be simplified after contraction? Defaults
#' to \code{TRUE}. This means that multiple edges and loop edges will be
#' removed. Multiple edges are introduced by contraction when there are several
#' connections between the same groups of nodes. Loop edges are introduced by
#' contraction when there are connections within a group. Note however that
#' setting this to \code{TRUE} also removes multiple edges and loop edges that
#' already existed before contraction.
#'
#' @param compute_centroids Should the new geometry of each contracted group of
#' nodes be the centroid of all group members? Defaults to \code{TRUE}. If set
#' to \code{FALSE}, the geometry of the first node in each group will be used
#' instead, which requires considerably less computing time.
#'
#' @param reconnect_edges Should the geometries of the edges be updated such
#' they match the new node geometries? Defaults to \code{TRUE}. Only set this
#' to \code{FALSE} if you know the node geometries did not change, otherwise
#' the valid spatial network structure is broken.
#'
#' @param attribute_summary How should the attributes of contracted nodes be
#' summarized? There are several options, see
#' \code{\link[igraph]{igraph-attribute-combination}} for details.
#'
#' @param store_original_ids For each group of contracted nodes, should
#' the indices of the original nodes be stored as an attribute of the new edge,
#' in a column named \code{.tidygraph_node_index}? This is in line with the
#' design principles of \code{tidygraph}. Defaults to \code{FALSE}.
#'
#' @param store_original_data For each group of contracted nodes, should
#' the data of the original nodes be stored as an attribute of the new edge, in
#' a column named \code{.orig_data}? This is in line with the design principles
#' of \code{tidygraph}. Defaults to \code{FALSE}.
#'
#' @returns The contracted network as object of class \code{\link{sfnetwork}}.
#'
#' @importFrom igraph contract delete_edges delete_vertex_attr is_directed
#' vertex_attr<- vertex_attr_names which_loop which_multiple
#' @importFrom sf st_as_sf st_centroid st_combine
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
#' @export
contract_nodes = function(x, groups, simplify = TRUE,
                          compute_centroids = TRUE, reconnect_edges = TRUE,
                          attribute_summary = "ignore",
                          store_original_ids = FALSE,
                          store_original_data = FALSE) {
  # Add index columns if not present.
  # These keep track of original node and edge indices.
  x = add_original_ids(x)
  # Extract nodes.
  nodes = nodes_as_sf(x)
  node_geomcol = attr(nodes, "sf_column")
  node_geom = nodes[[node_geomcol]]
  # If each group consists of only one node:
  # --> We do not need to do any contraction.
  if (! any(duplicated(groups))) {
    # Store original node data in a .orig_data column if requested.
    if (store_original_data) {
      x = add_original_node_data(x, nodes)
    }
    # Remove original indices if requested.
    if (! store_original_ids) {
      x = delete_vertex_attr(x, ".tidygraph_node_index")
    }
    # Return x without contraction.
    return(x)
  }
  ## ===========================
  # STEP I: CONTRACT THE NODES
  # # For this we simply rely on igraphs contract function
  ## ===========================
  # Update the attribute summary instructions.
  # In the summarise attributes only real attribute columns were referenced.
  # On top of those, we need to include:
  # --> The tidygraph node index column.
  if (! inherits(attribute_summary, "list")) {
    attribute_summary = list(attribute_summary)
  }
  attribute_summary[".tidygraph_node_index"] = "concat"
  # The geometries will be summarized at a later stage.
  # However igraph does not know the geometries are special.
  # We therefore temporarily remove the geometries before contracting.
  x_tmp = delete_vertex_attr(x, node_geomcol)
  # Contract with igraph::contract.
  x_new = as_tbl_graph(contract(x_tmp, groups, attribute_summary))
  ## =======================================
  # STEP II: SUMMARIZE THE NODE GEOMETRIES
  # Each contracted node should get a new geometry.
  ## =======================================
  # Extract the nodes from the contracted network.
  new_nodes = as_tibble(x_new, "nodes", focused = FALSE)
  # Add geometries to the new nodes.
  # Geometries of contracted nodes are a summary of the original group members.
  # Either the centroid or the geometry of the first member.
  if (compute_centroids) {
    centroid = function(i) if (length(i) > 1) st_centroid(st_combine(i)) else i
    grouped_geoms = split(node_geom, groups)
    names(grouped_geoms) = NULL
    new_node_geom = do.call("c", lapply(grouped_geoms, centroid))
  } else {
    new_node_geom = node_geom[!duplicated(groups)]
  }
  new_nodes[node_geomcol] = list(new_node_geom)
  new_nodes = st_as_sf(new_nodes, sf_column_name = node_geomcol)
  ## ============================================
  # STEP III: CONVERT BACK INTO A SPATIAL NETWORK
  # Now we have the geometries of the new nodes.
  # This means we can convert the contracted network into a sfnetwork again.
  # We copy original attributes of x to not lose them.
  ## ============================================
  # First we remove multiple edges and loop edges if this was requested.
  # Multiple edges occur when there are several connections between groups.
  # Loop edges occur when there are connections within groups.
  # Note however that original multiple and loop edges are also removed.
  if (simplify) {
    x_new = delete_edges(x_new, which(which_multiple(x_new) | which_loop(x_new)))
  }
  # Now add the spatially embedded nodes to the network.
  # And copy original attributes (including the sfnetwork class).
  node_data(x_new) = new_nodes
  x_new = x_new %preserve_all_attrs% x
  ## =======================================
  # STEP IV: RECONNECT THE EDGE GEOMETRIES
  # The geometries of the contracted nodes are updated.
  # This means the edge geometries of their incident edges also need an update.
  # Otherwise the valid spatial network structure is not preserved.
  ## =======================================
  if (reconnect_edges & has_explicit_edges(x)) {
    if (! is_directed(x)) {
      x_new = make_edges_follow_indices(x_new)
    }
    x_new = make_edges_valid(x_new)
  }
  ## ==============================================
  # STEP V: POST-PROCESS AND RETURN
  ## ==============================================
  # Store original data if requested.
  if (store_original_data) {
    x_new = add_original_node_data(x_new, nodes)
  }
  # Remove original indices if requested.
  if (! store_original_ids) {
    x_new = drop_original_ids(x_new)
  }
  x_new
}