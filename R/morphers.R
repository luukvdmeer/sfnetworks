#' Morph spatial networks into a different structure
#'
#' Spatial morphers form spatial add-ons to the set of
#' \code{\link[tidygraph]{morphers}} provided by \code{tidygraph}. These
#' functions are not meant to be called directly. They should either be passed
#' into \code{\link[tidygraph]{morph}} to create a temporary alternative
#' representation of the input network. Such an alternative representation is a
#' list of one or more network objects. Single elements of that list can be
#' extracted directly as a new network by passing the morpher to
#' \code{\link[tidygraph]{convert}} instead, to make the changes lasting rather
#' than temporary. Alternatively, if the morphed state contains multiple
#' elements, all of them can be extracted together inside a
#' \code{\link[tibble]{tbl_df}} by passing the morpher to
#' \code{\link[tidygraph]{crystallise}}.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments to be passed on to other functions. See the description
#' of each morpher for details.
#'
#' @param store_original_data Whenever multiple features (i.e. nodes and/or
#' edges) are merged into a single feature during morphing, should the data of
#' the original features be stored as an attribute of the new feature, in a
#' column named \code{.orig_data}. This is in line with the design principles
#' of \code{tidygraph}. Defaults to \code{FALSE}.
#'
#' @param summarise_attributes Whenever multiple features (i.e. nodes and/or
#' edges) are merged into a single feature during morphing, how should their
#' attributes be combined? Several options are possible, see
#' \code{\link[igraph]{igraph-attribute-combination}} for details.
#'
#' @return Either a \code{morphed_sfnetwork}, which is a list of one or more
#' \code{\link{sfnetwork}} objects, or a \code{morphed_tbl_graph}, which is a
#' list of one or more \code{\link[tidygraph]{tbl_graph}} objects. See the
#' description of each morpher for details.
#'
#' @details It also possible to create your own morphers. See the documentation
#' of \code{\link[tidygraph]{morph}} for the requirements for custom morphers.
#'
#' @seealso The vignette on
#' \href{https://luukvdmeer.github.io/sfnetworks/articles/sfn05_morphers.html}{spatial morphers}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel, directed = FALSE) %>%
#'   st_transform(3035)
#'
#' # Temporary changes with morph and unmorph.
#' net %>%
#'  activate("edges") %>%
#'  mutate(weight = edge_length()) %>%
#'  morph(to_spatial_shortest_paths, from = 1, to = 10) %>%
#'  mutate(in_paths = TRUE) %>%
#'  unmorph()
#'
#' # Lasting changes with convert.
#' net %>%
#'  activate("edges") %>%
#'  mutate(weight = edge_length()) %>%
#'  convert(to_spatial_shortest_paths, from = 1, to = 10)
#'
#' @name spatial_morphers
NULL

#' @describeIn spatial_morphers Combine groups of nodes into a single node per
#' group. \code{...} is forwarded to \code{\link[dplyr]{group_by}} to
#' create the groups. The centroid of the group of nodes will be used by
#' default as geometry of the contracted node. If edges are spatially explicit,
#' edge geometries are updated accordingly such that the valid spatial network
#' structure is preserved. Returns a \code{morphed_sfnetwork} containing a
#' single element of class \code{\link{sfnetwork}}.
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
#' @importFrom dplyr group_by group_indices group_size
#' @importFrom igraph contract delete_edges delete_vertex_attr is_directed
#' which_loop which_multiple
#' @importFrom sf st_as_sf st_centroid st_combine st_drop_geometry st_geometry
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
#' @export
to_spatial_contracted = function(x, ..., simplify = TRUE,
                                 compute_centroids = TRUE,
                                 summarise_attributes = "ignore",
                                 store_original_data = FALSE) {
  # Retrieve nodes from the network.
  # Extract specific information from them.
  nodes = nodes_as_sf(x)
  node_data = st_drop_geometry(nodes)
  node_geom = st_geometry(nodes)
  node_geomcol = attr(nodes, "sf_column")
  ## =======================
  # STEP I: GROUP THE NODES
  # Group the nodes table by forwarding ... to dplyr::group_by.
  # Each group of nodes will later be contracted into a single node.
  ## =======================
  node_data = group_by(node_data, ...)
  group_ids = group_indices(node_data)
  # If no group contains more than one node simply return x.
  if (all(group_size(node_data) == 1)) return(list(contracted = x))
  ## ===========================
  # STEP II: CONTRACT THE NODES
  # Contract the nodes in the network using igraph::contract.
  # Use the extracted group indices as mapping.
  ## ===========================
  # Update the attribute summary instructions.
  # During morphing tidygraph adds the tidygraph node index column.
  # Since it is added internally it is not referenced in summarise_attributes.
  # We need to include it manually.
  # They should be concatenated into a vector.
  if (! inherits(summarise_attributes, "list")) {
    summarise_attributes = list(summarise_attributes)
  }
  summarise_attributes[".tidygraph_node_index"] = "concat"
  # The geometries will be summarized at a later stage.
  # However igraph does not know the geometries are special.
  # We therefore temporarily remove the geometries before contracting.
  x_tmp = delete_vertex_attr(x, node_geomcol)
  # Contract with igraph::contract.
  x_new = as_tbl_graph(contract(x_tmp, group_ids, summarise_attributes))
  ## ======================================================
  # STEP III: UPDATE THE NODE DATA OF THE CONTRACTED NETWORK
  # Add the following information to the nodes table:
  # --> The geometries of the new nodes.
  # --> If requested the original node data in tibble format.
  ## ======================================================
  # Extract the nodes from the contracted network.
  new_nodes = as_tibble(x_new, "nodes", focused = FALSE)
  # Add geometries to the new nodes.
  # Geometries of contracted nodes are a summary of the original group members.
  # Either the centroid or the geometry of the first member.
  if (compute_centroids) {
    centroid = function(i) if (length(i) > 1) st_centroid(st_combine(i)) else i
    grouped_geoms = split(node_geom, group_ids)
    new_node_geom = do.call("c", lapply(grouped_geoms, centroid))
  } else {
    new_node_geom = node_geom[!duplicated(group_ids)]
  }
  new_nodes[node_geomcol] = list(new_node_geom)
  # If requested, store original node data in a .orig_data column.
  if (store_original_data) {
    drop_index = function(i) { i$.tidygraph_node_index = NULL; i }
    grouped_data = split(nodes, group_ids)
    new_nodes$.orig_data = lapply(grouped_data, drop_index)
  }
  # Update the nodes table of the contracted network.
  new_nodes = st_as_sf(new_nodes, sf_column_name = node_geomcol)
  node_data(x_new) = new_nodes
  # Convert to a sfnetwork.
  x_new = tbg_to_sfn(x_new)
  ## ===============================================================
  # STEP IV: RECONNECT THE EDGE GEOMETRIES OF THE CONTRACTED NETWORK
  # The geometries of the contracted nodes are updated.
  # This means the edge geometries of their incident edges also need an update.
  # Otherwise the valid spatial network structure is not preserved.
  ## ===============================================================
  # First we will remove multiple edges and loop edges if this was requested.
  # Multiple edges occur when there are several connections between groups.
  # Loop edges occur when there are connections within groups.
  # Note however that original multiple and loop edges are also removed.
  if (simplify) {
    x_new = delete_edges(x_new, which(which_multiple(x_new)))
    x_new = delete_edges(x_new, which(which_loop(x_new)))
    x_new = x_new %preserve_all_attrs% x_new
  }
  # Secondly we will update the geometries of the remaining affected edges.
  # The boundaries of the edges will be replaced by the new node geometries.
  if (has_explicit_edges(x)) {
    if (! is_directed(x)) {
      x_new = make_edges_follow_indices(x_new)
    }
    x_new = make_edges_valid(x_new)
  }
  # Return in a list.
  list(
    contracted = x_new %preserve_network_attrs% x
  )
}

#' @describeIn spatial_morphers Make a network directed in the direction given
#' by the linestring geometries of the edges. Differs from
#' \code{\link[tidygraph]{to_directed}}, which makes a network directed based
#' on the node indices given in the \code{from} and \code{to} columns. In
#' undirected networks these indices may not correspond with the endpoints of
#' the linestring geometries. Returns a \code{morphed_sfnetwork} containing a
#' single element of class \code{\link{sfnetwork}}. This morpher requires edges
#' to be spatially explicit. If not, use \code{\link[tidygraph]{to_directed}}.
#' @importFrom igraph is_directed
#' @export
to_spatial_directed = function(x) {
  if (is_directed(x)) return (x)
  # Retrieve the nodes and edges from the network.
  nodes = nodes_as_sf(x)
  edges = edges_as_sf(x)
  # Get the node indices that correspond to the geometries of the edge bounds.
  idxs = edge_boundary_ids(x, matrix = TRUE)
  from = idxs[, 1]
  to = idxs[, 2]
  # Update the from and to columns of the edges such that:
  # --> The from node matches the startpoint of the edge.
  # --> The to node matches the endpoint of the edge.
  edges$from = from
  edges$to = to
  # Recreate the network as a directed one.
  x_new = sfnetwork_(nodes, edges, directed = TRUE)
  # Return in a list.
  list(
    directed = x_new %preserve_network_attrs% x
  )
}

#' @describeIn spatial_morphers Create linestring geometries between source
#' and target nodes of edges. If the edges data can be directly converted to
#' an object of class \code{\link[sf]{sf}} using \code{\link[sf]{st_as_sf}},
#' extra arguments can be provided as \code{...} and will be forwarded to
#' \code{\link[sf]{st_as_sf}} internally. Otherwise, straight lines will be
#' drawn between the source and target node of each edge. Returns a
#' \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}.
#' @importFrom rlang dots_n
#' @importFrom sf st_as_sf
#' @export
to_spatial_explicit = function(x, ...) {
  # Workflow:
  # --> If ... is given, convert edges to sf by forwarding ... to st_as_sf.
  # --> If ... is not given, draw straight lines from source to target nodes.
  if (dots_n() > 0) {
    edges = edge_data(x, focused = FALSE)
    new_edges = st_as_sf(edges, ...)
    x_new = x
    edge_data(x_new) = new_edges
  } else {
    x_new = make_edges_explicit(x)
  }
  # Return in a list.
  list(
    explicit = x_new
  )
}

#' @describeIn spatial_morphers Limit a network to the spatial neighborhood of
#' a specific node. \code{...} is forwarded to \code{\link{st_network_cost}} to
#' compute the travel cost from the source node to all other nodes in the
#' network. Returns a \code{morphed_sfnetwork} containing a single element of
#' class \code{\link{sfnetwork}}.
#'
#' @param node The node for which the neighborhood will be calculated.
#' Evaluated by \code{\link{evaluate_node_query}}. When multiple nodes are
#' given, only the first one is used.
#'
#' @param threshold The threshold distance to be used. Only nodes within the
#' threshold distance from the reference node will be included in the
#' neighborhood. Should be a numeric value in the same units as the weight
#' values used for the cost matrix computation. Alternatively, units can be
#' specified explicitly by providing a \code{\link[units]{units}} object.
#'
#' @importFrom igraph induced_subgraph
#' @importFrom methods hasArg
#' @importFrom units as_units deparse_unit
#' @export
to_spatial_neighborhood = function(x, node, threshold, ...) {
  # Compute the cost matrix from the source node.
  # By calling st_network_cost with the given arguments.
  if (hasArg("from")) {
    # Deprecate the former "from" argument specifying routing direction.
    deprecate_from()
    if (isFALSE(list(...)$from)) {
      costs = st_network_cost(x, from = node, direction = "in", ...)
    } else {
      costs = st_network_cost(x, from = node, ...)
    }
  } else {
    costs = st_network_cost(x, from = node, ...)
  }
  # Use the given threshold to define which nodes are in the neighborhood.
  if (inherits(costs, "units") && ! inherits(threshold, "units")) {
    threshold = as_units(threshold, deparse_unit(costs))
  }
  in_neighborhood = costs[1, ] <= threshold
  # Subset the network to keep only the nodes in the neighborhood.
  x_new = induced_subgraph(x, in_neighborhood)
  # Return in a list.
  list(
    neighborhood = x_new %preserve_all_attrs% x
  )
}

#' @describeIn spatial_morphers Limit a network to those nodes and edges that
#' are part of the shortest path between two nodes. \code{...} is evaluated in
#' the same manner as \code{\link{st_network_paths}} with
#' \code{type = 'shortest'}. Returns a \code{morphed_sfnetwork} that may
#' contain multiple elements of class \code{\link{sfnetwork}}, depending on
#' the number of requested paths. When unmorphing only the first instance of
#' both the node and edge data will be used, as the the same node and/or edge
#' can be present in multiple paths.
#' @importFrom igraph delete_edges delete_vertices edge_attr vertex_attr
#' @export
to_spatial_shortest_paths = function(x, ...) {
  # Call st_network_paths with the given arguments.
  if (hasArg("type")) raise_unsupported_arg("type")
  paths = st_network_paths(
    x,
    ...,
    type = "shortest",
    use_names = FALSE,
    return_cost = FALSE,
    return_geometry = FALSE
  )
  # Retrieve original node and edge indices from the network.
  orig_node_idxs = vertex_attr(x, ".tidygraph_node_index")
  orig_edge_idxs = edge_attr(x, ".tidygraph_edge_index")
  # Subset the network for each computed shortest path.
  get_single_path = function(i) {
    edge_idxs = as.integer(paths$edges[[i]])
    node_idxs = as.integer(paths$nodes[[i]])
    x_new = delete_edges(x, orig_edge_idxs[-edge_idxs])
    x_new = delete_vertices(x_new, orig_node_idxs[-node_idxs])
    x_new %preserve_all_attrs% x
  }
  lapply(seq_len(nrow(paths)), get_single_path)
}

#' @describeIn spatial_morphers Remove loop edges and/or merges multiple edges
#' into a single edge. Multiple edges are edges that have the same source and
#' target nodes (in directed networks) or edges that are incident to the same
#' nodes (in undirected networks). When merging them into a single edge, the
#' geometry of the first edge is preserved. The order of the edges can be
#' influenced by calling \code{\link[dplyr]{arrange}} before simplifying.
#' Returns a \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}.
#'
#' @param remove_multiple Should multiple edges be merged into one. Defaults
#' to \code{TRUE}.
#'
#' @param remove_loops Should loop edges be removed. Defaults to \code{TRUE}.
#'
#' @importFrom igraph simplify
#' @importFrom sf st_as_sf st_crs st_crs<- st_precision st_precision<- st_sfc
#' @export
to_spatial_simple = function(x, remove_multiple = TRUE, remove_loops = TRUE,
                             summarise_attributes = "first",
                             store_original_data = FALSE) {
  # Update the attribute summary instructions.
  # In the summarise attributes only real attribute columns were referenced.
  # On top of those, we need to include:
  # --> The geometry column, if present.
  # --> The tidygraph edge index column added by tidygraph::morph.
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
  ) %preserve_network_attrs% x
  # Igraph does not know about geometry list columns.
  # Summarizing them results in a list of sfg objects.
  # We should reconstruct the sfc geometry list column out of that.
  if (! is.null(edge_geomcol)) {
    new_edges = edges_as_regular_tibble(x_new)
    new_edges[edge_geomcol] = list(st_sfc(new_edges[[edge_geomcol]]))
    new_edges = st_as_sf(new_edges, sf_column_name = edge_geomcol)
    st_crs(new_edges) = st_crs(x)
    st_precision(new_edges) = st_precision(x)
    edge_data(x_new) = new_edges
  }
  # If requested, original edge data should be stored in a .orig_data column.
  if (store_original_data) {
    edges = edge_data(x, focused = FALSE)
    edges$.tidygraph_edge_index = NULL
    new_edges = edge_data(x, focused = FALSE_new)
    copy_data = function(i) edges[i, , drop = FALSE]
    new_edges$.orig_data = lapply(new_edges$.tidygraph_edge_index, copy_data)
    edge_data(x_new) = new_edges
  }
  # Return in a list.
  list(
    simple = x_new
  )
}

#' @describeIn spatial_morphers Construct a smoothed version of the network by
#' iteratively removing pseudo nodes, while preserving the connectivity of the
#' network. In the case of directed networks, pseudo nodes are those nodes that
#' have only one incoming and one outgoing edge. In undirected networks, pseudo
#' nodes are those nodes that have two incident edges. Equality of attribute
#' values among the two edges can be defined as an additional requirement by
#' setting the \code{require_equal} parameter. Connectivity of the
#' network is preserved by concatenating the incident edges of each removed
#' pseudo node. Returns a \code{morphed_sfnetwork} containing a single element
#' of class \code{\link{sfnetwork}}.
#'
#' @param protect Nodes to be protected from being removed, no matter if they
#' are a pseudo node or not. Evaluated by \code{\link{evaluate_node_query}}.
#' Defaults to \code{NULL}, meaning that none of the nodes is protected.
#'
#' @param require_equal Should nodes only be removed when the attribute values
#' of their incident edges are equal? Defaults to \code{FALSE}. If \code{TRUE},
#' only pseudo nodes that have incident edges with equal attribute values are
#' removed. May also be given as a vector of attribute names. In that case only
#' those attributes are checked for equality. Equality tests are evaluated
#' using the \code{==} operator.
#'
#' @importFrom cli cli_abort
#' @importFrom igraph adjacent_vertices decompose degree delete_vertices
#' edge_attr get.edge.ids igraph_opt igraph_options
#' incident_edges induced_subgraph is_directed vertex_attr
#' @importFrom sf st_as_sf st_cast st_combine st_crs st_equals st_is
#' st_line_merge
#' @export
to_spatial_smooth = function(x,
                             protect = NULL,
                             summarise_attributes = "ignore",
                             require_equal = FALSE,
                             store_original_data = FALSE) {
  # Change default igraph options.
  # This prevents igraph returns node or edge indices as formatted sequences.
  # We only need the "raw" integer indices.
  # Changing this option improves performance especially on large networks.
  default_igraph_opt = igraph_opt("return.vs.es")
  igraph_options(return.vs.es = FALSE)
  on.exit(igraph_options(return.vs.es = default_igraph_opt))
  # Retrieve nodes and edges from the network.
  nodes = nodes_as_sf(x)
  edges = edge_data(x, focused = FALSE)
  # For later use:
  # --> Check if x is directed.
  # --> Check if x has spatially explicit edges.
  # --> Retrieve the name of the geometry column of the edges in x.
  directed = is_directed(x)
  explicit_edges = is_sf(edges)
  edge_geomcol = attr(edges, "sf_column")
  ## ==========================
  # STEP I: DETECT PSEUDO NODES
  # The first step is to detect which nodes in x are pseudo nodes.
  # In directed networks, we define a pseudo node as follows:
  # --> A node with only one incoming and one outgoing edge.
  # In undirected networks, we define a pseudo node as follows:
  # --> A node with only two connections.
  ## ==========================
  if (directed) {
    pseudo = degree(x, mode = "in") == 1 & degree(x, mode = "out") == 1
  } else {
    pseudo = degree(x) == 2
  }
  if (! any(pseudo)) return (x)
  ## ===========================
  # STEP II: FILTER PSEUDO NODES
  # Users can define additional requirements for a node to be smoothed:
  # --> It should not be listed in the provided set of protected nodes.
  # --> Its incident edges should have equal values for some attributes.
  # In these cases we need to filter the set of detected pseudo nodes.
  ## ===========================
  # Detected pseudo nodes that are protected should be filtered out.
  if (! is.null(protect)) {
    # Evaluate the given protected nodes query.
    protect = evaluate_node_query(x, protect)
    # Mark all protected nodes as not being a pseudo node.
    pseudo[protect] = FALSE
    if (! any(pseudo)) return (x)
  }
  # Check for equality of certain attributes between incident edges.
  # Detected pseudo nodes that fail this check should be filtered out.
  if (! isFALSE(require_equal)) {
    # If require_equal is TRUE all attributes will be checked for equality.
    # In other cases only a subset of attributes will be checked.
    if (isTRUE(require_equal)) {
      require_equal = edge_colnames(x, geom = FALSE)
    } else {
      # Check if all given attributes exist in the edges table of x.
      attr_exists = require_equal %in% edge_colnames(x, geom = FALSE)
      if (! all(attr_exists)) {
        unknown_attrs = paste(require_equal[!attr_exists], collapse = ", ")
        cli_abort(c(
          "Failed to check for edge attribute equality.",
          "x" = "The following edge attributes were not found: {unknown_attrs}"
        ))
      }
    }
    # Get the node indices of the detected pseudo nodes.
    pseudo_idxs = which(pseudo)
    # Get the edge indices of the incident edges of each pseudo node.
    # Combine them into a single numerical vector.
    # Note the + 1 since incident_edges returns indices starting from 0.
    incident_idxs = incident_edges(x, pseudo_idxs, mode = "all")
    incident_idxs = do.call("c", incident_idxs) + 1
    # Define for each of the incident edges if they are incoming or outgoing.
    # In undirected networks this can be read instead as "first or second".
    is_in = seq(1, 2 * length(pseudo_idxs), by = 2)
    is_out = seq(2, 2 * length(pseudo_idxs), by = 2)
    # Obtain the attributes to be checked for each of the incident edges.
    incident_attrs = edge_attr(x, require_equal, incident_idxs)
    # For each of these attributes:
    # --> Check if its value is equal for both incident edges of a pseudo node.
    check_equality = function(A) {
      # Check equality for each pseudo node.
      # NOTE:
      # --> Operator == is used because element-wise comparisons are needed.
      # --> Not sure if this approach works with identical() or all.equal().
      are_equal = A[is_in] == A[is_out]
      # If one of the two values is NA or NaN:
      # --> The result of the element-wise comparison is always NA.
      # --> This means the two elements are certainly not equal.
      # --> Hence the result of this comparison can be set to FALSE.
      are_equal[is.na(are_equal)] = FALSE
      are_equal
    }
    tests = lapply(incident_attrs, check_equality)
    # If one or more equality tests failed for a detected pseudo node:
    # --> Mark this pseudo node as FALSE, i.e. not being a pseudo node.
    failed = rowSums(do.call("cbind", tests)) != length(require_equal)
    pseudo[pseudo_idxs[failed]] = FALSE
    if (! any(pseudo)) return (x)
  }
  ## ====================================
  # STEP II: INITIALIZE REPLACEMENT EDGES
  # When removing pseudo nodes their incident edges get removed to.
  # To preserve the network connectivity we need to:
  # --> Find the two adjacent nodes of a pseudo node.
  # --> Connect these by merging the incident edges of the pseudo node.
  # An adjacent node of a pseudo node can also be another pseudo node.
  # Instead of processing each pseudo node on its own, we will:
  # --> Find connected sets of pseudo nodes.
  # --> Find the adjacent non-pseudo nodes (junction or pendant) to that set.
  # --> Connect them by merging the edges in the set plus its incident edges.
  ## ====================================
  # Subset x to only contain pseudo nodes and the edges between them.
  # Decompose this subgraph to find connected sets of pseudo nodes.
  pseudo_sets = decompose(induced_subgraph(x, pseudo))
  # For each set of connected pseudo nodes:
  # --> Find the indices of the adjacent nodes.
  # --> Find the indices of the edges that need to be merged.
  # The workflow for this is different for directed and undirected networks.
  if (directed) {
    initialize_replacement_edge = function(S) {
      # Retrieve the original node indices of the pseudo nodes in this set.
      # Retrieve the original edge indices of the edges that connect them.
      N = vertex_attr(S, ".tidygraph_node_index")
      E = edge_attr(S, ".tidygraph_edge_index")
      # Find the following:
      # --> The index of the pseudo node where an edge comes into the set.
      # --> The index of the pseudo node where an edge goes out of the set.
      n_i = N[degree(S, mode = "in") == 0]
      n_o = N[degree(S, mode = "out") == 0]
      # If these nodes do not exists:
      # --> We are dealing with a loop of connected pseudo nodes.
      # --> The loop is by definition not connected to the rest of the network.
      # --> Hence, there is no need to create a new edge.
      # --> Therefore we should not return a path.
      if (length(n_i) == 0) return (NULL)
      # Find the following:
      # --> The index of the edge that comes in to the pseudo node set.
      # --> The index of the non-pseudo node at the other end of that edge.
      # We'll call this the source node and source edge of the set.
      # Note the + 1 since adjacent_vertices returns indices starting from 0.
      source_node = adjacent_vertices(x, n_i, mode = "in")[[1]] + 1
      source_edge = get.edge.ids(x, c(source_node, n_i))
      # Find the following:
      # --> The index of the edge that goes out of the pseudo node set.
      # --> The index of the non-pseudo node at the other end of that edge.
      # We'll call this the sink node and sink edge of the set.
      # Note the + 1 since adjacent_vertices returns indices starting from 0.
      sink_node = adjacent_vertices(x, n_o, mode = "out")[[1]] + 1
      sink_edge = get.edge.ids(x, c(n_o, sink_node))
      # List indices of all edges that will be merged into the replacement edge.
      edge_idxs = c(source_edge, E, sink_edge)
      # Return all retrieved information in a list.
      list(
        from = as.integer(source_node),
        to = as.integer(sink_node),
        .tidygraph_edge_index = as.integer(edge_idxs)
      )
    }
  } else {
    initialize_replacement_edge = function(S) {
      # Retrieve the original node indices of the pseudo nodes in this set.
      # Retrieve the original edge indices of the edges that connect them.
      N = vertex_attr(S, ".tidygraph_node_index")
      E = edge_attr(S, ".tidygraph_edge_index")
      # Find the following:
      # --> The two adjacent non-pseudo nodes to the set.
      # --> The edges that connect these nodes to the set.
      # We'll call these the adjacent nodes and incident edges of the set.
      # --> The adjacent node with the lowest index will be the source node.
      # --> The adjacent node with the higest index will be the sink node.
      if (length(N) == 1) {
        # When we have a single pseudo node that forms a set:
        # --> It will be adjacent to both adjacent nodes of the set.
        # Note the + 1 since adjacent_vertices returns indices starting from 0.
        adjacent = adjacent_vertices(x, N)[[1]] + 1
        if (length(adjacent) == 1) {
          # If there is only one adjacent node to the pseudo node:
          # --> The two adjacent nodes of the set are the same node.
          # --> We only have to query for incident edges of the set once.
          incident = get.edge.ids(x, c(adjacent, N))
          source_node = adjacent
          source_edge = incident[1]
          sink_node = adjacent
          sink_edge = incident[2]
        } else {
          # If there are two adjacent nodes to the pseudo node:
          # --> The one with the lowest index will be source node.
          # --> The one with the highest index will be sink node.
          source_node = min(adjacent)
          source_edge = get.edge.ids(x, c(source_node, N))
          sink_node = max(adjacent)
          sink_edge = get.edge.ids(x, c(N, sink_node))
        }
      } else {
        # When we have a set of multiple pseudo nodes:
        # --> There are two pseudo nodes that form the boundary of the set.
        # --> These are the ones connected to only one other pseudo node.
        N_b = N[degree(S) == 1]
        # If these boundaries do not exist:
        # --> We are dealing with a loop of connected pseudo nodes.
        # --> The loop is by definition not connected to the rest of the network.
        # --> Hence, there is no need to create a new edge.
        # --> Therefore we should not return a path.
        if (length(N_b) == 0) return (NULL)
        # Find the adjacent nodes of the set.
        # These are the adjacent non-pseudo nodes to the boundaries of the set.
        # We find them iteratively for the two boundary nodes of the set:
        # --> A boundary connects to one pseudo node and one non-pseudo node.
        # --> The non-pseudo node is the one not present in the pseudo set.
        # Note the + 1 since adjacent_vertices returns indices starting from 0.
        get_set_neighbour = function(n) {
          all = adjacent_vertices(x, n)[[1]] + 1
          all[!(all %in% N)]
        }
        adjacent = do.call("c", lapply(N_b, get_set_neighbour))
        # The adjacent node with the lowest index will be source node.
        # The adjacent node with the highest index will be sink node.
        N_b = N_b[order(adjacent)]
        source_node = min(adjacent)
        source_edge = get.edge.ids(x, c(source_node, N_b[1]))
        sink_node = max(adjacent)
        sink_edge = get.edge.ids(x, c(N_b[2], sink_node))
      }
      # List indices of all edges that will be merged into the replacement edge.
      edge_idxs = c(source_edge, E, sink_edge)
      # Return all retrieved information in a list.
      list(
        from = as.integer(source_node),
        to = as.integer(sink_node),
        .tidygraph_edge_index = as.integer(edge_idxs)
      )
    }
  }
  new_idxs = lapply(pseudo_sets, initialize_replacement_edge)
  new_idxs = new_idxs[lengths(new_idxs) != 0] # Remove NULLs.
  ## ===================================
  # STEP III: SUMMARISE EDGE ATTRIBUTES
  # Each replacement edge replaces multiple original edges.
  # Their attributes should all be summarised in a single value.
  # The summary techniques to be used are given as summarise_attributes.
  ## ===================================
  # Obtain the attribute values of all original edges in the network.
  # These should not include the geometries and original edge indices.
  exclude = c(".tidygraph_edge_index", edge_geomcol)
  edge_attrs = edge_attr(x)
  edge_attrs = edge_attrs[!(names(edge_attrs) %in% exclude)]
  # For each replacement edge:
  # --> Summarise the attributes of the edges it replaces into single values.
  merge_attrs = function(E) {
    orig_edges = E$.tidygraph_edge_index
    orig_attrs = lapply(edge_attrs, `[`, orig_edges)
    apply_summary_function = function(i) {
      # Store return value in a list.
      # This prevents automatic type promotion when rowbinding later on.
      list(get_summary_function(i, summarise_attributes)(orig_attrs[[i]]))
    }
    new_attrs = lapply(names(orig_attrs), apply_summary_function)
    names(new_attrs) = names(orig_attrs)
    new_attrs
  }
  new_attrs = lapply(new_idxs, merge_attrs)
  ## ===================================
  # STEP VI: CONCATENATE EDGE GEOMETRIES
  # If the edges to be replaced have geometries:
  # --> These geometries have to be concatenated into a single new geometry.
  # --> The new geometry should go from the defined source to sink node.
  ## ===================================
  if (explicit_edges) {
    # Obtain geometries of all original edges and nodes in the network.
    edge_geoms = st_geometry(edges)
    node_geoms = st_geometry(nodes)
    # For each replacement edge:
    # --> Merge geometries of the edges it replaces into a single geometry.
    merge_geoms = function(E) {
      orig_edges = E$.tidygraph_edge_index
      orig_geoms = edge_geoms[orig_edges]
      new_geom = st_line_merge(st_combine(orig_geoms))
      # There are two situations where merging lines like this is problematic.
      # 1. When the source and sink node of the new edge are the same.
      # --> In this case the original edges to be replaced form a closed loop.
      # --> Any original endpoint can then be the startpoint of the new edge.
      # --> st_line_merge chooses the point with the lowest x coordinate.
      # --> This is not necessarily the source node we defined.
      # --> This behaviour comes from third partly libs and can not be tuned.
      # --> Hence, we manually need to reorder the points in the merged line.
      if (E$from == E$to && length(orig_edges) > 1) {
        pts = st_cast(new_geom, "POINT")
        from_idx = st_equals(node_geoms[E$from], pts)[[1]]
        if (length(from_idx) == 1) {
          n = length(pts)
          ordered_pts = c(pts[c(from_idx:n)], pts[c(2:from_idx)])
          new_geom = st_cast(st_combine(ordered_pts), "LINESTRING")
        }
      }
      # 2. When the new edge crosses itself.
      # --> In this case st_line_merge creates a multilinestring geometry.
      # --> We just want a regular linestring (even if this is invalid).
      if (any(st_is(new_geom, "MULTILINESTRING"))) {
        new_geom = force_multilinestrings_to_linestrings(new_geom)
      }
      new_geom
    }
    new_geoms = do.call("c", lapply(new_idxs, merge_geoms))
  }
  ## ============================================
  # STEP V: ADD REPLACEMENT EDGES TO THE NETWORK
  # The newly created edges should be added to the original network.
  # This must happen before removing the pseudo nodes.
  # Otherwise their from and to values do not match the correct node indices.
  ## ============================================
  # Create the data frame for the new edges.
  new_edges = cbind(
    data.frame(do.call("rbind", new_idxs)),
    data.frame(do.call("rbind", new_attrs))
  )
  # Bind together with the original edges.
  # Merged edges may have list-columns for some attributes.
  # This requires a bit more complicated rowbinding.
  if (explicit_edges) {
    new_edges[edge_geomcol] = list(new_geoms)
    all_edges = bind_rows_list(edges, new_edges)
    all_edges = st_as_sf(all_edges, sf_column_name = edge_geomcol)
  } else {
    all_edges = bind_rows_list(edges, new_edges)
  }
  # Recreate an sfnetwork.
  x_new = sfnetwork_(nodes, all_edges, directed = directed)
  ## ============================================
  # STEP VI: REMOVE PSEUDO NODES FROM THE NETWORK
  # Remove all the detected pseudo nodes from the original network.
  # This will automatically also remove their incident edges.
  # Remember that their replacement edges have already been added in step IV.
  # From and to indices will be updated automatically.
  ## ============================================
  x_new = delete_vertices(x_new, pseudo) %preserve_all_attrs% x
  ## ==============================================
  # STEP VII: STORE ORIGINAL EDGE DATA IF REQUESTED
  # Users can request to store the data of original edges in a special column.
  # This column will - by tidygraph design - be named .orig_data.
  # The value in this column is for each edge a tibble containing:
  # --> The data of the original edges that were merged into the new edge.
  ## ==============================================
  if (store_original_data) {
    # Store the original edge data in a .orig_data column.
    new_edges = edge_data(x, focused = FALSE_new)
    edges$.tidygraph_edge_index = NULL
    copy_data = function(i) edges[i, , drop = FALSE]
    new_edges$.orig_data = lapply(new_edges$.tidygraph_edge_index, copy_data)
    edge_data(x_new) = new_edges
  }
  # Return in a list.
  list(
    smooth = x_new
  )
}

#' @describeIn spatial_morphers Construct a subdivision of the network by
#' subdividing edges at each interior point that is equal to any other interior
#' or boundary point in the edges table. Interior points are those points that
#' shape a linestring geometry feature but are not endpoints of it, while
#' boundary points are the endpoints of the linestrings, i.e. the existing
#' nodes in het network. Returns a \code{morphed_sfnetwork} containing a single
#' element of class \code{\link{sfnetwork}}. This morpher requires edges to be
#' spatially explicit.
#'
#' @param merge_equal Should multiple subdivision points at the same location
#' be merged into a single node, and should subdivision points at the same
#' as an existing node be merged into that node? Defaults to \code{TRUE}. If
#' set to \code{FALSE}, each subdivision point is added separately as a new
#' node to the network. By default sfnetworks rounds coordinates to 12 decimal
#' places to determine spatial equality. You can influence this behavior by
#' explicitly setting the precision of the network using
#' \code{\link[sf]{st_set_precision}}.
#'
#' @export
to_spatial_subdivision = function(x, merge_equal = TRUE) {
  x_new = subdivide(x, merge_equal = merge_equal)
  list(
    subdivision = x_new %preserve_network_attrs% x
  )
}

#' @describeIn spatial_morphers Subset the network by applying a spatial
#' filter, i.e. a filter on the geometry column based on a spatial predicate.
#' \code{...} is evaluated in the same manner as \code{\link[sf]{st_filter}}.
#' Returns a \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}. For filters on an attribute column, use
#' \code{\link[tidygraph]{to_subgraph}}.
#'
#' @param subset_by Whether to create subgraphs based on nodes or edges.
#'
#' @importFrom cli cli_alert
#' @export
to_spatial_subset = function(x, ..., subset_by = NULL) {
  if (is.null(subset_by)) {
    subset_by = attr(x, "active")
    cli_alert("Subsetting by {subset_by}")
  }
  x_new = switch(
    subset_by,
    nodes = spatial_filter_nodes(x, ...),
    edges = spatial_filter_edges(x, ...),
    raise_unknown_input("subset_by", subset_by, c("nodes", "edges"))
  )
  list(
    subset = x_new
  )
}

#' @describeIn spatial_morphers Transform the geospatial coordinates of the
#' network into a different coordinate reference system. \code{...} is
#' evaluated in the same manner as \code{\link[sf]{st_transform}}.
#' Returns a \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}.
#' @importFrom sf st_transform
#' @export
to_spatial_transformed = function(x, ...) {
  list(
    transformed = st_transform(x, ...)
  )
}

#' @describeIn spatial_morphers Merge nodes with equal geometries into a single
#' node. Returns a \code{morphed_sfnetwork} containing a single element of
#' class \code{\link{sfnetwork}}. By default sfnetworks rounds coordinates to
#' 12 decimal places to determine spatial equality. You can influence this
#' behavior by explicitly setting the precision of the network using
#' \code{\link[sf]{st_set_precision}}.
#'
#' @importFrom igraph contract delete_vertex_attr
#' @importFrom sf st_as_sf st_geometry
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
#' @export
to_spatial_unique = function(x, summarise_attributes = "ignore",
                             store_original_data = FALSE) {
  # Retrieve nodes from the network.
  # Extract specific information from them.
  nodes = nodes_as_sf(x)
  node_geoms = st_geometry(nodes)
  node_geomcol = attr(nodes, "sf_column")
  # Define which nodes have equal geometries.
  matches = st_match_points(node_geoms)
  # Update the attribute summary instructions.
  # During morphing tidygraph adds the tidygraph node index column.
  # Since it is added internally it is not referenced in summarise_attributes.
  # We need to include it manually.
  # They should be concatenated into a vector.
  if (! inherits(summarise_attributes, "list")) {
    summarise_attributes = list(summarise_attributes)
  }
  summarise_attributes[".tidygraph_node_index"] = "concat"
  # The geometries will be summarized at a later stage.
  # However igraph does not know the geometries are special.
  # We therefore temporarily remove the geometries before contracting.
  x_tmp = delete_vertex_attr(x, node_geomcol)
  # Contract with igraph::contract.
  x_new = as_tbl_graph(contract(x_tmp, matches, summarise_attributes))
  # Extract the nodes from the contracted network.
  new_nodes = as_tibble(x_new, "nodes", focused = FALSE)
  # Add geometries to the new nodes.
  # These are simply the original node geometries with duplicates removed.
  new_node_geoms = node_geoms[!duplicated(matches)]
  new_nodes[node_geomcol] = list(new_node_geoms)
  # If requested, store original node data in a .orig_data column.
  if (store_original_data) {
    drop_index = function(i) { i$.tidygraph_node_index = NULL; i }
    grouped_data = split(nodes, matches)
    new_nodes$.orig_data = lapply(grouped_data, drop_index)
  }
  # Update the nodes table of the contracted network.
  new_nodes = st_as_sf(new_nodes, sf_column_name = node_geomcol)
  node_data(x_new) = new_nodes
  # Return new network as sfnetwork object in a list.
  list(
    unique = tbg_to_sfn(x_new %preserve_network_attrs% x)
  )
}
