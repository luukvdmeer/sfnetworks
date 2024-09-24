#' Morph spatial networks into a different structure
#'
#' Spatial morphers form spatial add-ons to the set of
#' \code{\link[tidygraph]{morphers}} provided by \code{tidygraph}. These
#' functions change the existing structure of the network.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param protect Nodes or edges to be protected from being changed in
#' structure. Evaluated by \code{\link{evaluate_node_query}} in the case of
#' nodes and by \code{\link{evaluate_edge_query}} in the case of edges.
#' Defaults to \code{NULL}, meaning that no features are protected.
#'
#' @param summarise_attributes Whenever groups of nodes or edges are merged
#' into a single feature during morphing, how should their attributes be
#' summarized? There are several options, see
#' \code{\link[igraph]{igraph-attribute-combination}} for details.
#'
#' @param store_original_data Whenever groups of nodes or edges are merged
#' into a single feature during morphing, should the data of the original
#' features be stored as an attribute of the new feature, in a column named
#' \code{.orig_data}. This is in line with the design principles of
#' \code{tidygraph}. Defaults to \code{FALSE}.
#'
#' @param ... Arguments to be passed on to other functions. See the description
#' of each morpher for details.
#'
#' @return Either a \code{morphed_sfnetwork}, which is a list of one or more
#' \code{\link{sfnetwork}} objects, or a \code{morphed_tbl_graph}, which is a
#' list of one or more \code{\link[tidygraph]{tbl_graph}} objects. See the
#' description of each morpher for details.
#'
#' @details Morphers are not meant to be called directly. Instead, they should
#' be called inside the \code{\link[tidygraph]{morph}} verb to change the
#' network structure temporarily. Depending on the chosen morpher, this results
#' in a list of one or more network objects. Single elements of that list can
#' be extracted directly as a new network by calling the morpher inside the
#' \code{\link[tidygraph]{convert}} verb instead, to make the changes lasting
#' rather than temporary.
#'
#' It also possible to create your own morphers. See the documentation of
#' \code{\link[tidygraph]{morph}} for the requirements for custom morphers.
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel, directed = FALSE) |>
#'   st_transform(3035)
#'
#' # Temporary changes with morph and unmorph.
#' net |>
#'  activate(edges) |>
#'  morph(to_spatial_shortest_paths, from = 1, to = 10) |>
#'  mutate(in_paths = TRUE) |>
#'  unmorph()
#'
#' # Lasting changes with convert.
#' net |>
#'  activate(edges) |>
#'  convert(to_spatial_shortest_paths, from = 1, to = 10)
#'
#' @name spatial_morphers
NULL

#' @describeIn spatial_morphers Combine groups of nodes into a single node per
#' group. \code{...} is forwarded to \code{\link[dplyr]{group_by}} to
#' create the groups. The centroid of such a group will be used by default as
#' geometry of the contracted node. If edges are spatially explicit, edge
#' geometries are updated accordingly such that the valid spatial network
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
#' @importFrom dplyr group_by group_indices
#' @importFrom sf st_drop_geometry
#' @export
to_spatial_contracted = function(x, ..., simplify = TRUE,
                                 compute_centroids = TRUE,
                                 summarise_attributes = "concat",
                                 store_original_data = FALSE) {
  # Create groups.
  groups = group_by(st_drop_geometry(nodes_as_sf(x)), ...)
  group_ids = group_indices(groups)
  # Contract.
  x_new = contract_nodes(
    x = x,
    groups = group_ids,
    simplify = simplify,
    compute_centroids = compute_centroids,
    reconnect_edges = TRUE,
    summarise_attributes = summarise_attributes,
    store_original_ids = TRUE,
    store_original_data = store_original_data
  )
  # Return in a list.
  list(
    contracted = x_new
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
#' @export
to_spatial_directed = function(x) {
  list(
    directed = make_edges_directed(x)
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
#' @export
to_spatial_explicit = function(x, ...) {
  list(
    explicit = make_edges_explicit(x, ...)
  )
}

#' @describeIn spatial_morphers Drop edge geometries from the network. Returns
#' a \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}.
#' @export
to_spatial_implicit = function(x) {
  list(
    implict = make_edges_implict(x, ...)
  )
}

#' @describeIn spatial_morphers Construct a mixed network in which some edges
#' are directed, and some are undirected. In practice this is implemented as a
#' directed network in which those edges that are meant to be undirected are
#' duplicated and reversed. Returns a \code{morphed_sfnetwork} containing a
#' single element of class \code{\link{sfnetwork}}.
#'
#' @param directed Which edges should be directed? Evaluated by
#' \code{\link{evaluate_edge_query}}.
#'
#' @export
to_spatial_mixed = function(x, directed) {
  list(
    mixed = make_edges_mixed(x, directed)
  )
}

#' @describeIn spatial_morphers Limit a network to the spatial neighborhood of
#' a specific node. \code{...} is forwarded to \code{\link{st_network_cost}} to
#' compute the travel cost from the specified node to all other nodes in the
#' network. Returns a \code{morphed_sfnetwork} that may contain multiple
#' elements of class \code{\link{sfnetwork}}, depending on the number of given
#' thresholds. When unmorphing only the first instance of both the node and
#' edge data will be used, as the the same node and/or edge can be present in
#' multiple neighborhoods.
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
#' Multiple threshold values may be given, which will result in mutliple
#' neigborhoods being returned.
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
  # Parse the given threshold values.
  if (inherits(costs, "units") && ! inherits(threshold, "units")) {
    threshold = as_units(threshold, deparse_unit(costs))
  }
  # For each given threshold:
  # --> Define which nodes are in the neighborhood.
  # --> Subset the network to keep only the nodes in the neighborhood.
  get_single_neighborhood = function(k) {
    in_neighborhood = costs[1, ] <= k
    induced_subgraph(x, in_neighborhood) %preserve_all_attrs% x
  }
  lapply(threshold, get_single_neighborhood)
}

#' @describeIn spatial_morphers Reverse the direction of edges. Returns a
#' \code{morphed_sfnetwork} containing a single element of class
#' \code{\link{sfnetwork}}.
#' @importFrom igraph is_directed reverse_edges
#' @importFrom sf st_reverse
#' @export
to_spatial_reversed = function(x, protect = NULL) {
  # Define which edges should be reversed.
  if (is.null(protect)) {
    reverse = edge_ids(x, focused = FALSE)
  } else {
    protect = evaluate_edge_query(x, protect)
    reverse = setdiff(edge_ids(x, focused = FALSE), protect)
  }
  # Reverse the from and to indices of those edges.
  # This will have no effect on undirected networks.
  x_new = reverse_edges(x, eids = reverse) %preserve_all_attrs% x
  # Reverse the geometries of those edges.
  if (has_explicit_edges(x)) {
    edge_geom = pull_edge_geom(x)
    edge_geom[reverse] = st_reverse(edge_geom)[reverse]
    x_new = mutate_edge_geom(x_new, edge_geom)
  }
  # Return in a list.
  list(
    reversed = x_new
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
#' @importFrom igraph is_directed
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
  nodes = nodes_as_sf(x)
  edges = edge_data(x, focused = FALSE)
  # Subset the network for each computed shortest path.
  get_single_path = function(i) {
    if (paths[i, ]$path_found) {
      node_ids = paths$nodes[[i]]
      edge_ids = paths$edges[[i]]
      N = nodes[node_ids, ]
      E = edges[edge_ids, ]
      E$from = c(1:(length(node_ids) - 1))
      E$to = c(2:length(node_ids))
    } else {
      N = nodes[0, ]
      E = edges[0, ]
    }
    sfnetwork_(N, E, directed = is_directed(x))
  }
  lapply(seq_len(nrow(paths)), get_single_path)
}

#' @describeIn spatial_morphers Construct a simple version of the network. A
#' simple network is defined as a network without loop edges and multiple
#' edges. A loop edge is an edge that starts and ends at the same node.
#' Multiple edges are different edges between the same node pair. When merging
#' them into a single edge, the geometry of the first edge is preserved. The
#' order of the edges can be influenced by calling \code{\link[dplyr]{arrange}}
#' before simplifying. Returns a \code{morphed_sfnetwork} containing a single
#' element of class \code{\link{sfnetwork}}.
#'
#' @param remove_multiple Should multiple edges be merged into one. Defaults
#' to \code{TRUE}.
#'
#' @param remove_loops Should loop edges be removed. Defaults to \code{TRUE}.
#'
#' @export
to_spatial_simple = function(x, remove_multiple = TRUE, remove_loops = TRUE,
                             summarise_attributes = "first",
                             store_original_data = FALSE) {
  # Simplify.
  x_new = simplify_network(
    x = x,
    remove_loops = remove_loops,
    remove_multiple = remove_multiple,
    summarise_attributes = summarise_attributes,
    store_original_ids = TRUE,
    store_original_data = store_original_data
  )
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
#' @param require_equal Should nodes only be smoothed when the attribute values
#' of their incident edges are equal? Defaults to \code{FALSE}. If \code{TRUE},
#' only pseudo nodes that have incident edges with equal attribute values are
#' smoothed. May also be given as a vector of attribute names. In that case
#' only those attributes are checked for equality. Equality tests are evaluated
#' using the \code{==} operator.
#'
#' @export
to_spatial_smooth = function(x, protect = NULL, require_equal = FALSE,
                             summarise_attributes = "concat",
                             store_original_data = FALSE) {
  # Smooth.
  x_new = smooth_pseudo_nodes(
    x = x,
    protect = protect,
    summarise_attributes = summarise_attributes,
    require_equal = require_equal,
    store_original_ids = TRUE,
    store_original_data = store_original_data
  )
  # Return in a list.
  list(
    smooth = x_new
  )
}

#' @describeIn spatial_morphers Construct a subdivision of the network by
#' subdividing edges at interior points. Subdividing means that a new node is
#' added on an edge, and the edge is split in two at that location. Interior
#' points are those points that shape a linestring geometry feature but are not
#' endpoints of it. Returns a \code{morphed_sfnetwork} containing a single
#' element of class \code{\link{sfnetwork}}. This morpher requires edges to be
#' spatially explicit.
#'
#' @param all Should edges be subdivided at all their interior points? If set
#' to \code{FALSE}, edges are only subdivided at those interior points that
#' share their location with any other interior or boundary point (a node) in
#' the edges table. Defaults to \code{FALSE}. By default sfnetworks rounds
#' coordinates to 12 decimal places to determine spatial equality. You can
#' influence this behavior by explicitly setting the precision of the network
#' using \code{\link[sf]{st_set_precision}}.
#'
#' @param merge Should multiple subdivision points at the same location be
#' merged into a single node, and should subdivision points at the same
#' location as an existing node be merged into that node? Defaults to
#' \code{TRUE}. If set to \code{FALSE}, each subdivision point is added
#' separately as a new node to the network. By default sfnetworks rounds
#' coordinates to 12 decimal places to determine spatial equality. You can
#' influence this behavior by explicitly setting the precision of the network
#' using \code{\link[sf]{st_set_precision}}.
#'
#' @export
to_spatial_subdivision = function(x, protect = NULL, all = FALSE,
                                  merge = TRUE) {
  # Subdivide.
  x_new = subdivide_edges(
    x = x,
    protect = protect,
    all = all,
    merge = merge
  )
  # Return in a list.
  list(
    subdivision = x_new
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
  # Subset.
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
  # Return in a list.
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
#' @export
to_spatial_unique = function(x, summarise_attributes = "concat",
                             store_original_data = FALSE) {
  # Create groups.
  group_ids = st_match_points(pull_node_geom(x))
  # Contract.
  x_new = contract_nodes(
    x = x,
    groups = group_ids,
    simplify = FALSE,
    compute_centroids = FALSE,
    reconnect_edges = FALSE,
    summarise_attributes = summarise_attributes,
    store_original_ids = TRUE,
    store_original_data = store_original_data
  )
  # Return in a list.
  list(
    unique = x_new
  )
}
