#' Query spatial edge measures
#'
#' These functions are a collection of edge measures in spatial networks.
#'
#' @details Just as with all query functions in tidygraph, spatial edge
#' measures are meant to be called inside tidygraph verbs such as
#' \code{\link[tidygraph]{mutate}} or \code{\link[tidygraph]{filter}}, where
#' the network that is currently being worked on is known and thus not needed
#' as an argument to the function. If you want to use an algorithm outside of
#' the tidygraph framework you can use \code{\link[tidygraph]{with_graph}} to
#' set the context temporarily while the algorithm is being evaluated.
#'
#' @return A numeric vector of the same length as the number of edges in the
#' graph.
#'
#' @name spatial_edge_measures
NULL

#' @describeIn spatial_edge_measures The angle in radians between a straight
#' line from the edge startpoint pointing north, and the straight line from
#' the edge startpoint and the edge endpoint. Calculated with
#' \code{\link[lwgeom]{st_geod_azimuth}}. Requires a geographic CRS.
#'
#' @param degrees Should the angle be returned in degrees instead of radians?
#' Defaults to \code{FALSE}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel)
#'
#' net |>
#'   activate(edges) |>
#'   mutate(azimuth = edge_azimuth())
#'
#' net |>
#'   activate(edges) |>
#'   mutate(azimuth = edge_azimuth(degrees = TRUE))
#'
#' @importFrom lwgeom st_geod_azimuth
#' @importFrom tidygraph .G
#' @importFrom units set_units
#' @export
edge_azimuth = function(degrees = FALSE) {
  require_active_edges()
  x = .G()
  bounds = edge_incident_geoms(x, focused = TRUE)
  values = st_geod_azimuth(bounds)[seq(1, length(bounds), 2)]
  if (degrees) values = set_units(values, "degrees")
  values
}

#' @describeIn spatial_edge_measures The ratio of the length of an edge
#' linestring geometry versus the straight-line distance between its boundary
#' nodes, as described in Giacomin &
#' Levinson, 2015. DOI: 10.1068/b130131p.
#'
#' @param Inf_as_NaN Should the circuity values of loop edges be stored as
#' \code{NaN} instead of \code{Inf}? Defaults to \code{FALSE}.
#'
#' @examples
#' net |>
#'   activate(edges) |>
#'   mutate(circuity = edge_circuity())
#'
#' @importFrom sf st_length
#' @importFrom tidygraph .G
#' @importFrom units drop_units
#' @export
edge_circuity = function(Inf_as_NaN = FALSE) {
  require_active_edges()
  x = .G()
  if (has_explicit_edges(x)) {
    # Compute circuity as the ratio between length and displacement.
    length = st_length(pull_edge_geom(x, focused = TRUE))
    sldist = straight_line_distance(x)
    values = length / sldist
    # Drop units since circuity is unitless (it is a ratio of m/m).
    if (inherits(values, "units")) values = drop_units(values)
    # Replace Inf values by NaN if requested.
    if (Inf_as_NaN) values[is.infinite(values)] = NaN
  } else {
    # Implicit edges are always straight lines, i.e. circuity = 0.
    values = rep(0, n_edges(x, focused = TRUE))
  }
  values
}

#' @describeIn spatial_edge_measures The length of an edge linestring geometry
#' as calculated by \code{\link[sf]{st_length}}. If edges are spatially
#' implicit, the straight-line distance between its boundary nodes is computed
#' instead, using \code{\link[sf]{st_distance}}.
#'
#' @examples
#' net |>
#'   activate(edges) |>
#'   mutate(length = edge_length())
#'
#' @importFrom sf st_length
#' @importFrom tidygraph .G
#' @export
edge_length = function() {
  require_active_edges()
  x = .G()
  if (has_explicit_edges(x)) {
    st_length(pull_edge_geom(x, focused = TRUE))
  } else {
    straight_line_distance(x)
  }
}

#' @describeIn spatial_edge_measures The straight-line distance between the two
#' boundary nodes of an edge, as calculated by \code{\link[sf]{st_distance}}.
#'
#' @examples
#' net |>
#'   activate(edges) |>
#'   mutate(displacement = edge_displacement())
#'
#' @importFrom tidygraph .G
#' @export
edge_displacement = function() {
  require_active_edges()
  straight_line_distance(.G())
}

#' @importFrom sf st_distance
straight_line_distance = function(x) {
  # Extract the nodes from the network.
  nodes = pull_node_geom(x)
  # Get the indices of the boundary nodes of each edge.
  # Returns a matrix with source ids in column 1 and target ids in column 2.
  idxs = edge_incident_ids(x, focused = TRUE, matrix = TRUE)
  # Calculate distances pairwise.
  st_distance(nodes[idxs[, 1]], nodes[idxs[, 2]], by_element = TRUE)
}

#' @describeIn spatial_edge_measures The number of segments contained in the
#' linestring geometry of an edge. Segments are those parts of a linestring
#' geometry that do not contain any interior points.
#'
#' @examples
#' net |>
#'   activate(edges) |>
#'   mutate(n_segs = edge_segment_count())
#'
#' @importFrom tidygraph .G
#' @export
edge_segment_count = function() {
  require_active_edges()
  geoms = pull_edge_geom(.G(), focused = TRUE)
  lengths(geoms) / 2 - 1
}

#' Query edges with spatial predicates
#'
#' These functions allow to interpret spatial relations between edges and
#' other geospatial features directly inside \code{\link[tidygraph]{filter}}
#' and \code{\link[tidygraph]{mutate}} calls. All functions return a logical
#' vector of the same length as the number of edges in the network. Element i
#' in that vector is \code{TRUE} whenever the chosen spatial predicate applies
#' to the spatial relation between the i-th edge and any of the features in
#' \code{y}.
#'
#' @param y The geospatial features to test the edges against, either as an
#' object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @param ... Arguments passed on to the corresponding spatial predicate
#' function of sf. See \code{\link[sf]{geos_binary_pred}}. The argument
#' \code{sparse} should not be set.
#'
#' @return A logical vector of the same length as the number of edges in the
#' network.
#'
#' @details See \code{\link[sf]{geos_binary_pred}} for details on each spatial
#' predicate. The function \code{edge_is_nearest} instead wraps around
#' \code{\link[sf]{st_nearest_feature}} and returns \code{TRUE} for element i
#' if the i-th edge is the nearest edge to any of the features in \code{y}.
#'
#' Just as with all query functions in tidygraph, these functions are meant to
#' be called inside tidygraph verbs such as \code{\link[tidygraph]{mutate}} or
#' \code{\link[tidygraph]{filter}}, where the network that is currently being
#' worked on is known and thus not needed as an argument to the function. If
#' you want to use an algorithm outside of the tidygraph framework you can use
#' \code{\link[tidygraph]{with_graph}} to set the context temporarily while the
#' algorithm is being evaluated.
#'
#' @note Note that \code{edge_is_within_distance} is a wrapper around the
#' \code{st_is_within_distance} predicate from sf. Hence, it is based on
#' 'as-the-crow-flies' distance, and not on distances over the network.
#'
#' @examples
#' library(sf, quietly = TRUE)
#' library(tidygraph, quietly = TRUE)
#'
#' # Create a network.
#' net = as_sfnetwork(roxel) |>
#'   st_transform(3035)
#'
#' # Create a geometry to test against.
#' p1 = st_point(c(4151358, 3208045))
#' p2 = st_point(c(4151340, 3207520))
#' p3 = st_point(c(4151756, 3207506))
#' p4 = st_point(c(4151774, 3208031))
#'
#' poly = st_multipoint(c(p1, p2, p3, p4)) |>
#'   st_cast('POLYGON') |>
#'   st_sfc(crs = 3035)
#'
#' # Use predicate query function in a filter call.
#' intersects = net |>
#'   activate(edges) |>
#'   filter(edge_intersects(poly))
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1))
#' plot(st_geometry(net, "edges"))
#' plot(st_geometry(intersects, "edges"), col = "red", lwd = 2, add = TRUE)
#' par(oldpar)
#'
#' # Use predicate query function in a mutate call.
#' net |>
#'   activate(edges) |>
#'   mutate(disjoint = edge_is_disjoint(poly)) |>
#'   select(disjoint)
#'
#' # Use predicate query function directly.
#' intersects = with_graph(net, edge_intersects(poly))
#' head(intersects)
#'
#' @name spatial_edge_predicates
NULL

#' @name spatial_edge_predicates
#' @importFrom sf st_intersects
#' @importFrom tidygraph .G
#' @export
edge_intersects = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_intersects, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom sf st_disjoint
#' @importFrom tidygraph .G
#' @export
edge_is_disjoint = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_disjoint, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom sf st_touches
#' @importFrom tidygraph .G
#' @export
edge_touches = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_touches, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom sf st_crosses
#' @importFrom tidygraph .G
#' @export
edge_crosses = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_crosses, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom sf st_within
#' @importFrom tidygraph .G
#' @export
edge_is_within = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_within, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom sf st_contains
#' @importFrom tidygraph .G
#' @export
edge_contains = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_contains, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom sf st_contains_properly
#' @importFrom tidygraph .G
#' @export
edge_contains_properly = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_contains_properly, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom sf st_overlaps
#' @importFrom tidygraph .G
#' @export
edge_overlaps = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_overlaps, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom sf st_equals
#' @importFrom tidygraph .G
#' @export
edge_equals = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_equals, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom sf st_covers
#' @importFrom tidygraph .G
#' @export
edge_covers = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_covers, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom sf st_covered_by
#' @importFrom tidygraph .G
#' @export
edge_is_covered_by = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_covered_by, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom sf st_is_within_distance
#' @importFrom tidygraph .G
#' @export
edge_is_within_distance = function(y, ...) {
  require_active_edges()
  evaluate_edge_predicate(st_is_within_distance, .G(), y, ...)
}

#' @name spatial_edge_predicates
#' @importFrom tidygraph .G
#' @export
edge_is_nearest = function(y) {
  require_active_edges()
  x = .G()
  vec = rep(FALSE, n_edges(x))
  vec[nearest_edge_ids(x, y, focused = FALSE)] = TRUE
  vec[edge_ids(x, focused = TRUE)]
}

evaluate_edge_predicate = function(predicate, x, y, ...) {
  E = pull_edge_geom(x, focused = TRUE)
  lengths(predicate(E, y, sparse = TRUE, ...)) > 0
}

#' Convert undirected edges into directed edges based on their geometries
#'
#' This function converts an undirected network to a directed network following
#' the direction given by the linestring geometries of the edges.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @details In undirected spatial networks it is required that the boundary of
#' edge geometries contain their incident node geometries. However, it is not
#' required that their start point equals their specified *from* node and their
#' end point their specified *to* node. Instead, it may be vice versa. This is
#' because for undirected networks *from* and *to* indices are always swopped
#' if the *to* index is lower than the *from* index. Therefore, the direction
#' given by the *from* and *to* indices does not necessarily match the
#' direction given by the edge geometries.
#'
#' @note If the network is already directed it is returned unmodified.
#'
#' @return A directed network as object of class \code{\link{sfnetwork}}.
#'
#' @importFrom igraph is_directed
#' @export
make_edges_directed = function(x) {
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
  sfnetwork_(nodes, edges, directed = TRUE) %preserve_network_attrs% x
}

#' Make some edges directed and some undirected
#'
#' This function creates a mixed network, meaning that some edges are directed,
#' and some are undirected. In practice this is implemented as a directed
#' network in which those edges that are meant to be undirected are duplicated
#' and reversed.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param directed An integer vector of edge indices specifying those edges
#' that should be directed.
#'
#' @return A mixed network as object of class \code{\link{sfnetwork}}.
#'
#' @importFrom dplyr arrange bind_rows
#' @importFrom sf st_reverse
#' @export
make_edges_mixed = function(x, directed) {
  # First make the network directed.
  x = make_edges_directed(x)
  # Extract edges from the network
  edges = edge_data(x, focused = FALSE)
  edge_ids = seq_len(nrow(edges))
  # Keep track of the original edge index.
  # This is used to later sort the edges table.
  if (".sfnetwork_index" %in% names(edges)) {
    raise_reserved_attr(".sfnetwork_index")
  }
  edges$.sfnetwork_index = edge_ids
  # Define which edges should be undirected.
  undirected = setdiff(edge_ids, directed)
  # Duplicate undirected edges.
  duplicates = edges[undirected, ]
  # Reverse the duplicated undirected edges.
  from = duplicates$from
  to = duplicates$to
  duplicates$from = to
  duplicates$to = from
  if (is_sf(edges)) {
    duplicates = st_reverse(duplicates)
  }
  # Bind duplicated and reversed edges to the original edges.
  new_edges = bind_rows(edges, duplicates)
  # Use original indices to sort the new edges table.
  new_edges = arrange(new_edges, .sfnetwork_index)
  new_edges$.sfnetwork_index = NULL
  # Create a new network with the updated edges.
  x_new = sfnetwork_(nodes_as_sf(x), new_edges, directed = TRUE)
  x_new %preserve_network_attrs% x
}

#' Construct edge geometries for spatially implicit networks
#'
#' This function turns spatially implicit networks into spatially explicit
#' networks by adding a geometry column to the edge data.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments forwarded to \code{\link[sf]{st_as_sf}} to directly
#' convert the edges table into a sf object. If no arguments are given, the
#' edges are made explicit by simply drawing straight lines between the start
#' and end node of each edge.
#'
#' @note If the network is already spatially explicit it is returned
#' unmodified.
#'
#' @return An object of class \code{\link{sfnetwork}} with spatially explicit
#' edges.
#'
#' @importFrom rlang dots_n
#' @importFrom sf st_as_sf st_crs st_sfc
#' @export
make_edges_explicit = function(x, ...) {
  # Return x unmodified if edges are already spatially explicit.
  if (has_explicit_edges(x)) return(x)
  # Add an empty geometry column if there are no edges.
  if (n_edges(x) == 0) return(mutate_edge_geom(x, st_sfc(crs = st_crs(x))))
  # In any other case:
  # --> If ... is specified use it to convert edges table to sf.
  # --> Otherwise simply draw straight lines between the incident nodes.
  if (dots_n(...) > 0) {
    edges = edge_data(x, focused = FALSE)
    new_edges = st_as_sf(edges, ...)
    x_new = x
    edge_data(x_new) = new_edges
  } else {
    bounds = edge_incident_geoms(x, list = TRUE)
    x_new = mutate_edge_geom(x, draw_lines(bounds[[1]], bounds[[2]]))
  }
  x_new
}

#' Drop edge geometries of spatially explicit networks
#'
#' This function turns spatially explicit networks into spatially implicit
#' networks by dropping the geometry column of the edge data.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @note If the network is already spatially implicit it is returned
#' unmodified.
#'
#' @return An object of class \code{\link{sfnetwork}} with spatially implicit
#' edges.
#'
#' @export
make_edges_implicit = function(x) {
  drop_edge_geom(x)
}

#' Match the direction of edge geometries to their specified incident nodes
#'
#' This function updates edge geometries in undirected networks such that they
#' are guaranteed to start at their specified *from* node and end at their
#' specified *to* node.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @details In undirected spatial networks it is required that the boundary of
#' edge geometries contain their incident node geometries. However, it is not
#' required that their start point equals their specified *from* node and their
#' end point their specified *to* node. Instead, it may be vice versa. This is
#' because for undirected networks *from* and *to* indices are always swopped
#' if the *to* index is lower than the *from* index.
#'
#' This function reverses edge geometries if they start at the *to* node and
#' end at the *from* node, such that in the resulting network it is guaranteed
#' that edge boundary points exactly match their incident node geometries. In
#' directed networks, there will be no change.
#'
#' @return An object of class \code{\link{sfnetwork}} with updated edge
#' geometries.
#'
#' @importFrom sf st_reverse
#' @export
make_edges_follow_indices = function(x) {
  # Extract geometries of edges and subsequently their start points.
  edges = pull_edge_geom(x)
  start_points = linestring_start_points(edges)
  # Extract the geometries of the nodes that should be at their start.
  start_nodes = edge_source_geoms(x)
  # Reverse edge geometries for which start point does not equal start node.
  to_be_reversed = ! have_equal_geometries(start_points, start_nodes)
  edges[to_be_reversed] = st_reverse(edges[to_be_reversed])
  mutate_edge_geom(x, edges)
}

#' Match edge geometries to their incident node locations
#'
#' This function makes invalid edges valid by modifying either edge or node
#' geometries such that the boundary points of edge linestring geometries
#' always match the point geometries of the nodes that are specified as their
#' incident nodes by the *from* and *to* columns.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param preserve_geometries Should the edge geometries remain unmodified?
#' Defaults to \code{FALSE}. See Details.
#'
#' @details If geometries should be preserved, edges are made valid by adding
#' edge boundary points that do not equal their corresponding node geometry as
#' new nodes to the network, and updating the *from* and *to* indices to match
#' this newly added nodes. If \code{FALSE}, edges are made valid by modifying
#' their geometries, i.e. edge boundary points that do not equal their
#' corresponding node geometry are replaced by that node geometry.
#'
#' @note This function works only if the edge geometries are meant to start at
#' their specified *from* node and end at their specified *to* node. In
#' undirected networks this is not necessarily the case, since edge geometries
#' are allowed to start at their specified *to* node and end at their specified
#' *from* node. Therefore, in undirected networks those edges first have to be
#' reversed before running this function. Use
#' \code{\link{make_edges_follow_indices}} for this.
#'
#' @return An object of class \code{\link{sfnetwork}} with corrected edge
#' geometries.
#'
#' @export
make_edges_valid = function(x, preserve_geometries = FALSE) {
  if (preserve_geometries) {
    add_invalid_edge_boundaries(x)
  } else {
    replace_invalid_edge_boundaries(x)
  }
}

#' @importFrom dplyr bind_rows
#' @importFrom igraph is_directed
#' @importFrom sf st_geometry st_sf
add_invalid_edge_boundaries = function(x) {
  # Extract node and edge data.
  nodes = nodes_as_sf(x)
  edges = edges_as_sf(x)
  # Check which edge boundary points do not match their specified nodes.
  boundary_points = linestring_boundary_points(edges)
  boundary_node_ids = edge_incident_ids(x)
  boundary_nodes = st_geometry(nodes)[boundary_node_ids]
  no_match = !have_equal_geometries(boundary_points, boundary_nodes)
  # For boundary points that do not match their node:
  # Boundary points that don't match their node become new nodes themselves.
  new_nodes = list()
  new_nodes[node_geom_colname(x)] = list(boundary_points[which(no_match)])
  new_nodes = st_sf(new_nodes)
  all_nodes = bind_rows(nodes, new_nodes)
  # Update the from and to columns of the edges accordingly.
  n_nodes = nrow(nodes)
  n_new_nodes = nrow(new_nodes)
  boundary_node_ids[no_match] = c((n_nodes + 1):(n_nodes + n_new_nodes))
  n_boundaries = length(boundary_node_ids)
  edges$from = boundary_node_ids[seq(1, n_boundaries - 1, 2)]
  edges$to = boundary_node_ids[seq(2, n_boundaries, 2)]
  # Return a new network with the added nodes and updated edges.
  sfnetwork_(all_nodes, edges, is_directed(x)) %preserve_network_attrs% x
}

#' @importFrom sfheaders sfc_to_df
replace_invalid_edge_boundaries = function(x) {
  # Extract geometries of edges.
  edges = pull_edge_geom(x)
  # Extract the geometries of the nodes that should be at their ends.
  nodes = edge_incident_geoms(x)
  # Decompose the edges into the points that shape them.
  # Convert the correct boundary nodes into the same structure.
  E = sfc_to_df(edges)
  N = sfc_to_df(nodes)
  # Define for each edge point if it is a boundary point.
  is_start = ! duplicated(E$linestring_id)
  is_end = ! duplicated(E$linestring_id, fromLast = TRUE)
  is_bound = is_start | is_end
  # Update the coordinates of the edge boundary points.
  # They should match the coordinates of their boundary nodes.
  E_new = list()
  if (! is.null(E$x)) {
    x_new = E$x
    x_new[is_bound] = N$x
    E_new$x = x_new
  }
  if (! is.null(E$y)) {
    y_new = E$y
    y_new[is_bound] = N$y
    E_new$y = y_new
  }
  if (! is.null(E$z)) {
    z_new = E$z
    z_new[is_bound] = N$z
    E_new$z = z_new
  }
  if (! is.null(E$m)) {
    m_new = E$m
    m_new[is_bound] = N$m
    E_new$m = m_new
  }
  E_new$id = E$linestring_id
  # Update the geometries of the edges table.
  mutate_edge_geom(x, df_to_lines(as.data.frame(E_new), edges, id_col = "id"))
}
