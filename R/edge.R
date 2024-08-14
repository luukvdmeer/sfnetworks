#' Query spatial edge measures
#'
#' These functions are a collection of specific spatial edge measures, that
#' form a spatial extension to edge measures in
#' \code{\link[tidygraph:tidygraph-package]{tidygraph}}.
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
  bounds = edge_boundary_nodes(x, focused = TRUE)
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
  idxs = edge_boundary_node_ids(x, focused = TRUE, matrix = TRUE)
  # Calculate distances pairwise.
  st_distance(nodes[idxs[, 1]], nodes[idxs[, 2]], by_element = TRUE)
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

#' Get the geometries of the boundary nodes of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only the boundary nodes of edges that are in focus be
#' extracted? Defaults to \code{FALSE}. See \code{\link[tidygraph]{focus}} for
#' more information on focused networks.
#'
#' @param list Should te result be returned as a two-element list? Defaults
#' to \code{FALSE}.
#'
#' @return If list is \code{FALSE}, An object of class \code{\link[sf]{sfc}}
#' with \code{POINT} geometries of length equal to twice the number of edges in
#' x, and ordered as [start of edge 1, end of edge 1, start of edge 2, end of
#' edge 2, ...]. If list is \code{TRUE}, a list with the first element being
#' the start nodes of the edges as object of class \code{\link[sf]{sfc}} with
#' \code{POINT} geometries, and the second element being the end nodes of the
#' edges as object of class \code{\link[sf]{sfc}} with \code{POINT} geometries.
#'
#' @details Boundary nodes differ from boundary points in the sense that
#' boundary points are retrieved by taking the boundary points of the
#' \code{LINESTRING} geometries of edges, while boundary nodes are retrieved
#' by querying the nodes table of a network with the 'to' and 'from' columns
#' in the edges table. In a valid directed network structure boundary points
#' should be equal to boundary nodes. In a valid undirected network structure
#' boundary points should contain the boundary nodes.
#'
#' @importFrom igraph ends
#' @noRd
edge_boundary_nodes = function(x, focused = FALSE, list = FALSE) {
  nodes = pull_node_geom(x)
  ids = ends(x, edge_ids(x, focused = focused), names = FALSE)
  if (list) {
    list(nodes[ids[, 1]], nodes[ids[, 2]])
  } else {
    nodes[as.vector(t(ids))]
  }
}

#' Get the geometries of the start nodes of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only the start nodes of edges that are in focus be
#' extracted? Defaults to \code{FALSE}. See \code{\link[tidygraph]{focus}} for
#' more information on focused networks.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, of length equal to the number of edges in x.
#'
#' @importFrom igraph ends
#' @noRd
edge_start_nodes = function(x, focused = FALSE) {
  nodes = pull_node_geom(x)
  id_mat = ends(x, edge_ids(x, focused = focused), names = FALSE)
  nodes[id_mat[, 1]]
}

#' Get the geometries of the end nodes of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only the end nodes of edges that are in focus be
#' extracted? Defaults to \code{FALSE}. See \code{\link[tidygraph]{focus}} for
#' more information on focused networks.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, of length equal to the number of edges in x.
#'
#' @importFrom igraph ends
#' @noRd
edge_end_nodes = function(x, focused = FALSE) {
  nodes = pull_node_geom(x)
  id_mat = ends(x, edge_ids(x, focused = focused), names = FALSE)
  nodes[id_mat[, 2]]
}

#' Get the indices of the boundary nodes of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only the indices of boundary nodes of edges that are
#' in focus be extracted? Defaults to \code{FALSE}. See
#' \code{\link[tidygraph]{focus}} for more information on focused networks.
#'
#' @param matrix Should te result be returned as a two-column matrix? Defaults
#' to \code{FALSE}.
#'
#' @return If matrix is \code{FALSE}, a numeric vector of length equal to twice
#' the number of edges in x, and ordered as [start of edge 1, end of edge 1,
#' start of edge 2, end of edge 2, ...]. If matrix is \code{TRUE}, a two-column
#' matrix, with the number of rows equal to the number of edges in the network.
#' The first column contains the indices of the start nodes of the edges, the
#' second column contains the indices of the end nodes of the edges.
#'
#' @importFrom igraph ends
#' @noRd
edge_boundary_node_ids = function(x, focused = FALSE, matrix = FALSE) {
  ends = ends(x, edge_ids(x, focused = focused), names = FALSE)
  if (matrix) ends else as.vector(t(ends))
}

#' Get the indices of the start nodes of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only the indices of start nodes of edges that are
#' in focus be extracted? Defaults to \code{FALSE}. See
#' \code{\link[tidygraph]{focus}} for more information on focused networks.
#'
#' @return A numeric vector of length equal to the number of edges in x.
#'
#' @importFrom igraph ends
#' @noRd
edge_start_node_ids = function(x, focused = FALSE, matrix = FALSE) {
  ends(x, edge_ids(x, focused = focused), names = FALSE)[, 1]
}

#' Get the indices of the end nodes of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only the indices of end nodes of edges that are
#' in focus be extracted? Defaults to \code{FALSE}. See
#' \code{\link[tidygraph]{focus}} for more information on focused networks.
#'
#' @return A numeric vector of length equal to the number of edges in x.
#'
#' @importFrom igraph ends
#' @noRd
edge_end_node_ids = function(x, focused = FALSE, matrix = FALSE) {
  ends(x, edge_ids(x, focused = focused), names = FALSE)[, 2]
}

#' Get the geometries of the boundary points of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only the boundary points of edges that are in focus be
#' extracted? Defaults to \code{FALSE}. See \code{\link[tidygraph]{focus}} for
#' more information on focused networks.
#'
#' @param list Should te result be returned as a two-element list? Defaults
#' to \code{FALSE}.
#'
#' @return If list is \code{FALSE}, An object of class \code{\link[sf]{sfc}}
#' with \code{POINT} geometries of length equal to twice the number of edges in
#' x, and ordered as [start of edge 1, end of edge 1, start of edge 2, end of
#' edge 2, ...]. If list is \code{TRUE}, a list with the first element being
#' the start points of the edges as object of class \code{\link[sf]{sfc}} with
#' \code{POINT} geometries, and the second element being the end points of the
#' edges as object of class \code{\link[sf]{sfc}} with \code{POINT} geometries.
#'
#' @details Boundary nodes differ from boundary points in the sense that
#' boundary points are retrieved by taking the boundary points of the
#' \code{LINESTRING} geometries of edges, while boundary nodes are retrieved
#' by querying the nodes table of a network with the 'to' and 'from' columns
#' in the edges table. In a valid directed network structure boundary points
#' should be equal to boundary nodes. In a valid undirected network structure
#' boundary points should contain the boundary nodes.
#'
#' @noRd
edge_boundary_points = function(x, focused = FALSE, list = FALSE) {
  edges = pull_edge_geom(x, focused = focused)
  points = linestring_boundary_points(edges)
  if (list) {
    starts = points[seq(1, length(points), 2)]
    ends = points[seq(2, length(points), 2)]
    list(starts, ends)
  } else {
    points
  }
}

#' Get the node indices of the boundary points of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param focused Should only the indices of boundary points of edges that are
#' in focus be extracted? Defaults to \code{FALSE}. See
#' \code{\link[tidygraph]{focus}} for more informatio
#'
#' @param matrix Should te result be returned as a two-column matrix? Defaults
#' to \code{FALSE}.
#'
#' @return If matrix is \code{FALSE}, a numeric vector of length equal to twice
#' the number of edges in x, and ordered as
#' [start of edge 1, end of edge 1, start of edge 2, end of edge 2, ...]. If
#' matrix is \code{TRUE}, a two-column matrix, with the number of rows equal to
#' the number of edges in the network. The first column contains the node
#' indices of the start points of the edges, the seconds column contains the
#' node indices of the end points of the edges.
#'
#' @importFrom sf st_equals
#' @noRd
edge_boundary_point_ids = function(x, focused = FALSE, matrix = FALSE) {
    nodes = pull_node_geom(x)
    edges = edges_as_sf(x, focused = focused)
    idxs_lst = st_equals(linestring_boundary_points(edges), nodes)
    idxs_vct = do.call("c", idxs_lst)
    # In most networks the location of a node will be unique.
    # However, this is not a requirement.
    # There may be cases where multiple nodes share the same geometry.
    # Then some more processing is needed to find the correct indices.
    if (length(idxs_vct) != n_edges(x, focused = focused) * 2) {
      n = length(idxs_lst)
      from = idxs_lst[seq(1, n - 1, 2)]
      to = idxs_lst[seq(2, n, 2)]
      p_idxs = mapply(c, from, to, SIMPLIFY = FALSE)
      n_idxs = mapply(c, edges$from, edges$to, SIMPLIFY = FALSE)
      find_indices = function(a, b) {
        idxs = a[a %in% b]
        if (length(idxs) > 2) b else idxs
      }
      idxs_lst = mapply(find_indices, p_idxs, n_idxs, SIMPLIFY = FALSE)
      idxs_vct = do.call("c", idxs_lst)
    }
    if (matrix) t(matrix(idxs_vct, nrow = 2)) else idxs_vct
}

#' Correct edge geometries to match their boundary nodes
#'
#' This function makes invalid edge geometries valid by replacing their
#' boundary points with the geometries of the nodes that should be at their
#' boundary according to the specified from and to indices.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link{sfnetwork}} with corrected edge
#' geometries.
#'
#' @note This function works only if the edge geometries are meant to start at
#' their specified *from* node and end at their specified *to* node. In
#' undirected networks this is not necessarily the case, since edge geometries
#' are allowed to start at their specified *to* node and end at their specified
#' *from* node. Therefore, in undirected networks those edges first have to be
#' reversed before running this function.
#'
#' @importFrom sfheaders sfc_to_df
#' @noRd
correct_edge_geometries = function(x) {
  # Extract geometries of edges.
  edges = pull_edge_geom(x)
  # Extract the geometries of the nodes that should be at their ends.
  nodes = edge_boundary_nodes(x)
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
  E_new = data.frame()
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
  mutate_edge_geom(x, df_to_lines(E_new, edges, id_col = "id"))
}

#' Construct edge geometries for spatially implicit networks
#'
#' This function turns spatially implicit networks into spatially explicit
#' networks by adding a geometry column to the edges data containing straight
#' lines between the start and end nodes.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link{sfnetwork}} with spatially explicit
#' edges. If \code{x} was already spatially explicit it is returned unmodified.
#'
#' @importFrom sf st_crs st_sfc
#' @noRd
construct_edge_geometries = function(x) {
  # Return x unmodified if edges are already spatially explicit.
  if (has_explicit_edges(x)) return(x)
  # Add an empty geometry column if there are no edges.
  if (n_edges(x) == 0) return(mutate_edge_geom(x, st_sfc(crs = st_crs(x))))
  # In any other case draw straight lines between the boundary nodes of edges.
  bounds = edge_boundary_nodes(x, list = TRUE)
  mutate_edge_geom(x, draw_lines(bounds[[1]], bounds[[2]]))
}
