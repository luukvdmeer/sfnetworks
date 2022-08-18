#' List-column friendly version of bind_rows
#'
#' @param ... Tables to be row-binded.
#'
#' @details Behaviour of this function should be similar to rbindlist from the
#' data.table package.
#'
#' @importFrom dplyr across bind_rows mutate
#' @noRd
bind_rows_list = function(...) {
  cols_as_list = function(x) list2DF(lapply(x, function(y) unname(as.list(y))))
  ins = lapply(list(...), cols_as_list)
  out = bind_rows(ins)
  is_listcol = vapply(out, function(x) any(lengths(x) > 1), logical(1))
  mutate(out, across(which(!is_listcol), unlist))
}

#' Print a string with a subtle style.
#'
#' @param ... A string to print.
#'
#' @return A printed string to console with subtle style.
#'
#' @importFrom crayon silver
#' @noRd
cat_subtle = function(...) { # nocov start
  # Util function for print method, testing should be up to crayon
  cat(silver(...))
} # nocov end

#' Create edges from nodes
#'
#' @param nodes An object of class \code{\link[sf]{sf}} with \code{POINT}
#' geometries.
#'
#' @details It is assumed that the given POINT geometries form the nodes. Edges
#' need to be created as linestrings between those nodes. It is assumed that
#' the given nodes are connected sequentially.
#'
#' @return A list with the nodes as an object of class \code{\link[sf]{sf}}
#' with \code{POINT} geometries and the created edges as an object of class
#' \code{\link[sf]{sf}} with \code{LINESTRING} geometries.
#'
#' @importFrom sf st_geometry st_sf
#' @noRd
create_edges_from_nodes = function(nodes) {
  # Define indices for source and target nodes.
  source_ids = 1:(nrow(nodes) - 1)
  target_ids = 2:nrow(nodes)
  # Create separate tables for source and target nodes.
  sources = nodes[source_ids, ]
  targets = nodes[target_ids, ]
  # Create linestrings between the source and target nodes.
  edges = st_sf(
    from = source_ids,
    to = target_ids,
    geometry = draw_lines(st_geometry(sources), st_geometry(targets))
  )
  # Use the same sf column name as in the nodes.
  nodes_geom_colname = attr(nodes, "sf_column")
  if (nodes_geom_colname != "geometry") {
    names(edges)[3] = nodes_geom_colname
    attr(edges, "sf_column") = nodes_geom_colname
  }
  # Return a list with both the nodes and edges.
  class(edges) = class(nodes)
  list(nodes = nodes, edges = edges)
}

#' Create nodes from edges
#'
#' @param edges An object of class \code{\link[sf]{sf}} with \code{LINESTRING}
#' geometries.
#'
#' @details It is assumed that the given LINESTRING geometries form the edges.
#' Nodes need to be created at the boundary points of the edges. Identical
#' boundary points should become the same node.
#'
#' @return A list with the edges as an object of class \code{\link[sf]{sf}}
#' with \code{LINESTRING} geometries and the created nodes as an object of
#' class \code{\link[sf]{sf}} with \code{POINT} geometries.
#'
#' @importFrom sf st_sf
#' @noRd
create_nodes_from_edges = function(edges) {
  # Get the boundary points of the edges.
  nodes = linestring_boundary_points(edges)
  # Give each unique location a unique ID.
  indices = st_match(nodes)
  # Define for each endpoint if it is a source or target node.
  is_source = rep(c(TRUE, FALSE), length(nodes) / 2)
  # Define for each edges which node is its source and target node.
  if ("from" %in% colnames(edges)) raise_overwrite("from")
  edges$from = indices[is_source]
  if ("to" %in% colnames(edges)) raise_overwrite("to")
  edges$to = indices[!is_source]
  # Remove duplicated nodes from the nodes table.
  nodes = nodes[!duplicated(indices)]
  # Convert to sf object
  nodes = st_sf(geometry = nodes)
  # Use the same sf column name as in the edges.
  edges_geom_colname = attr(edges, "sf_column")
  if (edges_geom_colname != "geometry") {
    names(nodes)[1] = edges_geom_colname
    attr(nodes, "sf_column") = edges_geom_colname
  }
  # Return a list with both the nodes and edges.
  class(nodes) = class(edges)
  list(nodes = nodes, edges = edges)
}

#' Draw lines between two sets of points, row-wise
#'
#' @param x An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, representing the points where lines need to start at.
#'
#' @param y An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, representing the points where lines need to end at.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{LINESTRING}
#' geometries.
#'
#' @details Lines are drawn row-wise. That is, between the first point in x
#' and the first point in y, the second point in x and the second point in y,
#' et cetera.
#'
#' @importFrom sf st_crs st_crs<- st_precision st_precision<-
#' @importFrom sfheaders sfc_linestring sfc_to_df
#' @noRd
draw_lines = function(x, y) {
  df = rbind(sfc_to_df(x), sfc_to_df(y))
  df = df[order(df$point_id), ]
  lines = sfc_linestring(df, x = "x", y = "y", linestring_id = "point_id")
  st_crs(lines) = st_crs(x)
  st_precision(lines) = st_precision(x)
  lines
}

#' Get the geometries of the boundary nodes of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, of length equal to twice the number of edges in x, and ordered
#' as [start of edge 1, end of edge 1, start of edge 2, end of edge 2, ...].
#'
#' @details Boundary nodes differ from boundary points in the sense that
#' boundary points are retrieved by taking the boundary points of the
#' \code{LINESTRING} geometries of edges, while boundary nodes are retrieved
#' by querying the nodes table of a network with the `to` and `from` columns
#' in the edges table. In a valid network structure, boundary nodes should be
#' equal to boundary points.
#'
#' @importFrom igraph E ends
#' @importFrom sf st_as_sf st_geometry
#' @noRd
edge_boundary_nodes = function(x) {
  nodes = pull_node_geom(x)
  id_mat = ends(x, E(x), names = FALSE)
  id_vct = as.vector(t(id_mat))
  nodes[id_vct]
}

#' Get the indices of the boundary nodes of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param matrix Should te result be returned as a two-column matrix? Defaults
#' to \code{FALSE}.
#'
#' @return If matrix is \code{FALSE}, a numeric vector of length equal to twice
#' the number of edges in x, and ordered as
#' [start of edge 1, end of edge 1, start of edge 2, end of edge 2, ...]. If
#' matrix is \code{TRUE}, a two-column matrix, with the number of rows equal to
#' the number of edges in the network. The first column contains the indices of
#' the start nodes of the edges, the seconds column contains the indices of the
#' end nodes of the edges.
#'
#' @importFrom igraph E ends
#' @noRd
edge_boundary_node_indices = function(x, matrix = FALSE) {
  ends = ends(x, E(x), names = FALSE)
  if (matrix) ends else as.vector(t(ends))
}

#' Get the geometries of the boundary points of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, of length equal to twice the number of edges in x, and ordered
#' as [start of edge 1, end of edge 1, start of edge 2, end of edge 2, ...].
#'
#' @details Boundary points differ from boundary nodes in the sense that
#' boundary points are retrieved by taking the boundary points of the
#' \code{LINESTRING} geometries of edges, while boundary nodes are retrieved
#' by querying the nodes table of a network with the `to` and `from` columns
#' in the edges table. In a valid network structure, boundary nodes should be
#' equal to boundary points.
#'
#' @importFrom sf st_as_sf
#' @noRd
edge_boundary_points = function(x) {
  edges = pull_edge_geom(x)
  linestring_boundary_points(edges)
}

#' Get the node indices of the boundary points of edges in an sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
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
#' @importFrom igraph ecount
#' @importFrom sf st_equals
#' @noRd
edge_boundary_point_indices = function(x, matrix = FALSE) {
    nodes = pull_node_geom(x)
    edges = edges_as_sf(x)
    idxs_lst = st_equals(linestring_boundary_points(edges), nodes)
    idxs_vct = do.call("c", idxs_lst)
    # In most networks the location of a node will be unique.
    # However, this is not a requirement.
    # There may be cases where multiple nodes share the same geometry.
    # Then some more processing is needed to find the correct indices.
    if (length(idxs_vct) != ecount(x) * 2) {
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

#' Make edges spatially explicit
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link{sfnetwork}} with spatially explicit
#' edges.
#'
#' @importFrom rlang !! :=
#' @importFrom sf st_geometry
#' @importFrom tidygraph mutate
#' @noRd
explicitize_edges = function(x) {
  if (has_explicit_edges(x)) {
    x
  } else {
    # Extract the node geometries from the network.
    nodes = pull_node_geom(x)
    # Get the indices of the boundary nodes of each edge.
    # Returns a matrix with source ids in column 1 and target ids in column 2.
    ids = edge_boundary_node_indices(x, matrix = TRUE)
    # Get the boundary node geometries of each edge.
    from = nodes[ids[, 1]]
    to = nodes[ids[, 2]]
    # Draw linestring geometries between the boundary nodes of each edge.
    mutate_edge_geom(x, draw_lines(from, to))
  }
}

#' Get the nearest node to a given feature
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y A single spatial feature as object of class \code{\link[sf]{sf}}
#' or \code{\link[sf]{sfc}}. If multiple features are given only the first one
#' is considered.
#'
#' @return An object of class \code{\link[sf]{sf}} containing a single feature
#' with \code{POINT} geometry.
#'
#' @importFrom sf st_geometry st_nearest_feature
#' @noRd
get_nearest_node = function(x, y) {
  nodes = nodes_as_sf(x)
  nodes[st_nearest_feature(st_geometry(y)[1], nodes), ]
}

#' Get the index of the nearest node to a given feature
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y A single spatial feature as object of class \code{\link[sf]{sf}}
#' or \code{\link[sf]{sfc}}. If multiple features are given only the first one
#' is considered.
#'
#' @return An integer.
#'
#' @importFrom sf st_geometry st_nearest_feature
#' @noRd
get_nearest_node_index = function(x, y) {
  st_nearest_feature(st_geometry(y)[1], nodes_as_sf(x))
}

#' Make edges spatially implicit
#'
#' @param x An object of class \code{\link{sfnetwork}}.

#' @return An object of class \code{\link{sfnetwork}} with spatially implicit
#' edges.
#'
#' @noRd
implicitize_edges = function(x) {
  if (has_explicit_edges(x)) {
    drop_edge_geom(x)
  } else {
    x
  }
}

#' Get the boundary points of linestring geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, of length equal to twice the number of lines in x, and ordered
#' as [start of line 1, end of line 1, start of line 2, end of line 2, ...].
#'
#' @details With boundary points we mean the points at the start and end of
#' a linestring.
#'
#' @importFrom sf st_crs st_crs<- st_geometry st_precision st_precision<-
#' @importFrom sfheaders sfc_point sfc_to_df
#' @noRd
linestring_boundary_points = function(x) {
  # Extract coordinates.
  coords = sfc_to_df(st_geometry(x))
  # Find row-indices of the first and last coordinate pair of each linestring.
  # These are the boundary points.
  first_pair = !duplicated(coords[["sfg_id"]])
  last_pair = !duplicated(coords[["sfg_id"]], fromLast = TRUE)
  idxs = first_pair | last_pair
  # Extract boundary point coordinates.
  pairs = coords[idxs, names(coords) %in% c("x", "y", "z", "m")]
  # Rebuild sf structure.
  points = sfc_point(pairs)
  st_crs(points) = st_crs(x)
  st_precision(points) = st_precision(x)
  points
}

#' Get the segments of linestring geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{LINESTRING}
#' geometries.
#'
#' @details With a line segment we mean a linestring geometry that has no
#' interior points.
#'
#' @importFrom sf st_crs st_crs<- st_geometry st_precision st_precision<-
#' @importFrom sfheaders sfc_linestring sfc_to_df
#' @noRd
linestring_segments = function(x) {
  # Decompose lines into the points that shape them.
  pts = sfc_to_df(st_geometry(x))
  # Define which of the points are a startpoint of a line.
  # Define which of the points are an endpoint of a line.
  is_startpoint = !duplicated(pts[["linestring_id"]])
  is_endpoint = !duplicated(pts[["linestring_id"]], fromLast = TRUE)
  # Extract the coordinates from the points.
  coords = pts[names(pts) %in% c("x", "y", "z", "m")]
  # Extract coordinates of the point that are a startpoint of a segment.
  # Extract coordinates of the point that are an endpoint of a segment.
  src_coords = coords[!is_endpoint, ]
  trg_coords = coords[!is_startpoint, ]
  src_coords$segment_id = seq_len(nrow(src_coords))
  trg_coords$segment_id = seq_len(nrow(trg_coords))
  # Construct the segments.
  segment_pts = rbind(src_coords, trg_coords)
  segment_pts = segment_pts[order(segment_pts$segment_id), ]
  segments = sfc_linestring(segment_pts, linestring_id = "segment_id")
  st_crs(segments) = st_crs(x)
  st_precision(segments) = st_precision(x)
  segments
}

#' Cast multilinestrings to single linestrings.
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{MULTILINESTRING} geometries or a combination of
#' \code{LINESTRING} geometries and \code{MULTILINESTRING} geometries.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{LINESTRING}
#' geometries.
#'
#' @details This may create invalid linestrings according to the simple feature
#' standard, e.g. linestrings may cross themselves.
#'
#' @importFrom sf st_crs st_crs<- st_geometry st_precision st_precision<-
#' @importFrom sfheaders sfc_linestring sfc_to_df
#' @noRd
multilinestrings_to_linestrings = function(x) {
  # Decompose lines into the points that shape them.
  pts = sfc_to_df(st_geometry(x))
  # Add a linestring ID to each of these points.
  # Points of a multilinestring should all have the same ID.
  is_in_multi = !is.na(pts$multilinestring_id)
  pts$linestring_id[is_in_multi] = pts$multilinestring_id[is_in_multi]
  # Select only coordinate and ID columns.
  pts = pts[, names(pts) %in% c("x", "y", "z", "m", "linestring_id")]
  # (Re)create linestring geometries.
  lines = sfc_linestring(pts, linestring_id = "linestring_id")
  st_crs(lines) = st_crs(x)
  st_precision(lines) = st_precision(x)
  lines
}

#' Determine duplicated geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return A logical vector of the same length as \code{x}.
#'
#' @importFrom sf st_equals
#' @noRd
st_duplicated = function(x) {
  dup = rep(FALSE, length(x))
  dup[unique(do.call("c", lapply(st_equals(x), `[`, - 1)))] = TRUE
  dup
}

#' Geometry matching
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return A numeric vector giving for each feature in x the row number of the
#' first feature in x that has equal coordinates.
#'
#' @importFrom sf st_equals
#' @noRd
st_match = function(x) {
  idxs = do.call("c", lapply(st_equals(x), `[`, 1))
  match(idxs, unique(idxs))
}