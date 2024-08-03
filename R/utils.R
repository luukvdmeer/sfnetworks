#' Count the number of nodes or edges in a network
#'
#' @param x An object of class \code{\link{sfnetwork}}, or any other network
#' object inheriting from \code{\link[igraph]{igraph}}.
#'
#' @return An integer.
#'
#' @examples
#' net = as_sfnetwork(roxel)
#' n_nodes(net)
#' n_edges(net)
#'
#' @name n
#' @importFrom igraph vcount
#' @export
n_nodes = function(x) {
  vcount(x)
}

#' @name n
#' @importFrom igraph ecount
#' @export
n_edges = function(x) {
  ecount(x)
}

#' Extract the indices of nodes or edges from a network
#'
#' @param x An object of class \code{\link{sfnetwork}}, or any other network
#' object inheriting from \code{\link[igraph]{igraph}}.
#'
#' @details The indices in these objects are always integers that correspond to
#' rownumbers in respectively the nodes or edges table.
#'
#' @return An vector of integers.
#'
#' @examples
#' net = as_sfnetwork(roxel[1:10, ])
#' node_ids(net)
#' edge_ids(net)
#'
#' @name ids
#' @export
node_ids = function(x) {
  seq_len(n_nodes(x))
}

#' @name ids
#' @export
edge_ids = function(x) {
  seq_len(n_edges(x))
}

#' Extract the nearest nodes or edges to given spatial features
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y Spatial features as object of class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @details To determine the nearest node or edge to each feature in \code{y}
#' the function \code{\link[sf]{st_nearest_feature}} is used. When extracting
#' nearest edges, spatially explicit edges are required, i.e. the edges table
#' should have a geometry column.
#'
#' @return An object of class \code{\link[sf]{sf}} with each row containing
#' the nearest node or edge to the corresponding spatial features in \code{y}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel)
#' pts = st_sample(st_bbox(roxel))
#'
#' nodes = nearest_nodes(net, pts)
#' edges = nearest_edges(net, pts)
#'
#' oldpar = par(no.readonly = TRUE)
#' par(mar = c(1,1,1,1), mfrow = c(1,2))
#'
#' plot(net, main = "Nearest nodes")
#' plot(pts, cex = 2, col = "red", pch = 20, add = TRUE)
#' plot(st_geometry(nodes), cex = 2, col = "orange", pch = 20, add = TRUE)
#'
#' plot(net, main = "Nearest edges")
#' plot(pts, cex = 2, col = "red", pch = 20, add = TRUE)
#' plot(st_geometry(edges), lwd = 2, col = "orange", pch = 20, add = TRUE)
#'
#' par(oldpar)
#'
#' @name nearest
#' @importFrom sf st_geometry st_nearest_feature
#' @export
nearest_nodes = function(x, y) {
  nodes = nodes_as_sf(x)
  nodes[st_nearest_feature(st_geometry(y), nodes), ]
}

#' @name nearest
#' @importFrom sf st_geometry st_nearest_feature
#' @export
nearest_edges = function(x, y) {
  edges = edges_as_sf(x)
  edges[st_nearest_feature(st_geometry(y), edges), ]
}

#' Extract the indices of nearest nodes or edges to given spatial features
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y Spatial features as object of class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}}.
#'
#' @details To determine the nearest node or edge to each feature in \code{y}
#' the function \code{\link[sf]{st_nearest_feature}} is used. When extracting
#' nearest edges, spatially explicit edges are required, i.e. the edges table
#' should have a geometry column.
#'
#' @return An integer vector with each element containing the index of the
#' nearest node or edge to the corresponding spatial features in \code{y}.
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel)
#' pts = st_sample(st_bbox(roxel))
#'
#' nearest_node_ids(net, pts)
#' nearest_edge_ids(net, pts)
#'
#' @name nearest_ids
#' @importFrom sf st_geometry st_nearest_feature
#' @export
nearest_node_ids = function(x, y) {
  st_nearest_feature(st_geometry(y), nodes_as_sf(x))
}

#' @name nearest_ids
#' @importFrom sf st_geometry st_nearest_feature
#' @export
nearest_edge_ids = function(x, y) {
  st_nearest_feature(st_geometry(y), edges_as_sf(x))
}

#' Get the nearest

#' Convert an adjacency matrix into a neighbor list
#'
#' Adjacency matrices of networks are n x n matrices with n being the number of
#' nodes, and element Aij holding a \code{TRUE} value if node i is adjacent to
#' node j, and a \code{FALSE} value otherwise. Neighbor lists are the sparse
#' version of these matrices, coming in the form of a list with one element per
#' node, holding the indices of the nodes it is adjacent to.
#'
#' @param x An adjacency matrix of class \code{\link{matrix}}. Non-logical
#' matrices are first converted into logical matrices using
#' \code{\link{as.logical}}.
#'
#' @return The sparse adjacency matrix as object of class \code{\link{list}}.
#'
#' @noRd
adj2nb = function(x) {
  if (! is.logical(x)) {
    apply(x, 1, \(x) which(as.logical(x)), simplify = FALSE)
  } else {
    apply(x, 1, which, simplify = FALSE)
  }
}

#' Convert a neighbor list into a sfnetwork
#'
#' Neighbor lists are sparse adjacency matrices in list format that specify for
#' each node to which other nodes it is adjacent.
#'
#' @param neighbors A list with one element per node, holding the indices of
#' the nodes it is adjacent to.
#'
#' @param nodes The nodes themselves as an object of class \code{\link[sf]{sf}}
#' or \code{\link[sf]{sfc}} with \code{POINT} geometries.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param edges_as_lines Should the created edges be spatially explicit, i.e.
#' have \code{LINESTRING} geometries stored in a geometry list column? Defaults
#' to \code{TRUE}.
#'
#' @param compute_length Should the geographic length of the edges be stored in
#' a column named \code{length}? Defaults to \code{FALSE}.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @importFrom tibble tibble
#' @noRd
nb2net = function(neighbors, nodes, directed = TRUE, edges_as_lines = TRUE,
                  compute_length = FALSE) {
  # Define the edges by their from and to nodes.
  # An edge will be created between each neighboring node pair.
  edges = rbind(
    rep(c(1:length(neighbors)), lengths(neighbors)),
    do.call("c", neighbors)
  )
  if (! directed && length(edges) > 0) {
    # If the network is undirected:
    # --> Edges i -> j and j -> i are the same.
    # --> We create the network only with unique edges.
    edges = unique(apply(edges, 2, sort), MARGIN = 2)
  }
  # Create the sfnetwork object.
  sfnetwork(
    nodes = nodes,
    edges = tibble(from = edges[1, ], to = edges[2, ]),
    directed = directed,
    edges_as_lines = edges_as_lines,
    compute_length = compute_length,
    force = TRUE
  )
}

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
    if (length(idxs_vct) != n_edges(x) * 2) {
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

#' Extract the edges as a table
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link[sf]{sf}} if the edges are spatially
#' explicit, and object of class \code{\link[tibble]{tibble}}.
#'
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
#' @noRd
edges_as_table = function(x) {
  if (has_explicit_edges(x)) {
    edges_as_sf(x)
  } else {
    as_tibble(as_tbl_graph(x), "edges")
  }
}

#' Make edges spatially explicit
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @return An object of class \code{\link{sfnetwork}} with spatially explicit
#' edges.
#'
#' @importFrom sf st_crs st_geometry st_sfc
#' @importFrom tidygraph mutate
#' @noRd
explicitize_edges = function(x) {
  if (has_explicit_edges(x)) {
    x
  } else {
    # Add empty geometry column if there are no edges.
    if (n_edges(x) == 0) {
      return(mutate_edge_geom(x, st_sfc(crs = st_crs(x))))
    }
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

#' Merge two spatial bounding box objects
#'
#' @param a An object of class \code{\link[sf:st_bbox]{bbox}}.
#'
#' @param b An object of class \code{\link[sf:st_bbox]{bbox}}.
#'
#' @note This function assumes that \code{a} and \code{b} have equal coordinate
#' reference systems.
#'
#' @return An object of class \code{\link[sf:st_bbox]{bbox}} containing the
#' most extreme coordinates of \code{a} and \code{b}.
#'
#' @noRd
merge_bboxes = function(a, b) {
  ab = a
  ab["xmin"] = min(a["xmin"], b["xmin"])
  ab["ymin"] = min(a["ymin"], b["ymin"])
  ab["xmax"] = max(a["xmax"], b["xmax"])
  ab["ymax"] = max(a["ymax"], b["ymax"])
  ab
}

#' Merge two spatial z range objects
#'
#' @param a An object of class \code{\link[sf:st_z_range]{z_range}}.
#'
#' @param b An object of class \code{\link[sf:st_z_range]{z_range}}.
#'
#' @note This function assumes that \code{a} and \code{b} have equal coordinate
#' reference systems.
#'
#' @return An object of class \code{\link[sf:st_z_range]{z_range}} containing
#' the most extreme coordinates of \code{a} and \code{b}.
#'
#' @noRd
merge_zranges = function(a, b) {
  ab = a
  ab["zmin"] = min(a["zmin"], b["zmin"])
  ab["zmax"] = max(a["zmax"], b["zmax"])
  ab
}

#' Merge two spatial m range objects
#'
#' @param a An object of class \code{\link[sf:st_m_range]{m_range}}.
#'
#' @param b An object of class \code{\link[sf:st_m_range]{m_range}}.
#'
#' @note This function assumes that \code{a} and \code{b} have equal coordinate
#' reference systems.
#'
#' @return An object of class \code{\link[sf:st_m_range]{m_range}} containing
#' the most extreme coordinates of \code{a} and \code{b}.
#'
#' @noRd
merge_mranges = function(a, b) {
  ab = a
  ab["mmin"] = min(a["mmin"], b["mmin"])
  ab["mmax"] = max(a["mmax"], b["mmax"])
  ab
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
