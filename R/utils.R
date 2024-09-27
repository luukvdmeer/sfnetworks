#' Determine duplicated geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return A logical vector specifying for each feature in \code{x} if its
#' geometry is equal to a previous feature in \code{x}.
#'
#' @seealso \code{\link{duplicated}}
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' p1 = st_sfc(st_point(c(1, 1)))
#' p2 = st_sfc(st_point(c(0, 0)))
#' p3 = st_sfc(st_point(c(1, 0)))
#'
#' st_duplicated(c(p1, p2, p2, p3, p1))
#'
#' @importFrom sf st_equals st_geometry
#' @export
st_duplicated = function(x) {
  dup = rep(FALSE, length(st_geometry(x)))
  dup[unique(do.call("c", lapply(st_equals(x), `[`, - 1)))] = TRUE
  dup
}

#' @importFrom sf st_geometry
#' @importFrom sfheaders sfc_to_df
st_duplicated_points = function(x, precision = attr(x, "precision")) {
  x_df = sfc_to_df(st_geometry(x))
  coords = x_df[, names(x_df) %in% c("x", "y", "z", "m")]
  st_duplicated_points_df(coords, precision = precision)
}

st_duplicated_points_df = function(x, precision = NULL) {
  x_trim = lapply(x, round, digits = precision_digits(precision))
  x_concat = do.call(paste, x_trim)
  duplicated(x_concat)
}

#' Geometry matching
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return A numeric vector giving for each feature in \code{x} the position of
#' the first feature in \code{x} that has an equal geometry.
#'
#' @seealso \code{\link{match}}
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' p1 = st_sfc(st_point(c(1, 1)))
#' p2 = st_sfc(st_point(c(0, 0)))
#' p3 = st_sfc(st_point(c(1, 0)))
#'
#' st_match(c(p1, p2, p2, p3, p1))
#'
#' @importFrom sf st_equals
#' @export
st_match = function(x) {
  idxs = do.call("c", lapply(st_equals(x), `[`, 1))
  match(idxs, unique(idxs))
}

#' @importFrom sf st_geometry
#' @importFrom sfheaders sfc_to_df
st_match_points = function(x, precision = attr(x, "precision")) {
  x_df = sfc_to_df(st_geometry(x))
  coords = x_df[, names(x_df) %in% c("x", "y", "z", "m")]
  st_match_points_df(coords, precision = precision)
}

st_match_points_df = function(x, precision = NULL) {
  x_trim = lapply(x, round, digits = precision_digits(precision))
  x_concat = do.call(paste, x_trim)
  match(x_concat, unique(x_concat))
}

#' Rounding of geometry coordinates
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @param digits Integer indicating the number of decimal places to be used.
#'
#' @return An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with rounded coordinates.
#'
#' @seealso \code{\link{round}}
#'
#' @examples
#' library(sf, quietly = TRUE)
#'
#' p1 = st_sfc(st_point(c(1.123, 1.123)))
#' p2 = st_sfc(st_point(c(0.789, 0.789)))
#' p3 = st_sfc(st_point(c(1.123, 0.789)))
#'
#' st_round(c(p1, p2, p2, p3, p1), digits = 1)
#'
#' @importFrom sf st_as_binary st_as_sfc st_geometry st_geometry<-
#' st_precision<-
#' @export
st_round = function(x, digits = 0) {
  x_geom = st_geometry(x)
  st_precision(x_geom) = 10^digits
  x_geom_rounded = st_as_sfc(st_as_binary(x_geom))
  st_geometry(x) = x_geom_rounded
  x
}

#' Convert a sfc object into a sf object.
#'
#' @param x An object of class \code{\link[sf]{sfc}}.
#'
#' @param colname The name that should be given to the geometry column.
#'
#' @return An object of class \code{\link[sf]{sf}}.
#'
#' @importFrom sf st_as_sf
#' @noRd
sfc_to_sf = function(x, colname = "geometry") {
  x_sf = st_as_sf(x)
  names(x_sf) = colname
  attr(x_sf, "sf_column") = colname
  x_sf
}

#' Convert a sfheaders data frame into sfc point geometries
#'
#' @param x_df An object of class \code{\link{data.frame}} as constructed by
#' the \pkg{sfheaders} package.
#'
#' @param x_sf The object of class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}} from which \code{x_df} was constructed. This is used
#' to copy the CRS and the precision to the new geometries.
#'
#' @param select Should coordinate columns first be selected from the given
#' data frame? If \code{TRUE}, columns with names "x", "y", "z" and "m" will
#' first be selected from the data frame. If \code{FALSE}, it is assumed the
#' data frame only contains these columns in exactly that order. Defaults to
#' \code{TRUE}.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries.
#'
#' @importFrom sf st_crs st_crs<- st_precision st_precision<-
#' @importFrom sfheaders sfc_point
#' @noRd
df_to_points = function(x_df, x_sf, select = TRUE) {
  if (select) x_df = x_df[, names(x_df) %in% c("x", "y", "z", "m")]
  pts = sfc_point(x_df)
  st_crs(pts) = st_crs(x_sf)
  st_precision(pts) = st_precision(x_sf)
  pts
}

#' Convert a sfheaders data frame into sfc linestring geometries
#'
#' @param x_df An object of class \code{\link{data.frame}} as constructed by
#' the \pkg{sfheaders} package.
#'
#' @param x_sf The object of class \code{\link[sf]{sf}} or
#' \code{\link[sf]{sfc}} from which \code{x_df} was constructed. This is used
#' to copy the CRS and the precision to the new geometries.
#'
#' @param id_col The name of the column in \code{x_df} that identifies which
#' row belongs to which linestring.
#'
#' @param select Should coordinate columns first be selected from the given
#' data frame? If \code{TRUE}, columns with names "x", "y", "z" and "m" will
#' first be selected from the data frame, alongside the specified index column.
#' If \code{FALSE}, it is assumed that the data frame besides the specified
#' index columns only contains these coordinate columns in exactly that order.
#' Defaults to \code{TRUE}.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{LINESTRING}
#' geometries.
#'
#' @importFrom sf st_crs st_crs<- st_precision st_precision<-
#' @importFrom sfheaders sfc_linestring
#' @noRd
df_to_lines = function(x_df, x_sf, id_col = "linestring_id", select = TRUE) {
  if (select) x_df = x_df[, names(x_df) %in% c("x", "y", "z", "m", id_col)]
  lns = sfc_linestring(x_df, linestring_id = id_col)
  st_crs(lns) = st_crs(x_sf)
  st_precision(lns) = st_precision(x_sf)
  lns
}

#' Get the boundary points of linestring geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @param return_df Should a data frame with one column per coordinate be
#' returned instead of a \code{\link[sf]{sfc}} object? Defaults to
#' \code{FALSE}.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, of length equal to twice the number of lines in x, and ordered
#' as [start of line 1, end of line 1, start of line 2, end of line 2, ...].
#' If \code{return_df = TRUE}, a data frame with one column per coordinate is
#' returned instead, with number of rows equal to twice the number of lines in
#' x.
#'
#' @details With boundary points we mean the points at the start and end of
#' a linestring.
#'
#' @importFrom sf st_geometry
#' @importFrom sfheaders sfc_to_df
#' @noRd
linestring_boundary_points = function(x, return_df = FALSE) {
  coords = sfc_to_df(st_geometry(x))
  is_start = !duplicated(coords[["linestring_id"]])
  is_end = !duplicated(coords[["linestring_id"]], fromLast = TRUE)
  is_bound = is_start | is_end
  bounds = coords[is_bound, names(coords) %in% c("x", "y", "z", "m")]
  if (return_df) return (bounds)
  df_to_points(bounds, x, select = FALSE)
}

#' Get the start points of linestring geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @param return_df Should a data frame with one column per coordinate be
#' returned instead of a \code{\link[sf]{sfc}} object? Defaults to
#' \code{FALSE}.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, of length equal to the number of lines in x. If
#' \code{return_df = TRUE}, a data frame with one column per coordinate is
#' returned instead, with number of rows equal to the number of lines in x.
#'
#' @importFrom sf st_geometry
#' @importFrom sfheaders sfc_to_df
#' @noRd
linestring_start_points = function(x, return_df = FALSE) {
  coords = sfc_to_df(st_geometry(x))
  is_start = !duplicated(coords[["linestring_id"]])
  starts = coords[is_start, names(coords) %in% c("x", "y", "z", "m")]
  if (return_df) return (starts)
  df_to_points(starts, x, select = FALSE)
}

#' Get the end points of linestring geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}
#' with \code{LINESTRING} geometries.
#'
#' @param return_df Should a data frame with one column per coordinate be
#' returned instead of a \code{\link[sf]{sfc}} object? Defaults to
#' \code{FALSE}.
#'
#' @return An object of class \code{\link[sf]{sfc}} with \code{POINT}
#' geometries, of length equal to the number of lines in x. If
#' \code{return_df = TRUE}, a data frame with one column per coordinate is
#' returned instead, with number of rows equal to the number of lines in x.
#'
#' @importFrom sf st_geometry
#' @importFrom sfheaders sfc_to_df
#' @noRd
linestring_end_points = function(x ,return_df = FALSE) {
  coords = sfc_to_df(st_geometry(x))
  is_end = !duplicated(coords[["linestring_id"]], fromLast = TRUE)
  ends = coords[is_end, names(coords) %in% c("x", "y", "z", "m")]
  if (return_df) return (ends)
  df_to_points(ends, x, select = FALSE)
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
#' @importFrom sf st_geometry
#' @importFrom sfheaders sfc_to_df
#' @noRd
linestring_segments = function(x) {
  # Decompose lines into the points that shape them.
  line_points = sfc_to_df(st_geometry(x))
  # Define which of the points are a startpoint of a line.
  # Define which of the points are an endpoint of a line.
  is_start = !duplicated(line_points[["linestring_id"]])
  is_end = !duplicated(line_points[["linestring_id"]], fromLast = TRUE)
  # Extract coordinates of the point that are a startpoint of a segment.
  # Extract coordinates of the point that are an endpoint of a segment.
  segment_starts = line_points[!is_end, ]
  segment_ends = line_points[!is_start, ]
  segment_starts$segment_id = seq_len(nrow(segment_starts))
  segment_ends$segment_id = seq_len(nrow(segment_ends))
  # Construct the segments.
  segment_points = rbind(segment_starts, segment_ends)
  segment_points = segment_points[order(segment_points$segment_id), ]
  df_to_lines(segment_points, x, id_col = "segment_id")
}

#' Forcefully cast multilinestrings to single linestrings.
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
#' @importFrom sf st_geometry
#' @importFrom sfheaders sfc_to_df
#' @noRd
force_multilinestrings_to_linestrings = function(x) {
  # Decompose lines into the points that shape them.
  pts = sfc_to_df(st_geometry(x))
  # Add a linestring ID to each of these points.
  # Points of a multilinestring should all have the same ID.
  is_in_multi = !is.na(pts$multilinestring_id)
  pts$linestring_id[is_in_multi] = pts$multilinestring_id[is_in_multi]
  # (Re)create linestring geometries.
  df_to_lines(pts, x, id_col = "linestring_id")
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
  all_points = rbind(sfc_to_df(x), sfc_to_df(y))
  all_points = all_points[order(all_points$point_id), ]
  df_to_lines(all_points, x, id_col = "point_id")
}

#' Merge multiple linestring geometries into one linestring
#'
#' @param x An object of class \code{\link[sf]{sfc}} with \code{LINESTRING}
#' geometries.
#'
#' @details If linestrings share endpoints they will be connected and form a
#' single linestring. If there are multiple disconnected components the result
#' will be a multi-linestring. If \code{x} does not contain any geometries, the
#' result will be an empty linestring.
#'
#' @return An object of class \code{\link[sf]{sfc}} with a single
#' \code{LINESTRING} or \code{MULTILINESTRING} geometry.
#'
#' @importFrom sf st_crs st_linestring st_line_merge st_sfc
#' @noRd
merge_lines = function(x) {
  if (length(x) == 0) {
    st_sfc(st_linestring(), crs = st_crs(x))
  } else {
    st_line_merge(st_combine(x))
  }
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

#' Infer the number of decimal places from a fixed precision scale factor
#'
#' @param x A fixed precision scale factor.
#'
#' @details For more information on fixed precision scale factors see
#' \code{\link[sf]{st_as_binary}}. When the precision scale factor is 0
#' or not defined, sfnetworks defaults to 12 decimal places.
#'
#' @return A numeric value specifying the number of decimal places.
#'
#' @importFrom cli cli_abort
#' @noRd
precision_digits = function(x) {
  if (is.null(x) || x == 0) return (12)
  if (x > 0) return (log(x, 10))
  cli_abort("Currently sfnetworks does not support negative precision")
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
