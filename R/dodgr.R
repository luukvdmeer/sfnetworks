#' Conversion between dodgr streetnets and sfnetworks
#'
#' The \code{\link[dodgr:dodgr-package]{dodgr}} package is designed for routing
#' on directed graphs, and is known for its fast computations of cost matrices,
#' shortest paths, and more. In sfnetwork, dodgr can be chosen as a routing
#' backend.
#'
#' @param x For the conversion to sfnetwork: an object of class
#' \code{\link[dodgr]{dodgr_streetnet}}. For the conversion from sfnetwork: an
#' object of class \code{\link{sfnetwork}}.
#'
#' @param edges_as_lines Should the created edges be spatially explicit, i.e.
#' have \code{LINESTRING} geometries stored in a geometry list column? Defaults
#' to \code{TRUE}.
#'
#' @param weights The edge weights to be stored in the dodgr streetnet.
#' Evaluated by \code{\link{evaluate_weight_spec}}. The default is
#' \code{\link{edge_length}}, which computes the geographic lengths of the
#' edges. Dual-weights can be provided through \code{\link{dual_weights}}.
#'
#' @param time Are the provided weights time values? If \code{TRUE}, they will
#' be stored in a column named 'time' rather than 'd'. Defaults to \code{FALSE}.
#'
#' @note The \code{\link[dodgr:dodgr-package]{dodgr}} package is designed for
#' directed graphs. If the provided \code{\link{sfnetwork}} object is
#' undirected, it is made directed by duplicating and reversing each edge.
#'
#' @return For the conversion to sfnetwork: An object of class
#' \code{\link{sfnetwork}}. For the conversion from sfnetwork: an object of
#' class \code{\link[dodgr]{dodgr_streetnet}}.
#'
#' @name sfnetwork_to_dodgr
NULL

#' @name sfnetwork_to_dodgr
#' @importFrom rlang check_installed
#' @export
dodgr_to_sfnetwork = function(x, edges_as_lines = TRUE) {
  check_installed("dodgr") # Package dodgr is required for this function.
  as_sfnetwork(
    dodgr::dodgr_to_tidygraph(x),
    force = TRUE,
    coords = c("x", "y"),
    crs = 4326,
    edges_as_lines = edges_as_lines
  )
}

#' @name sfnetwork_to_dodgr
#' @importFrom igraph is_directed
#' @importFrom rlang enquo
#' @importFrom sf st_coordinates st_drop_geometry st_transform
#' @export
sfnetwork_to_dodgr = function(x, weights = edge_length(), time = FALSE) {
  # Extract node geometries and edge data.
  # Note that dodgr requires coordinates to be in EPSG:4326.
  node_geom = st_transform(pull_node_geom(x), 4326)
  node_coords = st_coordinates(node_geom)
  edges = st_drop_geometry(edge_data(x, focused = FALSE))
  # Parse the given edge weights.
  # Dual-weights can be given through a dual-weights object.
  weights = evaluate_weight_spec(x, enquo(weights))
  if (inherits(weights, "dual_weights")) {
    dual = TRUE
    d = weights$reported
    w = weights$actual
  } else {
    dual = FALSE
    d = weights
  }
  # Initialize the output data frame.
  # If x is undirected:
  # --> It is made directed by duplicating and reversing each edge.
  if (is_directed(x)) {
    fids = edges$from
    tids = edges$to
    out = edges[, -c(1, 2)]
  } else {
    fids = c(edges$from, edges$to)
    tids = c(edges$to, edges$from)
    d = rep(d, 2)
    if (dual) w = rep(w, 2)
    out = edges[rep(seq_len(nrow(edges)), 2), -c(1, 2)]
  }
  # Fill the output data frame.
  if (time) {
    if (dual) out$time_weighted = w
    out$time = d
  } else {
    if (dual) out$d_weighted = w
    out$d = d
  }
  out$to_lat = node_coords[, "Y"][tids]
  out$to_lon = node_coords[, "X"][tids]
  out$to_id = as.character(tids)
  out$from_lat = node_coords[, "Y"][fids]
  out$from_lon = node_coords[, "X"][fids]
  out$from_id = as.character(fids)
  # Invert column order.
  out = out[, order(ncol(out):1)]
  # Return as a dodgr_streetnet.
  class(out) = c("dodgr_streetnet", "data.frame")
  out
}

#' @importFrom igraph as_edgelist is_directed
sfnetwork_to_minimal_dodgr = function(x, weights, direction = "out") {
  edgelist = as_edgelist(x, names = FALSE)
  if (inherits(weights, "dual_weights")) {
    dual = TRUE
    d = weights$reported
    w = weights$actual
  } else {
    dual = FALSE
    d = weights
  }
  if (!is_directed(x) | direction == "all") {
    x_dodgr = data.frame(
      from = as.character(c(edgelist[, 1], edgelist[, 2])),
      to = as.character(c(edgelist[, 2], edgelist[, 1])),
      d = rep(d, 2)
    )
    if (dual) x_dodgr$w = rep(w, 2)
  } else {
    if (direction == "out") {
      x_dodgr = data.frame(
        from = as.character(edgelist[, 1]),
        to = as.character(edgelist[, 2]),
        d = d
      )
    } else if (direction == "in") {
      x_dodgr = data.frame(
        from = as.character(edgelist[, 2]),
        to = as.character(edgelist[, 1]),
        d = d
      )
    } else {
      raise_unknown_input("direction", direction, c("out", "in", "all"))
    }
    if (dual) x_dodgr$w = w
  }
  x_dodgr
}