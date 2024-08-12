#' Extract the active element of a sfnetwork as spatial tibble
#'
#' The sfnetwork method for \code{\link[tibble]{as_tibble}} is conceptually
#' different. Whenever a geometry list column is present, it will by default
#' return what we call a 'spatial tibble'. With that we mean an object of
#' class \code{c('sf', 'tbl_df')} instead of an object of class
#' \code{'tbl_df'}. This little conceptual trick is essential for how
#' tidyverse functions handle \code{\link{sfnetwork}} objects, i.e. always
#' using the corresponding \code{\link[sf]{sf}} method if present. When using
#' \code{\link[tibble]{as_tibble}} on \code{\link{sfnetwork}} objects directly
#' as a user, you can disable this behaviour by setting \code{spatial = FALSE}.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param active Which network element (i.e. nodes or edges) to activate before
#' extracting. If \code{NULL}, it will be set to the current active element of
#' the given network. Defaults to \code{NULL}.
#'
#' @param focused Should only features that are in focus be extracted? Defaults
#' to \code{TRUE}. See \code{\link[tidygraph]{focus}} for more information on
#' focused networks.
#'
#' @param spatial Should the extracted tibble be a 'spatial tibble', i.e. an
#' object of class \code{c('sf', 'tbl_df')}, if it contains a geometry list
#' column. Defaults to \code{TRUE}.
#'
#' @param ... Arguments passed on to \code{\link[tibble]{as_tibble}}.
#'
#' @return The active element of the network as an object of class
#' \code{\link[sf]{sf}} if a geometry list column is present and
#' \code{spatial = TRUE}, and object of class \code{\link[tibble]{tibble}}
#' otherwise.
#'
#' @name as_tibble
#'
#' @examples
#' library(tibble, quietly = TRUE)
#'
#' net = as_sfnetwork(roxel)
#'
#' # Extract the active network element as a spatial tibble.
#' as_tibble(net)
#'
#' # Extract any network element as a spatial tibble.
#' as_tibble(net, "edges")
#'
#' # Extract the active network element as a regular tibble.
#' as_tibble(net, spatial = FALSE)
#'
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
#' @export
as_tibble.sfnetwork = function(x, active = NULL, focused = TRUE,
                               spatial = TRUE, ...) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  if (spatial) {
    switch(
      active,
      nodes = nodes_as_spatial_tibble(x, focused = focused, ...),
      edges = edges_as_spatial_tibble(x, focused = focused, ...),
      raise_invalid_active(active)
    )
  } else {
    switch(
      active,
      nodes = nodes_as_regular_tibble(x, focused = focused, ...),
      edges = edges_as_regular_tibble(x, focused = focused, ...),
      raise_invalid_active(active)
    )
  }
}

#' @importFrom sf st_as_sf
nodes_as_spatial_tibble = function(x, focused = FALSE, ...) {
  st_as_sf(
    nodes_as_regular_tibble(x, focused = focused, ...),
    agr = node_agr(x),
    sf_column_name = node_geom_colname(x)
  )
}

#' @importFrom sf st_as_sf
edges_as_spatial_tibble = function(x, focused = FALSE, ...) {
  if (has_explicit_edges(x)) {
    st_as_sf(
      edges_as_regular_tibble(x, focused = focused, ...),
      agr = edge_agr(x),
      sf_column_name = edge_geom_colname(x)
    )
  } else {
    edges_as_regular_tibble(x, ...)
  }
}

#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
nodes_as_regular_tibble = function(x, focused = FALSE, ...) {
  as_tibble(as_tbl_graph(x), active = "nodes", focused = focused, ...)
}

#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
edges_as_regular_tibble = function(x, focused = FALSE, ...) {
  as_tibble(as_tbl_graph(x), active = "edges", focused = focused, ...)
}

#' Extract the geometries of a sfnetwork as a S2 geography vector
#'
#' A method to convert an object of class \code{\link{sfnetwork}} into
#' \code{\link[s2]{s2_geography}} format. Use this method without the
#' .sfnetwork suffix and after loading the \pkg{s2} package.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments passed on the corresponding \code{s2} function.
#'
#' @return An object of class \code{\link[s2]{s2_geography}}.
#'
#' @name as_s2_geography
as_s2_geography.sfnetwork = function(x, focused = TRUE, ...) {
  s2::as_s2_geography(pull_geom(x, focused = focused), ...)
}

#' Convert a sfnetwork into a linnet
#'
#' A method to convert an object of class \code{\link{sfnetwork}} into
#' \code{\link[spatstat.linnet]{linnet}} format and enhance the
#' interoperability between \code{sfnetworks} and \code{spatstat}. Use
#' this method without the .sfnetwork suffix and after loading the
#' \pkg{spatstat} package.
#'
#' @param X An object of class \code{\link{sfnetwork}} with a projected CRS.
#'
#' @param ... Arguments passed to \code{\link[spatstat.linnet]{linnet}}.
#'
#' @return An object of class \code{\link[spatstat.linnet]{linnet}}.
#'
#' @seealso \code{\link{as_sfnetwork}} to convert objects of class
#' \code{\link[spatstat.linnet]{linnet}} into objects of class
#' \code{\link{sfnetwork}}.
#'
#' @importFrom rlang check_installed is_installed
#' @name as.linnet
as.linnet.sfnetwork = function(X, ...) {
  # Check the presence and the version of spatstat.geom and spatstat.linnet
  check_installed("spatstat.geom")
  check_installed("spatstat.linnet")
  check_installed("sf (>= 1.0)")
  if (is_installed("spatstat")) check_installed("spatstat (>= 2.0)")
  # Convert nodes to ppp.
  V = spatstat.geom::as.ppp(pull_node_geom(x))
  # Extract the edge list.
  E = as.matrix(edges_as_regular_tibble(x)[, c("from", "to")])
  # Build linnet.
  spatstat.linnet::linnet(vertices = V, edges = E, ...)
}
