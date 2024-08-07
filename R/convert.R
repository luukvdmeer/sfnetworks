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
as_tibble.sfnetwork = function(x, active = NULL, spatial = TRUE, ...) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  if (spatial) {
    switch(
      active,
      nodes = nodes_as_sf(x),
      edges = edges_as_table(x),
      raise_invalid_active(active)
    )
  } else {
    switch(
      active,
      nodes = as_tibble(as_tbl_graph(x), "nodes"),
      edges = as_tibble(as_tbl_graph(x), "edges"),
      raise_invalid_active(active)
    )
  }
}

#' Convert a sfnetwork into a S2 geography vector
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
#'
#' @importFrom sf st_geometry
as_s2_geography.sfnetwork = function(x, ...) {
  s2::as_s2_geography(st_geometry(x))
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
  # Extract the vertices of the sfnetwork.
  X_vertices_ppp = spatstat.geom::as.ppp(pull_node_geom(X))
  # Extract the edge list.
  X_edge_list = as.matrix(
    (as.data.frame(activate(X, "edges")))[, c("from", "to")]
  )
  # Build linnet.
  spatstat.linnet::linnet(
    vertices = X_vertices_ppp,
    edges = X_edge_list,
    ...
  )
}
