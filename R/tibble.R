#' as_tibble method for sfnetworks
#'
#' The sfnetwork method for \code{\link[tibble]{as_tibble}} is conceptually 
#' different. Whenever a geometry list column is present, it will by default 
#' return what we call a 'spatial tibble'. With that we mean an object of 
#' class \code{c('sf', 'tbl_df')} instead of an object of class 
#' \code{'tbl_df'}. This little conceptual trick is essential for how
#' tidyverse functions handle \code{\link{sfnetwork}} objects, i.e. always 
#' using the corresponding \code{\link[sf]{sf}} method if present.
#'
#' When using \code{\link[tibble]{as_tibble}} on \code{\link{sfnetwork}}
#' objects directly as a user, you can disable this behaviour by setting
#' \code{spatial = FALSE}.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param active Which network element (i.e. nodes or edges) to activate before
#' extracting. If \code{NULL}, it will be set to the current active element of
#' the given network. Defaults to \code{NULL}.
#'
#' @param spatial Should te extracted tibble be a 'spatial tibble', i.e. an
#' object of class \code{c('sf', 'tbl_df')}, if it contains a geometry list
#' column. Defaults to \code{TRUE}. 
#' 
#' @param ... Arguments passed on to \code{\link[tibble]{as_tibble}}.
#'
#' @name as_tibble
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
      nodes = node_spatial_tibble(x),
      edges = edge_spatial_tibble(x),
      raise_unknown_input(active)
    )
  } else {
    switch(
      active,
      nodes = as_tibble(as_tbl_graph(x), "nodes"),
      edges = as_tibble(as_tbl_graph(x), "edges"),
      raise_unknown_input(active)
    )
  }
}

#' @importFrom sf st_as_sf
node_spatial_tibble = function(x) {
  st_as_sf(x, "nodes")
}

#' @importFrom sf st_as_sf
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
edge_spatial_tibble = function(x) {
  if (has_spatially_explicit_edges(x)) {
    st_as_sf(x, "edges") 
  } else {
    as_tibble(as_tbl_graph(x), "edges")
  }
}