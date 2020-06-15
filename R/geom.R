#' Drop the geometry list column
#'
#' @param x An object of class \code{\link{sfnetwork}}
#'
#' @param active Either 'nodes' or 'edges'. If NULL, the active component of x
#' will be used.
#'
#' @return An object of class \code{\link{sfnetwork}} when active = 'edges', an
#' object of class \code{\link[tidygraph]{tbl_graph}} when active = 'nodes'.
#'
#' @noRd
drop_geometry = function(x, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = drop_node_geometry(x),
    edges = drop_edge_geometry(x),
    stop("Unknown active element: ", active, ". Only nodes and edges supported")
  )
}

drop_node_geometry = function(x) {
  x_new = igraph::delete_vertex_attr(x, sf_attr(x, "sf_column", "nodes"))
  sf_attr(x_new, "sf_column", "nodes") = NULL
  sf_attr(x_new, "agr", "nodes") = NULL
  as_tbl_graph(x_new)
}

drop_edge_geometry = function(x) {
  x_new = igraph::delete_edge_attr(x, sf_attr(x, "sf_column", "edges"))
  sf_attr(x_new, "sf_column", "edges") = NULL
  sf_attr(x_new, "agr", "edges") = NULL
  x_new
}

#' Replace a geometry list column with another geometry list column
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y An object of class \code{\link[sf]{sfc}}.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the active component 
#' of x will be used.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @noRd
replace_geometry = function(x, y, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = replace_node_geometry(x, y),
    edges = replace_edge_geometry(x, y),
    stop("Unknown active element: ", active, ". Only nodes and edges supported")
  )
}

#' @importFrom rlang !! :=
#' @importFrom tidygraph mutate
replace_node_geometry = function(x, y) {
  # Name of the attribute to replace.
  geom_attr = sf_attr(x, "sf_column", "nodes")
  if (is.null(geom_attr)) {
    if (is_spatially_explicit(igraph::vertex_attr(x))) {
      sfc_column = which(sapply(igraph::vertex_attr(x), function(x) is.sfc))
      geom_attr = vertex_attr_names(x)[sfc_column]
    } else {
      geom_attr = "geometry"
    }
  }
  # Replace.
  if (attr(x, "active") == "edges") {
    x = tidygraph::mutate(activate(x, "nodes"), !!geom_attr := y)
    activate(x, "edges")
  } else {
    tidygraph::mutate(x, !!geom_attr := y)
  }
}

#' @importFrom rlang !! :=
#' @importFrom tidygraph mutate
replace_edge_geometry = function(x, y) {
  # Name of the attribute to replace.
  geom_attr = sf_attr(x, "sf_column", "edges")
  if (is.null(geom_attr)) {
    if (is_spatially_explicit(igraph::edge_attr(x))) {
      sfc_column = which(sapply(igraph::edge_attr(x), function(x) is.sfc))
      geom_attr = edge_attr_names(x)[sfc_column]
    } else {
      geom_attr = "geometry"
    }
  }
  # Replace.
  if (attr(x, "active") == "nodes") {
    x = tidygraph::mutate(activate(x, "edges"), !!geom_attr := y)
    activate(x, "nodes")
  } else {
    tidygraph::mutate(x, !!geom_attr := y)
  }
}

#' Validate if a replacement geometry preserves a valid sfnetwork structure
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y An object of class \code{\link[sf]{sfc}}.
#'
#' @param active Either 'nodes' or 'edges'. If \code{NULL}, the active component 
#' of x will be used.
#'
#' @return Nothing when the valid sfnetwork structure is preserved, an error
#' message otherwise.
#'
#' @noRd
validate_geometry = function(x, y, active = NULL) {
  stopifnot(is.sfc(y)) # Invalid input.
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    attr(x, "active"),
    nodes = validate_node_geometry(x, y),
    edges = validate_edge_geometry(x, y),
    stop("Unknown active element: ", active, ". Only nodes and edges supported")
  )
}

validate_node_geometry = function(x, y) {
  # --> Are all node geometries points?
  if (! st_is_all(y, "POINT")) {
    stop("Only geometries of type POINT are allowed as nodes")
  }
  # --> Are the edges of the network spatially implicit?
  if (has_spatially_explicit_edges(x)) {
    stop("Replacing node geometries requires spatially implicit edges")
  }
}

validate_edge_geometry = function(x, y) {
  # --> Are all edge geometries linestrings?
  if (! st_is_all(y, "LINESTRING")) {
    stop("Only geometries of type LINESTRING are allowed as edges")
  }
  # --> Is the CRS of the edges the same as of the network?
  if (! same_crs(as_sf(x, "nodes"), y)) {
    stop("CRS of replacement not equal to network CRS. Run st_transform first?")
  }
  # --> Do the edge boundary points match their corresponding nodes?
  if (is_directed(x)) {
    # Start point should match start node.
    # End point should match end node.
    if (! nodes_match_edge_boundaries(as_sf(x, "nodes"), y)) {
      stop("Boundary points of replacement should match their corresponding nodes")
    }
  } else {
    # Start point should match either start or end node.
    # End point should match either start or end node.
    if (! nodes_in_edge_boundaries(as_sf(x, "nodes"), y)) {
      stop("Boundary points of replacement should match their corresponding nodes")
    }
  }
}