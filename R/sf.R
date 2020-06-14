#' @importFrom sf st_as_sf
#' @importFrom tidygraph as_tibble
as_sf = function(x, active = NULL) {
  if (is.sf(x) | is.sfc(x) | is.sfg(x)) {
    return(x)
  }
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = sf::st_as_sf(tidygraph::as_tibble(as_tbl_graph(x), "nodes")),
    edges = sf::st_as_sf(tidygraph::as_tibble(as_tbl_graph(x), "edges")),
    stop("Unknown active element: ", active, ". Only nodes and edges supported")
  )
}

is.sf = function(x) {
  inherits(x, "sf")
}

is.sfc = function(x) {
  inherits(x, "sfc")
}

is.sfg = function(x) {
  inherits(x, "sfg")
}

#' sf methods for sfnetwork objects
#'
#' \code{\link[sf]{sf}} methods for \code{\link{sfnetwork}} objects. Use these 
#' methods without the .sfnetwork suffix and after loading the sf package.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param y An object of class \link{sfnetwork} or class \code{\link[sf]{sf}}.
#' In some cases, it can also be an object of \code{\link[sf]{sfc}},
#' \code{\link[sf:st]{sfg}} or \code{\link[sf:st_bbox]{bbox}}. Always look at
#' the documentation of the corresponding \code{sf} function for details.
#'
#' @param ... Arguments passed on the corresponding \code{sf} function.
#'
#' @param active Which network element (i.e. nodes or edges) to activate before
#' extracting. If \code{NULL}, it will be set to the current active element of
#' the given network. Defaults to \code{NULL}.
#'
#' @param value See \code{\link[sf]{st_crs}} or \code{\link[sf]{st_geometry}}.
#'
#' @param join See \code{\link[sf]{st_join}}.
#'
#' @param left See \code{\link[sf]{st_join}}.
#'
#' @param .predicate See \code{\link[sf]{st_filter}}.
#'
#' @details See the \code{\link[sf]{sf}} documentation.
#'
#' @name sf
#' @importFrom sf st_as_sf
#' @export
st_as_sf.sfnetwork = function(x, active = NULL, ...) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = as_sf(x, "nodes"),
    edges = as_sf(x, "edges"),
    stop("Unknown active element: ", active, ". Only nodes and edges supported")
  )
}

# =============================================================================
# Geometry
# =============================================================================

#' @name sf
#' @importFrom igraph edge_attr vertex_attr
#' @importFrom sf st_geometry
#' @export
st_geometry.sfnetwork = function(x, ...) {
  geom = switch(
    attr(x, "active"),
    nodes = igraph::vertex_attr(x, sf_attr(x, "sf_column", "nodes")),
    edges = igraph::edge_attr(x, sf_attr(x, "sf_column", "edges"))
  )
  if (! is.sfc(geom)) {
    stop(
      "Attribute 'sf_column' does not point to a geometry column.\n",
      "Did you rename it, without setting st_geometry(x) = 'newname'?"
    )
  }
  geom
}

#' @name sf
#' @importFrom sf st_geometry<-
#' @export
`st_geometry<-.sfnetwork` = function(x, value) {
  switch(
    attr(x, "active"),
    nodes = set_node_geom(x, value),
    edges = set_edge_geom(x, value)
  )
}

set_node_geom = function(x, value) {
  if (is.character(value)) {
    col = igraph::vertex_attr(x, value)
    validate_geometry(x, col, "nodes")
    sf_attr(x, "sf_column", "nodes") = value
    x
  } else {
    validate_geometry(x, value, "nodes")
    replace_geometry(x, value, "nodes")
  }
}

set_edge_geom = function(x, value) {
  if (is.character(value)) {
    col = igraph::edge_attr(x, value)
    validate_geometry(x, col, "edges")
    sf_attr(x, "sf_column", "edges") = value
    x
  } else {
    validate_geometry(x, value, "edges")
    replace_geometry(x, value, "edges")
  }
}

#' @name sf
#' @importFrom sf st_bbox
#' @export
st_bbox.sfnetwork = function(x, ...) {
  sf::st_bbox(st_geometry(x), ...)
}

#' @name sf
#' @importFrom sf st_coordinates
#' @export
st_coordinates.sfnetwork = function(x, ...) {
  sf::st_coordinates(st_geometry(x), ...)
}

#' @name sf
#' @importFrom sf st_is
#' @export
st_is.sfnetwork = function(x, ...) {
  sf::st_is(st_geometry(x), ...)
}

# =============================================================================
# CRS
# =============================================================================

#' @name sf
#' @importFrom sf st_crs
#' @export
st_crs.sfnetwork = function(x, ...) {
  sf::st_crs(st_geometry(x), ...)
}

#' @name sf
#' @importFrom sf st_crs<-
#' @export
`st_crs<-.sfnetwork` = function(x, value) {
  switch(
    attr(x, "active"),
    nodes = set_node_crs(x, value),
    edges = set_edge_crs(x, value)
  )
}

set_node_crs = function(x, value) {
  # If edges are spatially explicit, set edge crs as well.
  if (has_spatially_explicit_edges(x)) {
    x = set_element_crs(x, "edges", value)
  }
  set_element_crs(x, "nodes", value)
}

set_edge_crs = function(x, value) {
  # Set node crs as well.
  x = set_element_crs(x, "nodes", value)
  set_element_crs(x, "edges", value)
}

set_element_crs = function(x, element, value) {
  x = switch(
    element,
    nodes = activate(x, "nodes"),
    edges = activate(x, "edges")
  )
  geom = st_geometry(x)
  sf::st_crs(geom) = value
  replace_geometry(x, geom, element)
}

#' @name sf
#' @importFrom sf st_shift_longitude
#' @export
st_shift_longitude.sfnetwork = function(x, ...) {
  change_coords(x, op = sf::st_shift_longitude, ...)
}

#' @name sf
#' @importFrom sf st_transform
#' @export
st_transform.sfnetwork = function(x, ...) {
  change_coords(x, op = sf::st_transform, ...)
}

#' @name sf
#' @importFrom sf st_wrap_dateline
#' @export
st_wrap_dateline.sfnetwork = function(x, ...) {
  change_coords(x, op = sf::st_wrap_dateline, ...)
}

change_coords = function(x, op, ...) {
  switch(
    active(x),
    nodes = change_node_coords(x, op, ...),
    edges = change_edge_coords(x, op, ...)
  )
}

change_node_coords = function(x, op, ...) {
  # If edges are spatially explicit, change edge coords as well.
  if (has_spatially_explicit_edges(x)) {
    x = change_element_coords(x, "edges", op, ...)
  }
  change_element_coords(x, "nodes", op, ...)
}

change_edge_coords = function(x, op) {
  # Change node coords as well.
  x = change_element_coords(x, "nodes", op, ...)
  change_element_coords(x, "edges", op, ...)
}

change_element_coords = function(x, element, op, ...) {
  x = switch(
    element,
    nodes = activate(x, "nodes"),
    edges = activate(x, "edges")
  )
  geom = st_geometry(x)
  new_geom = do.call(match.fun(op), list(geom, ...))
  replace_geometry(x, new_geom, element)
}

# =============================================================================
# Attribute Geometry Relationships
# =============================================================================

#' @name sf
#' @importFrom igraph edge_attr_names vertex_attr_names
#' @importFrom sf st_agr
#' @export
st_agr.sfnetwork = function(x, ...) {
  agr = sf_attr(x, "agr")
  if (attr(x, "active") == "nodes") return(agr)
  unlist(
    list(agr["from"], agr["to"], agr[setdiff(names(agr), c("from", "to"))])
  )
}

#' @name sf
#' @importFrom sf st_agr<- st_agr
#' @export
`st_agr<-.sfnetwork` = function(x, value) {
  x_sf = as_sf(x)
  sf::st_agr(x_sf) = value
  sf_attr(x, "agr") = sf::st_agr(x_sf)
}

empty_agr = function(attr_names) {
  structure(rep(sf::NA_agr_, length(attr_names)), names = attr_names)
}

# =============================================================================
# Geometric binary predicates
# =============================================================================

# Geometric binary predicates internally are applied to the geometry of the 
# given object. Since there is a st_geometry.sfnetwork method, they work
# automatically on sfnetwork objects too. However, st_intersects is the only one
# that is a generic, and thus an sfnetwork method needs to be created for it.

#' @name sf
#' @importFrom sf st_intersects
#' @export
st_intersects.sfnetwork = function(x, y = x, ...) {
  sf::st_intersects(as_sf(x), as_sf(y), ...)
}

# =============================================================================
# Geometric unary operations
# =============================================================================

# NOTE: Only those geometric unary operations y = f(x) are supported in which:
# The geometry type of y is POINT when the geometry type of x is POINT and the
# POINT geometries in y have the same coordinates as their corresponding POINT
# geometries in x (this is basically useless but is what happens when you call
# for example st_reverse on POINT geometries).
# Or:
# The geometry type of y is LINESTRING when the geometry type of x is LINESTRING
# and the LINESTRING geometries in y have the same boundary points (source and
# target may be switched) as their corresponding LINESTRING geometries in x.

#' @name sf
#' @importFrom sf st_reverse
#' @importFrom tidygraph reroute
#' @export
st_reverse.sfnetwork = function(x, ...) {
  if (active(x) == "edges") {
    if (! is_directed(x)) {
      warning("For undirected networks st_reverse has no effect on columns 'to' and 'from'")
    } else {
      warning("For directed networks st_reverse swaps columns 'to' and 'from'")
    }
    node_ids = get_boundary_node_indices(x, out = "both")
    from_ids = node_ids[, 1]
    to_ids = node_ids[, 2]
    x_tbg = tidygraph::reroute(as_tbl_graph(x), from = to_ids, to = from_ids)
    x = tbg_to_sfn(x_tbg)
  } else {
    warning("st_reverse has no effect on nodes. Maybe you want to activate edges?")
  }
  geom_unary_ops(sf::st_reverse, x, ...)
}

#' @name sf
#' @importFrom sf st_simplify
#' @export
st_simplify.sfnetwork = function(x, ...) {
  geom_unary_ops(sf::st_simplify, x, ...)
}

geom_unary_ops = function(op, x, ...) {
  xsf = as_sf(x)
  d_tmp = do.call(match.fun(op), list(xsf, ...))
  replace_geometry(x, sf::st_geometry(d_tmp))
}

# =============================================================================
# Join and filter
# =============================================================================

#' @name sf
#' @importFrom sf st_join
#' @importFrom tidygraph slice
#' @export
st_join.sfnetwork = function(x, y, join = st_intersects, ..., left = TRUE) {
  xsf = as_sf(x)
  ysf = as_sf(y)
  if (".sfnetwork_index" %in% names(xsf)) {
    stop("The attribute name '.sfnetwork_index' is reserved")
  }
  xsf$.sfnetwork_index = seq_len(nrow(xsf))
  d_tmp = sf::st_join(xsf, ysf, join = join, ..., left = left)
  if (active(x) == "nodes") {
    if (has_multiple_matches(d_tmp)) {
      stop("Multiple matches are not allowed when using st_join on the nodes")
    }
  }
  if (! left) {
    keep_ind = d_tmp$.sfnetwork_index
    x = tidygraph::slice(x, keep_ind)
  }
  d_tmp$.sfnetwork_index = NULL
  if (active(x) == "nodes") {
    n_tmp = d_tmp
    e_tmp = as_tibble(x, "edges")
  }
  if (active(x) == "edges") {
    n_tmp = as_tibble(x, "nodes")
    e_tmp = d_tmp
  }
  sfnetwork(n_tmp, e_tmp, directed = is_directed(x), force = TRUE)
}

#' @name sf
#' @importFrom sf st_crop
#' @export
st_crop.sfnetwork = function(x, y, ...) {
  filter_network(sf::st_crop, x, y, ...)
}

#' @name sf
#' @importFrom sf st_filter
#' @export
st_filter.sfnetwork = function(x, y, ..., .predicate = st_intersects) {
  filter_network(sf::st_filter, x, y, ..., .predicate = .predicate)
}

filter_network = function(op, x, y, ...) {
  xsf = as_sf(x)
  ysf = as_sf(y)
  if (".sfnetwork_index" %in% names(xsf)) {
    stop("The attribute name '.sfnetwork_index' is reserved")
  }
  xsf$.sfnetwork_index = seq_len(nrow(xsf))
  d_tmp = do.call(match.fun(op), list(xsf, ysf, ...))
  keep_ind = d_tmp$.sfnetwork_index
  tidygraph::slice(x, keep_ind)
}

#' Query sf attributes from the active element of an sfnetwork object
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param name Name of the attribute to query. If \code{NULL}, then all sf 
#' attributes are returned in a list. Defaults to \code{NULL}.
#'
#' @param active Which network element (i.e. nodes or edges) to activate before
#' extracting. If \code{NULL}, it will be set to the current active element of
#' the given network. Defaults to \code{NULL}.
#'
#' @param value The new value of the attribute, or \code{NULL} to remove the 
#' attribute.
#'
#' @return For the extractor: a list of attributes if \code{name} is \code{NULL},
#' otherwise the value of the attribute matched, or NULL if no exact match is 
#' found and no or more than one partial match is found.
#'
#' @details sf attributes include \code{sf_column} (the name of the sf column)
#' and \code{agr} (the attribute-geometry-relationships).
#'
#' @name sf_attr
#' @importFrom igraph edge_attr vertex_attr
#' @export
sf_attr = function(x, name = NULL, active = NULL) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  if (is.null(name)) {
    switch(
      active,
      nodes = attributes(igraph::vertex_attr(x)),
      edges = attributes(igraph::edge_attr(x))
    )
  } else {
    switch(
      active,
      nodes = attr(igraph::vertex_attr(x), name),
      edges = attr(igraph::edge_attr(x), name)
    )
  }
}

#' @name sf_attr
#' @export
`sf_attr<-` = function(x, name, active = NULL, value) {
  if (is.null(active)) {
    active = attr(x, "active")
  }
  switch(
    active,
    nodes = set_node_sf_attr(x, name, value),
    edges = set_edge_sf_attr(x, name, value)
  )
}

set_node_sf_attr = function(x, name, value) {
  attr(igraph::vertex_attr(x), name) = value
  x
}

set_edge_sf_attr = function(x, name, value) {
  attr(igraph::edge_attr(x), name) = value
  x
}