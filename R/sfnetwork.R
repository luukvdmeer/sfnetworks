#' Create an sfnetwork object
#'
#' \code{sfnetwork} is a tidy data structure for geospatial networks. It extends
#' the graph manipulation functionalities of the
#' \code{\link[tidygraph]{tidygraph-package}} package into the domain of
#' geospatial networks, where the nodes and edges are embedded in geographical 
#' space, and enables to apply the spatial analytical functions from the 
#' \code{\link[sf:sf]{sf-package}} directly to the network.
#'
#' @param nodes An object containing information about the nodes in the network.
#' The nodes should contain geospatial coordinates, either by being an object of
#' class \code{\link[sf]{sf}} with \code{POINT} geometry features, or by being 
#' convertible to such an object with \code{\link[sf]{st_as_sf}}.
#'
#' @param edges An object containing information about the edges in the network.
#' This object may contain explicit geospatial information by being an object of
#' class \code{\link[sf]{sf}} with \code{LINESTRING} geometry features. It may 
#' also be a regular \code{data.frame} or \code{tbl_df} object. In any case,
#' the adjacent nodes of each edge must either be encoded in a \code{to} and
#' \code{from} column, or in the two first columns, as integers. These integers
#' refer to nodes indices, which in turn refer to the position of a node in the
#' nodes table.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param node_key The name of the column in the nodes table that character
#' represented \code{to} and \code{from} columns should be matched against. If
#' NA the first column is always chosen. This setting has no effect if \code{to}
#' and \code{from} are given as integers. Defaults to \code{'name'}.
#'
#' @param edges_as_lines Should the edges be spatially explicit, i.e. have
#' \code{LINESTRING} geometries stored in a geometry list column? If \code{NULL},
#' this will be automatically defined, by setting the argument to \code{TRUE}
#' when the given edges object contains a geometry list column, and \code{FALSE}
#' otherwise. Defaults to \code{NULL}.
#'
#' @param force Should network validity checks be skipped? Defaults to
#' \code{FALSE}, meaning that network validity checks are executed when
#' constructing the network. These checks guarantee a valid spatial network
#' structure. For the nodes, this means that they all should have \code{POINT}
#' geometries. In the case of spatially explicit edges, it is also checked that
#' all edges have \code{LINESTRING} geometries, nodes and edges have the same
#' CRS and boundary points of edges match their corresponding node coordinates.
#' These checks are important, but also time consuming. If you are already sure
#' your input data meet the requirements, the checks are unneccesary and can be
#' turned off to improve performance.
#'
#' @param ... Arguments passed on to \code{\link[sf]{st_as_sf}}, if nodes
#' need to be converted into an \code{sf} object during construction.
#'
#' @return An object of class \code{sfnetwork}.
#'
#' @examples
#' # Create sfnetwork from sf objects
#' p1 = sf::st_point(c(7, 51))
#' p2 = sf::st_point(c(7, 52))
#' p3 = sf::st_point(c(8, 52))
#' nodes = sf::st_as_sf(sf::st_sfc(p1, p2, p3, crs = 4326))
#'
#' e1 = sf::st_cast(sf::st_union(p1,p2), "LINESTRING")
#' e2 = sf::st_cast(sf::st_union(p1,p3), "LINESTRING")
#' e3 = sf::st_cast(sf::st_union(p2,p3), "LINESTRING")
#' edges = sf::st_as_sf(sf::st_sfc(e1, e2, e3, crs = 4326))
#' edges$from = c(1, 1, 2)
#' edges$to = c(2, 3, 3)
#'
#' ## directed network
#' sfnetwork(nodes, edges, directed = TRUE)
#'
#' ## undirected network
#' sfnetwork(nodes, edges, directed = FALSE)
#'
#' ## spatially implicit edges
#' sfnetwork(nodes, edges, directed = FALSE, edges_as_lines = FALSE)
#'
#' @importFrom tidygraph tbl_graph
#' @export
sfnetwork = function(nodes, edges, directed = TRUE, node_key = "name",
                     edges_as_lines = NULL, force = FALSE, ...) {
  # Prepare nodes.
  # If nodes is not an sf object:
  # --> Try to convert it to an sf object.
  # --> Arguments passed in ... will be passed on to st_as_sf.
  if (! is.sf(nodes)) {
    tryCatch(
      sf::st_as_sf(nodes, ...),
      error = function(e) {
        stop("Failed to convert nodes to sf object because: ", e, call. = FALSE)
      }
    )
  }
  node_sf_attrs = attrs_from_sf(nodes)
  # Prepare edges.
  # If edges is not an sf object but does have a geometry list column:
  # --> First convert it to sf such that the required attributes are present.
  # --> Then proceed as described below.
  # If edges is an sf object:
  # --> Tidygraph cannot handle it due to sticky geometry.
  # --> Therefore it has to be converted into a regular data frame (or tibble).
  if (has_sfc(edges)) {
    if (! is.sf(edges)) edges = sf::st_as_sf(edges)
    edge_sf_attrs = attrs_from_sf(edges)
    class(edges) = setdiff(class(edges), "sf")
    # When edges_as_lines was not set, set to TRUE.
    if (is.null(edges_as_lines)) edges_as_lines = TRUE
  } else {
    edge_sf_attrs = NULL
    # When edges_as_lines was not set, set to FALSE.
    if (is.null(edges_as_lines)) edges_as_lines = FALSE
  }
  # Create network.
  # Store sf attributes of the nodes and edges in a special graph attribute.
  x_tbg = tidygraph::tbl_graph(nodes, edges, directed, node_key)
  x_sfn = structure(
    x_tbg,
    class = c("sfnetwork", class(x_tbg)),
    sf = list(nodes = node_sf_attrs, edges = edge_sf_attrs)
  )
  # Post-process network.
  if (edges_as_lines) {
    # Run validity check before explicitizing edges.
    if (! force) require_valid_network_structure(x_sfn)
    # Add edge geometries if needed.
    x_sfn = explicitize_edges(x_sfn)
    # Update agr factor of edges.
    # Because positions of from and to columns were moved during construction.
    edge_agr(x_sfn) = updated_edge_agr(x_sfn)
  } else {
    # Remove edge geometries if needed.
    x_sfn = implicitize_edges(x_sfn)
    # Run validity check after implicitizing edges.
    if (! force) require_valid_network_structure(x_sfn)
  }
  x_sfn
}

#' Convert a foreign object to an sfnetwork object
#'
#' Convert a given object into an object of class \code{\link{sfnetwork}}.
#' If an object can be read by \code{\link[tidygraph]{as_tbl_graph}} and the
#' nodes can be read by \code{\link[sf]{st_as_sf}}, it is automatically
#' supported by sfnetworks.
#'
#' @param x Object to be converted into an \code{\link{sfnetwork}} object.
#'
#' @param ... Arguments passed on to the \code{\link{sfnetwork}} construction
#' function.
#'
#' @return An object of class \code{\link{sfnetwork}}.
#'
#' @export
as_sfnetwork = function(x, ...) {
  UseMethod("as_sfnetwork")
}

#' @name as_sfnetwork
#' @importFrom tidygraph as_tbl_graph
#' @export
as_sfnetwork.default = function(x, ...) {
  as_sfnetwork(tidygraph::as_tbl_graph(x), ...)
}

#' @name as_sfnetwork
#' @examples
#' # Examples for linnet method
#' if (require(spatstat)) {
#' plot(simplenet, main = "spatstat input")
#' simplenet_as_sfnetwork = as_sfnetwork(simplenet)
#' plot(simplenet_as_sfnetwork, main = "sfnetworks output")
#' }
#' @export
as_sfnetwork.linnet = function(x, ...) {
  # The easiest approach is the same as for psp objects, i.e. converting the
  # linnet object into a psp format and then applying the corresponding method.
  if (!requireNamespace("spatstat", quietly = TRUE)) {
    stop("Package spatstat required, please install it first", call. = FALSE)
  }
  x_psp = spatstat::as.psp(x)
  as_sfnetwork(x_psp, ...)
}

#' @name as_sfnetwork
#' @examples
#' # Examples for psp method
#' if (require(spatstat)) {
#' set.seed(42)
#' test_psp = psp(runif(10), runif(10), runif(10), runif(10), window=owin())
#' plot(test_psp, main = "spatstat input")
#' test_psp_as_sfnetwork = as_sfnetwork(test_psp)
#' plot(test_psp_as_sfnetwork, main = "sfnetworks output")
#' }
#' @importFrom sf st_as_sf st_collection_extract
#' @export
as_sfnetwork.psp = function(x, ...) {
  # The easiest method for transforming a Line Segment Pattern (psp) object
  # into sfnetwork format is to transform it into sf format and then apply
  # the usual methods.
  x_sf = sf::st_as_sf(x)
  # x_sf is an sf object composed by 1 POLYGON (the window of the psp object)
  # and several LINESTRINGs (the line segments). I'm not sure if and how we can
  # use the window object so I will extract only the LINESTRINGs.
  x_linestring = sf::st_collection_extract(x_sf, "LINESTRING")
  # Apply as_sfnetwork.sf.
  as_sfnetwork(x_linestring, ...)
}

#' @name as_sfnetwork
#' @examples
#' # Examples for sf method
#' ## from POINT geometries
#' p1 = sf::st_point(c(7, 51))
#' p2 = sf::st_point(c(7, 52))
#' p3 = sf::st_point(c(8, 52))
#' points = sf::st_as_sf(sf::st_sfc(p1, p2, p3, crs = 4326))
#' as_sfnetwork(points)
#' ## from LINESTRING geometries
#' e1 = sf::st_cast(sf::st_union(p1,p2), "LINESTRING")
#' e2 = sf::st_cast(sf::st_union(p1,p3), "LINESTRING")
#' e3 = sf::st_cast(sf::st_union(p2,p3), "LINESTRING")
#' lines = sf::st_as_sf(sf::st_sfc(e1, e2, e3, crs = 4326))
#' as_sfnetwork(lines)
#' @importFrom sf st_geometry
#' @export
as_sfnetwork.sf = function(x, ...) {
  if (has_single_geom_type(x, "LINESTRING")) {
    # Workflow:
    # It is assumed that the given LINESTRING geometries form the edges.
    # Nodes need to be created at the boundary points of the edges.
    # Identical boundary points should become the same node.
    n_lst = create_nodes_from_edges(x)
  } else if (has_single_geom_type(x, "POINT")) {
    # Workflow:
    # It is assumed that the given POINT geometries form the nodes.
    # Edges need to be created as linestrings between those nodes.
    # It is assumed that the given nodes are connected sequentially.
    n_lst = create_edges_from_nodes(x)
  } else {
    stop(
      "Geometries are not all of type LINESTRING, or all of type POINT", 
      call. = FALSE
    )
  }
  sfnetwork(n_lst$nodes, n_lst$edges, force = TRUE, ...)
}

#' @name as_sfnetwork
#' @export
as_sfnetwork.sfNetwork = function(x, ...) {
  args = list(...)
  # Retrieve the @sl slot, which contains the linestring of the network.
  args$x = x@sl
  # Define the directed argument automatically if not given, using the @g slot.
  dir_missing = is.null(args$directed)
  args$directed = if (dir_missing) is_directed(x@g) else args$directed
  # Call as_sfnetwork.sf to build the sfnetwork.
  do.call("as_sfnetwork.sf", args)
}

#' @name as_sfnetwork
#' @export
as_sfnetwork.sfnetwork = function(x, ...) {
  x
}

#' @name as_sfnetwork
#' @export
as_sfnetwork.tbl_graph = function(x, ...) {
  # Get nodes and edges from the graph and add to the other given arguments.
  args = c(as.list(x), list(...))
  # If no directedness is specified, use the directedness from the tbl_graph.
  dir_missing = is.null(args$directed)
  args$directed = if (dir_missing) is_directed(x) else args$directed
  # Call the sfnetwork construction function.
  do.call("sfnetwork", args)
}

#' @importFrom rlang !!
#' @importFrom sf st_as_sf st_crs st_geometry
#' @importFrom tibble trunc_mat
#' @importFrom tidygraph as_tibble
#' @importFrom tools toTitleCase
#' @importFrom utils modifyList
#' @export
print.sfnetwork = function(x, ...) {
  # Capture graph output.
  nodes = as_tibble(x, "nodes")
  edges = as_tibble(x, "edges")
  # Truncate nodes and edges tibbles for printing
  arg_list = list(...)
  not_active = if (active(x) == "nodes") "edges" else "nodes"
  top = do.call(
    trunc_mat, 
    utils::modifyList(arg_list, list(x = as_tibble(x), n = 6))
  )
  top$summary[1] = paste0(top$summary[1], " (active)")
  if (
    active(x) == "edges" &&
    !has_spatially_explicit_edges(x) ||
    nrow(tidygraph::as_tibble(x)) == 0
    ) {
    names(top$summary)[1] = tools::toTitleCase(
      paste0(substr(active(x), 1, 4), " data")
    )
  } else {
    active_geom = sf::st_geometry(sf::st_as_sf(x))
    top$summary[2] = substr(class(active_geom)[1], 5, nchar(class(active_geom)[1]))
    bb = signif(attr(active_geom, "bbox"), options("digits")$digits)
    top$summary[3] = class(active_geom[[1]])[1]
    top$summary[4] = paste(paste(names(bb), bb[], sep = ": "), collapse = " ")
    names(top$summary) = c(
      tools::toTitleCase(paste0(substr(active(x), 1, 4), " data")),
      "Geometry type", "Dimension", "Bounding box"
    )
  }
  bottom = do.call(
    trunc_mat, 
    modifyList(arg_list, list(x = as_tibble(x, active = not_active), n = 3))
  )
  if (
    active(x) == "nodes" &&
    !has_spatially_explicit_edges(x) ||
    nrow(tidygraph::as_tibble(activate(x, !!not_active))) == 0
    ) {
    names(bottom$summary)[1] = tools::toTitleCase(
      paste0(substr(not_active, 1, 4), " data")
    )
  } else {
    inactive_geom = sf::st_geometry(sf::st_as_sf(activate(x, !!not_active)))
    bottom$summary[2] = substr(
      class(inactive_geom)[1], 
      5, 
      nchar(class(inactive_geom)[1])
    )
    bbi = signif(attr(inactive_geom, "bbox"), options("digits")$digits)
    bottom$summary[3] = class(inactive_geom[[1]])[1]
    bottom$summary[4] = paste(
      paste(names(bbi), bbi[], sep = ": "), 
      collapse = " "
    )
    names(bottom$summary) = c(
      tools::toTitleCase(paste0(substr(not_active, 1, 4), " data")),
      "Geometry type", "Dimension", "Bounding box"
    )
  }
  # Header.
  cat_subtle(c("# An sfnetwork with", nrow(nodes),"nodes and", nrow(edges), "edges\n"))
  cat_subtle("#\n")
  cat_subtle(c("# CRS: ", sf::st_crs(sf::st_as_sf(activate(x,"nodes")))$input, "\n"))
  cat_subtle("#\n")
  cat_subtle("#", tidygraph:::describe_graph(as_tbl_graph(x)))
  if (has_spatially_explicit_edges(x)) {
    cat_subtle(" with spatially explicit edges\n")
  } else {
    cat_subtle(" with spatially implicit edges\n")
  }
  cat_subtle("#\n")
  # Active data info.
  print(top)
  cat_subtle("#\n")
  # Inactive data info.
  print(bottom)
}

#' Check if an object is an sfnetwork
#'
#' @param x Object to be checked.
#'
#' @export
is.sfnetwork = function(x) {
  inherits(x, "sfnetwork")
}
