#' Create a sfnetwork
#'
#' \code{sfnetwork} is a tidy data structure for geospatial networks. It
#' extends the \code{\link[tidygraph]{tbl_graph}} data structure for
#' relational data into the domain of geospatial networks, whith nodes and
#' edges embedded in geographical space, and offers smooth integration with
#' \code{\link[sf]{sf}} for spatial data analysis.
#'
#' @param nodes The nodes of the network. Should be an object of class
#' \code{\link[sf]{sf}}, or directly convertible to it using
#' \code{\link[sf]{st_as_sf}}. All features should have an associated geometry
#' of type \code{POINT}.
#'
#' @param edges The edges of the network. May be an object of class
#' \code{\link[sf]{sf}}, with all features having an associated geometry of
#' type \code{LINESTRING}. It may also be a regular \code{data.frame} or
#' \code{tbl_df} object. In any case, the adjacent nodes of each edge must
#' either be encoded in a \code{to} and \code{from} column, as integers or
#' characters. Integers should refer to the position of a node in the nodes
#' table, while characters should refer to the name of a node encoded in the
#' column referred to in the \code{node_key} argument. Setting edges to
#' \code{NULL} will create a network without edges.
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
#' when the edges are given as an object of class \code{\link[sf]{sf}}, and
#' \code{FALSE} otherwise. Defaults to \code{NULL}.
#'
#' @param length_as_weight Should the length of the edges be stored in a column
#' named \code{weight}? If set to \code{TRUE}, this will calculate the length
#' of the linestring geometry of the edge in the case of spatially explicit
#' edges, and the straight-line distance between the source and target node in
#' the case of spatially implicit edges. If there is already a column named
#' \code{weight}, it will be overwritten. Defaults to \code{FALSE}.
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
#' @param ... Arguments passed on to \code{\link[sf]{st_as_sf}}, if nodes need
#' to be converted into an \code{\link[sf]{sf}} object during construction.
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
#' @importFrom sf st_as_sf
#' @importFrom tidygraph mutate tbl_graph
#' @export
sfnetwork = function(nodes, edges = NULL, directed = TRUE, node_key = "name",
                     edges_as_lines = NULL, length_as_weight = FALSE,
                     force = FALSE, ...) {
  # Prepare nodes.
  # If nodes is not an sf object:
  # --> Try to convert it to an sf object.
  # --> Arguments passed in ... will be passed on to st_as_sf.
  if (! is.sf(nodes)) {
    nodes = tryCatch(
      st_as_sf(nodes, ...),
      error = function(e) {
        stop("Failed to convert nodes to sf object because: ", e, call. = FALSE)
      }
    )
  }
  # Prepare edges.
  # If edges is an sf object:
  # --> Tidygraph cannot handle it due to sticky geometry.
  # --> Therefore it has to be converted into a regular data frame (or tibble).
  if (has_sfc(edges)) {
    if (is.sf(edges)) class(edges) = setdiff(class(edges), "sf")
    if (is.null(edges_as_lines)) edges_as_lines = TRUE
  } else {
    if (is.null(edges_as_lines)) edges_as_lines = FALSE
  }
  # Create network.
  # Store sf attributes of the nodes and edges in a special graph attribute.
  x_tbg = tbl_graph(nodes, edges, directed, node_key)
  x_sfn = structure(x_tbg, class = c("sfnetwork", class(x_tbg)))
  # Post-process network.
  if (is.null(edges)) {
    # Run validity check for nodes and return the network.
    if (! force) require_valid_network_structure(x_sfn, message = TRUE)
    return (x_sfn)
  }
  if (edges_as_lines) {
    # Run validity check before explicitizing edges.
    if (! force) require_valid_network_structure(x_sfn, message = TRUE)
    # Add edge geometries if needed.
    x_sfn = explicitize_edges(x_sfn)
  } else {
    # Remove edge geometries if needed.
    x_sfn = implicitize_edges(x_sfn)
    # Run validity check after implicitizing edges.
    if (! force) require_valid_network_structure(x_sfn, message = TRUE)
  }
  if (length_as_weight) {
    if ("weight" %in% edge_graph_attribute_names(x_sfn)) {
      raise_overwrite("weight")
    }
    x_sfn = activate(x_sfn, "edges")
    x_sfn = mutate(x_sfn, weight = edge_length())
    x_sfn = activate(x_sfn, "nodes")
  }
  x_sfn
}

# Simplified construction function.
# Must be sure that nodes and edges together form a valid sfnetwork.
# ONLY FOR INTERNAL USE!

#' @importFrom tidygraph tbl_graph
sfnetwork_ = function(nodes, edges = NULL, directed = TRUE) {
  if (is.sf(edges)) class(edges) = setdiff(class(edges), "sf")
  x_tbg = tbl_graph(nodes, edges, directed)
  structure(x_tbg, class = c("sfnetwork", class(x_tbg)))
}

# Fast function to convert from tbl_graph to sfnetwork.
# Must be sure that tbl_graph has already a valid sfnetwork structure.
# ONLY FOR INTERNAL USE!

tbg_to_sfn = function(x) {
  class(x) = c("sfnetwork", class(x))
  x
}

#' Convert a foreign object to a sfnetwork
#'
#' Convert a given object into an object of class \code{\link{sfnetwork}}.
#' If an object can be read by \code{\link[tidygraph]{as_tbl_graph}} and the
#' nodes can be read by \code{\link[sf]{st_as_sf}}, it is automatically
#' supported.
#'
#' @param x Object to be converted into an \code{\link{sfnetwork}}.
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
  as_sfnetwork(as_tbl_graph(x), ...)
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
  x_sf = st_as_sf(x)
  # x_sf is an sf object composed by 1 POLYGON (the window of the psp object)
  # and several LINESTRINGs (the line segments). I'm not sure if and how we can
  # use the window object so I will extract only the LINESTRINGs.
  x_linestring = st_collection_extract(x_sf, "LINESTRING")
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
#' @importFrom sf st_as_sf
#' @export
as_sfnetwork.sfc = function(x, ...) {
  as_sfnetwork(st_as_sf(x), ...)
}

#' @name as_sfnetwork
#' @importFrom igraph is_directed
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
#' @importFrom igraph is_directed
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

#' @importFrom igraph ecount vcount
#' @importFrom sf st_crs
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
#' @export
print.sfnetwork = function(x, ...) {
  # Define active and inactive component.
  active = attr(x, "active")
  inactive = if (active == "nodes") "edges" else "nodes"
  # Count number of nodes and edges in the network.
  nN = vcount(x) # Number of nodes in network.
  nE = ecount(x) # Number of edges in network.
  # Print header.
  cat_subtle(c("# An sfnetwork with", nN, "nodes and", nE, "edges\n"))
  cat_subtle("#\n")
  cat_subtle(c("# CRS: ", st_crs(x)$input, "\n"))
  cat_subtle("#\n")
  cat_subtle("#", tidygraph:::describe_graph(as_tbl_graph(x)))
  if (has_spatially_explicit_edges(x)) {
    cat_subtle(" with spatially explicit edges\n")
  } else {
    cat_subtle(" with spatially implicit edges\n")
  }
  cat_subtle("#\n")
  # Print active data summary.
  active_data = summarise_network_element(
    data = as_tibble(x, active),
    name = substr(active, 1, 4),
    active = TRUE,
    ...
  )
  print(active_data)
  cat_subtle("#\n")
  # Print inactive data summary.
  inactive_data = summarise_network_element(
    data = as_tibble(x, inactive),
    name = substr(inactive, 1, 4),
    active = FALSE,
    ...
  )
  print(inactive_data)
}

#' @importFrom igraph ecount vcount
#' @export
print.morphed_sfnetwork = function(x, ...) {
  graph = attr(x, '.orig_graph')
  cat('# An sfnetwork temporarily morphed to a ', gsub('_', ' ', sub('to_', '', attr(x, '.morpher'))), ' representation\n', sep = '')
  cat('# \n')
  cat('# Original network is ', tolower(tidygraph:::describe_graph(graph)), '\n', sep = '')
  cat('# consisting of ', ecount(graph), ' nodes and ', vcount(graph), ' edges\n', sep = '')
}

#' @importFrom sf st_geometry
#' @importFrom tibble trunc_mat
#' @importFrom tools toTitleCase
#' @importFrom utils modifyList
summarise_network_element = function(data, name, active = TRUE, ...) {
  # Capture ... arguments.
  args = list(...)
  # Truncate data.
  n = if (active) 6 else 3
  x = do.call(trunc_mat, modifyList(args, list(x = data, n = n)))
  # Write summary.
  x$summary[1] = paste(x$summary[1], if (active) "(active)" else "")
  if (name == "edge" && (!has_sfc(data) || nrow(data) == 0)) {
    names(x$summary)[1] = toTitleCase(paste(name, "data"))
  } else {
    geom = st_geometry(data)
    x$summary[2] = substr(class(geom)[1], 5, nchar(class(geom)[1]))
    x$summary[3] = class(geom[[1]])[1]
    bb = signif(attr(geom, "bbox"), options("digits")$digits)
    x$summary[4] = paste(paste(names(bb), bb[], sep = ": "), collapse = " ")
    names(x$summary) = c(
      toTitleCase(paste(name, "data")),
      "Geometry type",
      "Dimension",
      "Bounding box"
    )
  }
  x
}

#' @importFrom sf st_crs
#' @importFrom utils capture.output
#' @export
print.morphed_sfnetwork = function(x, ...) {
  x_tbg = structure(x, class = setdiff(class(x), "morphed_sfnetwork"))
  out = capture.output(print(x_tbg), ...)
  cat(gsub("tbl_graph", "sfnetwork", out[[1]]), "\n")
  cat(out[[2]], "\n")
  cat(out[[3]], "\n")
  cat(out[[4]], "\n")
  cat("# with CRS", st_crs(attr(x, ".orig_graph"))$input)
}

#' Check if an object is an sfnetwork
#'
#' @param x Object to be checked.
#'
#' @export
is.sfnetwork = function(x) {
  inherits(x, "sfnetwork")
}
