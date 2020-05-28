#' Create an sfnetwork object
#'
#' \code{sfnetwork} is a tidy data structure for spatial networks. It extends
#' the graph manipulation functionalities of the
#' \code{\link[tidygraph]{tidygraph-package}} package into the domain of
#' geospatial networks, where nodes are embedded in geographical space. It
#' subclasses \code{\link[tidygraph]{tbl_graph}}, and therefore all of tidygraphs
#' functions should work as expected, without any conversion. Just as with a
#' \code{\link[tidygraph]{tbl_graph}}, the nodes are activated by default. The
#' context can be changed using the \code{\link[tidygraph]{activate}} verb
#' and affects all subsequent operations. In \code{sfnetworks}, the nodes are
#' handled as being an \code{\link[sf]{sf}} object with only \code{POINT}
#' geometries, and can always be extracted with \code{\link[sf]{st_as_sf}} when
#' activated. The edges can be handled as being an \code{sf} object with only
#' \code{LINESTRING} geometries, but this is optional. When the edges do not
#' have an explicit spatial component, they can always be extracted with
#' \code{\link[tidygraph]{as_tibble}} when activated.
#'
#' @param nodes An object containing information about the nodes in the network.
#' The nodes should contain spatial information, either by being an \code{sf}
#' object with only \code{POINT} geometries, or by being convertable to such an
#' object with \code{\link[sf]{st_as_sf}}.
#'
#' @param edges An object containing information about the edges in the network.
#' This object may contain explicit spatial information by being an \code{sf}
#' object with only \code{LINESTRING} geometries. However, this is optional. It
#' may also be a regular \code{data.frame} or \code{tbl_df} object. In any case,
#' the terminal nodes of each edge must either be encoded in a \code{to} and
#' \code{from} column, or in the two first columns, as integers. These integers
#' refer to nodes indices, which in turn refer to the position of a node in the
#' nodes table.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param edges_as_lines Should the edges be spatially explicit, i.e. have
#' \code{LINESTRING} geometries stored in a geometry list column? Defaults to
#' \code{TRUE}.
#'
#' @param ... Arguments passed on to \code{\link[sf]{st_as_sf}}, when
#' converting the nodes to an \code{sf} object.
#'
#' @return An object of class \code{sfnetwork}.
#'
#' @importFrom sf st_as_sf
#' @importFrom tidygraph tbl_graph .N
#' @export
sfnetwork = function(nodes, edges, directed = TRUE, edges_as_lines = TRUE, ...) {
  # Construct the network.
  net = construct_sfnetwork(nodes, edges, directed, edges_as_lines, ...)
  # Run checks to guarantee a valid network structure.
  if (! st_is_all(get_nodes(net), "POINT")) {
    stop("Only geometries of type POINT are allowed as nodes")
  }
  if (has_spatially_explicit_edges(xsn)) {
    if (! st_is_all(get_edges(xsn), "LINESTRING")) {
      stop("Only geometries of type LINESTRING are allowed as edges")
    }
    if (! same_crs(get_nodes(xsn), get_edges(xsn))) {
      stop("Nodes and edges do not have the same CRS")
    }
    if (! nodes_match_edge_boundaries(xsn)) {
      stop("Boundary points of edges should match their corresponding nodes")
    }
  }
}

construct_sfnetwork = function(nodes, edges, directed = TRUE, edges_as_lines = TRUE, ...) {
  # If nodes is not an sf object, try to convert it to an sf object.
  if (! is.sf(nodes)) {
    tryCatch(
      expr = {
        nodes = sf::st_as_sf(nodes, ...)
      },
      error = function(e) {
        stop("Failed to convert nodes into sf object because: ", e)
      }
    )
  }
  # If edges is an sf object, tidygraph cannot handle it due to sticky geometry.
  # Therefore it has to be converted into a regular data frame (or tibble).
  if (is.sf(edges)) {
    edges = structure(edges, class = setdiff(class(edges), "sf"))
  }
  # Create the network with the nodes and edges.
  xtg = tidygraph::tbl_graph(nodes, edges, directed = directed)
  xsn = structure(xtg, class = c("sfnetwork", class(xtg)))
  # Add or remove edge geometries if needed.
  if (edges_as_lines) {
    xsn = to_spatially_explicit_edges(xsn)
  } else {
    if (has_spatially_explicit_edges(xsn)) {
      xsn = drop_geometry(xsn, "edges")
    }
  }
  xsn
}

#' Convert a foreign object to an sfnetwork object
#'
#' \code{sfnetwork} is a tidy data structure for spatial networks. It extends
#' the graph manipulation functionalities of the
#' \code{\link[tidygraph]{tidygraph-package}} package into the domain of
#' geospatial networks, where nodes are embedded in geographical space. It
#' subclasses \code{\link[tidygraph]{tbl_graph}}, and therefore all of tidygraphs
#' functions should work as expected, without any conversion. Just as with a
#' \code{\link[tidygraph]{tbl_graph}}, the nodes are activated by default. The
#' context can be changed using the \code{\link[tidygraph]{activate}} verb
#' and affects all subsequent operations. In \code{sfnetworks}, the nodes are
#' handled as being an \code{\link[sf]{sf}} object with only \code{POINT}
#' geometries, and can always be extracted with \code{\link[sf]{st_as_sf}} when
#' activated. The edges can be handled as being an \code{sf} object with only
#' \code{LINESTRING} geometries, but this is optional. When the edges do not
#' have an explicit spatial component, they can always be extracted with
#' \code{\link[tidygraph]{as_tibble}} when activated.
#'
#' @details
#' If an object can be read by \code{\link[tidygraph]{as_tbl_graph}} and the
#' nodes can be read by \code{\link[sf]{st_as_sf}}, it is automatically
#' supported by sfnetworks.
#'
#' @param x object to be converted into an \code{sfnetwork} object.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param edges_as_lines Should the edges be spatially explicit, i.e. have
#' \code{LINESTRING} geometries stored in a geometry list column? Defaults to
#' \code{TRUE}.
#'
#' @param ... arguments passed on to construction function.
#'
#' @return An object of class \code{sfnetwork}.
#'
#' @export
as_sfnetwork = function(x, ...) {
  UseMethod("as_sfnetwork")
}

#' @name as_sfnetwork
#' @importFrom tidygraph as_tbl_graph
#' @export
as_sfnetwork.default = function(x, directed = TRUE, edges_as_lines = TRUE, ...) {
  xtg = tidygraph::as_tbl_graph(x, directed = directed)
  as_sfnetwork(xtg, edges_as_lines = edges_as_lines, ...)
}

#' @name as_sfnetwork
#' @importFrom sf st_geometry
#' @export
as_sfnetwork.sf = function(x, directed = TRUE, edges_as_lines = TRUE, ...) {
  if (st_is_all(x, "LINESTRING")) {
    # Workflow:
    # It is assumed that the given LINESTRING geometries form the edges.
    # Nodes need to be created at the boundary points of the edges.
    # Identical boundary points should become the same node.
    network = create_nodes_from_edges(x)
  } else if (st_is_all(x, "POINT")) {
    # Workflow:
    # It is assumed that the given POINT geometries form the nodes.
    # Edges need to be created as linestrings between those nodes.
    # It is assumed that the given nodes are connected sequentially.
    network = create_edges_from_nodes(x)
  } else {
    stop("Only geometries of type LINESTRING or POINT are allowed")
  }
  construct_sfnetwork(network$nodes, network$edges, directed, edges_as_lines, ...)
}

#' @name as_sfnetwork
#' @export
as_sfnetwork.tbl_graph = function(x, edges_as_lines = TRUE, ...) {
  tblgraph_to_sfnetwork(x, edges_as_lines, run_checks = TRUE, ...)
}

tblgraph_to_sfnetwork = function(x, edges_as_lines = TRUE, run_checks = FALSE, ...) {
  xls = as.list(x)
  args = list(xls[[1]], xls[[2]], is_directed(x), edges_as_lines, ...)
  ifelse(checks, do.call("sfnetwork", args), do.call("construct_sfnetwork", args))
}

#' @importFrom sf st_as_sf st_crs st_geometry
#' @importFrom rlang !!
#' @importFrom tibble trunc_mat
#' @importFrom utils modifyList
#' @importFrom tools toTitleCase
#' @export
print.sfnetwork = function(x, ...) {
  # Capture graph output.
  nodes = as_tibble(x, "nodes")
  edges = as_tibble(x, "edges")
  # Truncate nodes and edges tibbles for printing
  arg_list = list(...)
  not_active = if (active(x) == "nodes") "edges" else "nodes"
  top = do.call(trunc_mat, utils::modifyList(arg_list, list(x = as_tibble(x), n = 6)))
  top$summary[1] = paste0(top$summary[1], " (active)")
  if (active(x) == "edges" && !has_spatially_explicit_edges(x)) {
    names(top$summary)[1] = tools::toTitleCase(paste0(substr(active(x), 1, 4), " data"))
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
  bottom = do.call(trunc_mat, modifyList(arg_list, list(x = as_tibble(x, active = not_active), n = 3)))
  if (active(x) == "nodes" && !has_spatially_explicit_edges(x)) {
    names(bottom$summary)[1] = tools::toTitleCase(paste0(substr(not_active, 1, 4), " data"))
  } else {
    inactive_geom = sf::st_geometry(sf::st_as_sf(activate(x, !!not_active)))
    bottom$summary[2] = substr(class(inactive_geom)[1], 5, nchar(class(inactive_geom)[1]))
    bbi = signif(attr(inactive_geom, "bbox"), options("digits")$digits)
    bottom$summary[3] = class(inactive_geom[[1]])[1]
    bottom$summary[4] = paste(paste(names(bbi), bbi[], sep = ": "), collapse = " ")
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
    cat_subtle(" and spatially explicit edges\n")
  } else {
    cat_subtle(" and spatially implicit edges\n")
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
