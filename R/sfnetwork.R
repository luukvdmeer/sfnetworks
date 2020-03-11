#' Create an sfnetwork object
#'
#' \code{sfnetwork} is a tidy data structure for spatial networks. It extends
#' the graph manipulation functionalities of the
#' \code{\link[tidygraph]{tidygraph-package}} package into the domain of geospatial
#' networks, where nodes are embedded in geographical space. It subclasses
#' \code{\link[tidygraph]{tbl_graph}}, and therefore all of tidygraphs functions
#' should work as expected, without any conversion. Just as with a
#' \code{\link[tidygraph]{tbl_graph}}, the nodes are activated by default. The
#' context can be changed using the \code{\link[tidygraph]{activate}} verb
#' and affects all subsequent operations. In \code{sfnetworks}, the nodes are
#' handled as being an \code{\link[sf]{sf}} object , and can always be extracted
#' with \code{\link[sf]{st_as_sf}} when activated. The edges can be handled as
#' being an \code{sf} object with \code{LINESTRING} geometry, but this is optional.
#' When the edges do not have an explicit spatial component, they can always be
#' extracted with \code{\link[tidygraph]{as_tibble}} when activated.
#'
#' @param nodes An object containing information about the nodes in the network.
#' The nodes should contain spatial information, either by being an \code{sf}
#' object, or by being convertable to an \code{sf}  object with
#' \code{\link[sf]{st_as_sf}}.
#'
#' @param edges An object containing information about the edges in the network.
#' This object may contain explicit spatial information by being an \code{sf}
#' object with \code{LINESTRING} geometry. However, this is optional. It may also
#' be a regular \code{data.frame} object. In any case, the terminal nodes of each
#' edge must either be encoded in a \code{to} and \code{from} column, or in the
#' two first columns, as integers. These integers refer to nodes indices.
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param ... Arguments passed on to \code{\link[sf]{st_as_sf}}, when
#' converting the nodes to an \code{sf} object.
#'
#' @return An object of class \code{sfnetwork}.
#'
#' @importFrom sf st_as_sf
#' @importFrom tidygraph tbl_graph
#' @export
sfnetwork = function(nodes, edges, directed = TRUE, ...) {
  # If nodes is not an sf object, try to convert it to an sf object.
  if (! inherits(nodes, "sf")) {
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
  if (inherits(edges, "sf")) {
    edges = structure(edges, class = setdiff(class(edges), "sf"))
  }

  x = tidygraph::tbl_graph(nodes, edges, directed = directed)
  class(x) = c("sfnetwork", class(x))
  x
}

#' Convert a foreign object to an sfnetwork object
#'
#' \code{sfnetwork} is a tidy data structure for spatial networks. It extends
#' the graph manipulation functionalities of the
#' \code{\link[tidygraph]{tidygraph-package}} package into the domain of geospatial
#' networks, where nodes are embedded in geographical space. It subclasses
#' \code{\link[tidygraph]{tbl_graph}}, and therefore all of tidygraphs functions
#' should work as expected, without any conversion. Just as with a
#' \code{\link[tidygraph]{tbl_graph}}, the nodes are activated by default. The
#' context can be changed using the \code{\link[tidygraph]{activate}} verb
#' and affects all subsequent operations. In \code{sfnetworks}, the nodes are
#' handled as being an \code{\link[sf]{sf}} object , and can always be extracted
#' with \code{\link[sf]{st_as_sf}} when activated. The edges can be handled as
#' being an \code{sf} object with \code{LINESTRING} geometry, but this is optional.
#' When the edges do not have an explicit spatial component, they can always be
#' extracted with \code{\link[tidygraph]{as_tibble}} when activated.
#'
#' @details
#' If an object can be read by \code{\link[tidygraph]{as_tbl_graph}} and the
#' nodes can be read by \code{\link[sf]{st_as_sf}}, it is automatically
#' supported by sfnetworks.
#'
#' @param x object to be converted into an \code{sfnetwork} object.
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
as_sfnetwork.default = function(x, ...) {
  tryCatch(
    expr = {
      as_sfnetwork(tidygraph::as_tbl_graph(x), ...)
    },
    error = function(e) {
      stop("No support for ", class(x)[1], " objects")
    }
  )
}

#' @name as_sfnetwork
#'
#' @param directed Should the constructed network be directed? Defaults to
#' \code{TRUE}.
#'
#' @param lines_as_edges Should \code{LINESTRING} geometries be considered
#' edges instead of nodes?
#' Defaults to \code{TRUE}.
#'
#' @importFrom sf st_as_sf st_cast st_centroid st_crs st_geometry st_sfc st_union
#' @export
as_sfnetwork.sf = function(x, directed = TRUE, lines_as_edges = TRUE, ...) {
  if (class(sf::st_geometry(x))[1] == "sfc_LINESTRING" & lines_as_edges) {
    edges = x
    coords = sf::st_coordinates(edges)
    all_x_coords = split(coords[, "X"], f = as.factor(coords[, "L1"]))
    all_y_coords = split(coords[, "Y"], f = as.factor(coords[, "L1"]))
    node_x_coords = do.call(
      "rbind",
      lapply(
        all_x_coords,
        function(x) data.frame(X = c(x[1], x[length(x)]))
      )
    )
    node_y_coords = do.call(
      "rbind",
      lapply(
        all_y_coords,
        function(x) data.frame(Y = c(x[1], x[length(x)]))
      )
    )
    nodes = cbind(node_x_coords, node_y_coords)
    nodes$XY = paste(nodes$X, nodes$Y)
    nodes$ID = match(nodes$XY, unique(nodes$XY))
    nodes$source = rep(c(TRUE, FALSE), nrow(nodes) / 2)
    if ("from" %in% colnames(edges)) {
      warning("Overwriting column 'from'")
    }
    edges$from = nodes[nodes$source, "ID"]
    paste('hey!')
    if ("to" %in% colnames(edges)) {
      warning("Overwriting column 'to'")
    }
    edges$to = nodes[!nodes$source, "ID"]
    nodes = nodes[!duplicated(nodes$ID), ]
    nodes = sf::st_as_sf(
      nodes[, c("X", "Y")],
      coords = c("X", "Y"),
      crs = sf::st_crs(edges)
    )
    class(nodes) = class(edges)
  } else {
    nodes = x
    if (class(sf::st_geometry(x))[1] == "sfc_POINT") {
      node_pts = nodes
    } else {
      node_pts = sf::st_centroid(nodes)
    }
    sources = node_pts[1:(nrow(node_pts)-1), ]
    targets = node_pts[2:nrow(node_pts), ]
    edges = sf::st_as_sf(
      data.frame(
        from = 1:(nrow(node_pts)-1),
        to = 2:nrow(node_pts),
        geometry = sf::st_sfc(
          mapply(
            function (a,b) sf::st_cast(sf::st_union(a,b), "LINESTRING"),
            sf::st_geometry(sources),
            sf::st_geometry(targets),
            SIMPLIFY=FALSE
          )
        )
      ),
      crs = sf::st_crs(nodes)
    )
    class(edges) = class(nodes)
  }
  sfnetwork(nodes, edges, directed = directed, ...)
}

#' @name as_sfnetwork
#' @importFrom igraph is_directed
#' @export
as_sfnetwork.tbl_graph = function(x, ...) {
  xls = as.list(x)
  directed = igraph::is_directed(x)
  sfnetwork(xls[[1]], xls[[2]], directed = directed, ...)
}

#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.sfnetwork = function(x, ...) {
  class(x) = setdiff(class(x), "sfnetwork")
  x
}

#' @export
print.sfnetwork = function(x, ...) {
  print(as_tbl_graph(x))
}
