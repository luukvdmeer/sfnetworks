#' @importFrom tidygraph activate
#' @export
tidygraph::activate

#' @importFrom tidygraph active
#' @export
tidygraph::active

#' @importFrom tidygraph morph
#' @export
tidygraph::morph

#' @importFrom tidygraph unmorph
#' @export
tidygraph::unmorph

#' @importFrom tidygraph convert
#' @export
tidygraph::convert

#' @importFrom tidygraph crystallize
#' @export
tidygraph::crystallize

#' @importFrom tidygraph crystallise
#' @export
tidygraph::crystallise

#' @importFrom tidygraph %>%
#' @export
tidygraph::`%>%`

#' tidygraph methods for sfnetworks
#'
#' Normally tidygraph functions should work out of the box on
#' \code{\link{sfnetwork}} objects, but in some cases special treatment is
#' needed especially for the geometry column, requiring a specific method.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param .data An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments passed on the corresponding \code{tidygraph} function.
#'
#' @return The method for \code{\link[tidygraph]{as_tbl_graph}} returns an
#' object of class \code{\link[tidygraph]{tbl_graph}}. The method for
#' \code{\link[tidygraph]{morph}} returns a \code{morphed_sfnetwork} if the
#' morphed network is still spatial, and a \code{morphed_tbl_graph} otherwise.
#' All other methods return an object of class \code{\link{sfnetwork}}.
#'
#' @details See the \code{\link[tidygraph]{tidygraph}} documentation. The
#' following methods have a special behavior:
#'
#' \itemize{
#'   \item \code{reroute}: To preserve the valid spatial network structure,
#'   this method will replace the boundaries of edge geometries by the location
#'   of the node those edges are rerouted to or from. Note that when the goal
#'   is to reverse edges in a spatial network, reroute will not simply reverse
#'   the edge geometries. In that case it is recommended to use the sfnetwork
#'   method for \code{\link[sf]{st_reverse}} instead.
#'   \item \code{morph}: This method checks if the morphed network still has
#'   spatially embedded nodes. In that case a \code{morphed_sfnetwork} is
#'   returned. If not, a \code{morphed_tbl_graph} is returned instead.
#'   \item \code{unmorph}: This method makes sure the geometry list column is
#'   correctly handled during the unmorphing process.
#' }
#'
#' @name tidygraph_methods
NULL

#' @name tidygraph_methods
#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.sfnetwork = function(x, ...) {
  class(x) = setdiff(class(x), "sfnetwork")
  x
}

#' @name tidygraph_methods
#' @importFrom igraph is_directed
#' @importFrom tidygraph reroute
#' @export
reroute.sfnetwork = function(.data, ...) {
  if (is_directed(.data)) .data = make_edges_follow_indices(.data)
  rerouted = NextMethod()
  make_edges_valid(rerouted)
}

#' @name tidygraph_methods
#' @importFrom tidygraph morph
#' @export
morph.sfnetwork = function(.data, ...) {
  # Morph using tidygraphs morphing functionality.
  morphed = NextMethod()
  # If morphed data still consist of valid sfnetworks:
  # --> Convert the morphed_tbl_graph into a morphed_sfnetwork.
  # --> Otherwise, just return the morphed_tbl_graph.
  if (is_sfnetwork(morphed[[1]])) {
    structure(
      morphed,
      class = c("morphed_sfnetwork", class(morphed))
    )
  } else if (has_spatial_nodes(morphed[[1]])) {
    morphed[] = lapply(morphed, tbg_to_sfn)
    structure(
      morphed,
      class = c("morphed_sfnetwork", class(morphed))
    )
  } else {
    morphed
  }
}

#' @name tidygraph_methods
#' @importFrom igraph edge_attr vertex_attr
#' @importFrom tibble as_tibble
#' @importFrom tidygraph unmorph
#' @export
unmorph.morphed_sfnetwork = function(.data, ...) {
  # Unmorphing needs additional preparation for morphed sfnetworks when:
  # --> Features were merged and original data stored in a .orig_data column.
  # --> In this case tidygraph attempts to bind columns of two tibbles.
  # --> The sticky geometry of sf creates problems in that process.
  # --> We can work around this by making sure .orig_data has no sf objects.
  if (! is.null(vertex_attr(.data[[1]], ".orig_data"))) {
    orig_data_to_tibble = function(x) {
      vertex_attr(x, ".orig_data") = as_tibble(vertex_attr(x, ".orig_data"))
      x
    }
    .data[] = lapply(.data, orig_data_to_tibble)
  }
  if (! is.null(edge_attr(.data[[1]], ".orig_data"))) {
    orig_data_to_tibble = function(x) {
      edge_attr(x, ".orig_data") = as_tibble(edge_attr(x, ".orig_data"))
      x
    }
    .data[] = lapply(.data, orig_data_to_tibble)
  }
  # Call tidygraphs unmorph.
  NextMethod(.data, ...)
}

#' @importFrom tidygraph unfocus
#' @export
unfocus.sfnetwork = function(.data, ...) {
  .data
}
