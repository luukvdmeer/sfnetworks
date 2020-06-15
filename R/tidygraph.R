#' @importFrom tidygraph activate
#' @export
tidygraph::activate

#' @importFrom tidygraph active
#' @export
tidygraph::active

#' @importFrom tidygraph %>%
#' @export
tidygraph::`%>%`

tbg_to_sfn = function(.data) {
  class(.data) = c("sfnetwork", class(.data))
  .data
}

#' tidygraph methods for sfnetwork objects
#'
#' \code{\link[tidygraph:tidygraph-package]{tidygraph}} methods for 
#' \code{\link{sfnetwork}} objects. Since \code{\link{sfnetwork}} objects
#' subclass \code{\link[tidygraph]{tbl_graph}} objects, most tidygraph
#' functions work automatically. However, some of them need a special method,
#' mostly as a result of the presence of the geometry list column in
#' \code{\link{sfnetwork}} objects. Use these methods without the .sfnetwork 
#' suffix and after loading the tidygraph package.
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param .data An object of class \code{\link{sfnetwork}}.
#'
#' @param ... Arguments passed on the corresponding \code{tidygraph} function.
#'
#' @param active Which network element (i.e. nodes or edges) to activate before
#' extracting. If \code{NULL}, it will be set to the current active element of
#' the given network. Defaults to \code{NULL}.
#'
#' @param spatial Should te extracted tibble be a 'spatial tibble', i.e. an
#' object of class \code{c('sf', 'tbl_df')}, if it contains a geometry list
#' column. Defaults to \code{TRUE}. 
#'
#' @param .f See \code{\link[tidygraph]{morph}}.
#'
#' @details See the \code{\link[tidygraph:tidygraph-package]{tidygraph}} 
#' documentation.
#'
#' @name tidygraph
#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.sfnetwork = function(x, ...) {
  class(x) = setdiff(class(x), "sfnetwork")
  x
}

#' @describeIn tidygraph The sfnetwork method for 
#' \code{\link[tidygraph]{as_tibble}} differs in the sense that whenever a
#' geometry list column is present, by default it will return a 
#' 'spatial tibble'. With that we mean an object of class 
#' \code{c('sf', 'tbl_df')} instead of an object of class \code{'tbl_df'}.
#' @importFrom tidygraph as_tibble
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
      stop("Unknown active element: ", active, ". Only nodes and edges supported")
    )
  } else {
    switch(
      active,
      nodes = tidygraph::as_tibble(as_tbl_graph(x), "nodes"),
      edges = tidygraph::as_tibble(as_tbl_graph(x), "edges"),
      stop("Unknown active element: ", active, ". Only nodes and edges supported")
    )
  }
}

node_spatial_tibble = function(x) {
  st_as_sf(x, "nodes")
}

edge_spatial_tibble = function(x) {
  if (has_spatially_explicit_edges(x)) {
    st_as_sf(x, "edges") 
  } else {
    tidygraph::as_tibble(as_tbl_graph(x), "edges")
  }
}

#' @describeIn tidygraph The sfnetwork method for 
#' \code{\link[tidygraph]{morph}} will first try to input the 
#' \code{\link{sfnetwork}} object into the morph method for a 
#' \code{\link[tidygraph]{tbl_graph}}. If this fails, it will first convert
#' the \code{\link{sfnetwork}} object into a \code{\link[tidygraph]{tbl_graph}}
#' object before calling \code{\link[tidygraph]{morph}}. The returned value
#' will be a \code{morphed_sfnetwork} when all elements of the morphed graph
#' are of class \code{\link{sfnetwork}}, and a \code{morphed_tbl_graph}
#' otherwise.
#' @importFrom tidygraph morph
#' @export
morph.sfnetwork = function(.data, .f, ...) {
  # Morph using tidygraphs morphing functionality.
  # First convert to tbl_graph if sfnetwork object gives errors.
  morphed = tryCatch(
    NextMethod(),
    error = function(e) tidygraph::morph(as_tbl_graph(.data), .f, ...)
  )
  # Try to convert morphed elements into sfnetwork objects.
  # If not possible, simply return as morphed_tbl_graph.
  tryCatch(
    expr = {
      morphed_sfn = suppressMessages(
        lapply(morphed, function(x) as_sfnetwork(x))
      )
      structure(
        morphed_sfn,
        class = c("morphed_sfnetwork", class(morphed)),
        .orig_graph = as_sfnetwork(attr(morphed, ".orig_graph"), force = TRUE),
        .morpher = attr(morphed, ".morpher")
      )
    },
    error = function(e) morphed
  )  
}

#' @describeIn tidygraph The sfnetwork method for \code{\link[tidygraph]{mutate}}
#' works the same, but updates the sf attributes of the resulting network.
#' @importFrom tidygraph mutate
#' @export
mutate.sfnetwork = function(.data, ...) {
  # Run tidygraphs mutate.
  x = NextMethod()
  if (attr(x, "active") == "edges" && !has_spatially_explicit_edges(x)) {
    return(x)
  }
  # Update the agr sf attribute.
  agr = st_agr(.data)
  attrs = get_attr_names(x)
  new_agr = setNames(agr[attrs], attrs) # NA's new columns
  sf_attr(x, "agr") = new_agr
  # Return x.
  x
}

#' @describeIn tidygraph The sfnetwork method for \code{\link[tidygraph]{select}}
#' works the same, but updates the sf attributes of the resulting network.
#' @importFrom tidygraph select
#' @export
select.sfnetwork = function(.data, ...) {
  # Run tidygraphs select.
  x = NextMethod()
  if (attr(x, "active") == "edges" && !has_spatially_explicit_edges(x)) {
    return(x)
  }
  # Update the agr sf attribute.
  agr = st_agr(.data)
  attrs = get_attr_names(x)
  new_agr = agr[attrs]
  sf_attr(x, "agr") = new_agr
  # Return x.
  x
}

