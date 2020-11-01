#' Plot sfnetwork geometries
#'
#' Plot the geometries of an object of class \code{\link{sfnetwork}}.
#'
#' @param x Object of class \code{\link{sfnetwork}}.
#'
#' @param draw_lines If the edges of the network are spatially implicit, should
#' straight lines be drawn between connected nodes? Defaults to \code{TRUE}.
#' Ignored when the edges of the network are spatially explicit.
#'
#' @param ... Arguments passed on to \code{\link[sf:plot]{plot.sf}}
#'
#' @details This is a basic plotting functionality. For more advanced plotting,
#' it is recommended to extract the nodes and edges from the network, and plot
#' them separately with one of the many available spatial plotting functions
#' as can be found in \code{sf}, \code{tmap}, \code{ggplot2}, \code{ggspatial},
#' and others.
#'
#' @examples
#' net = as_sfnetwork(roxel)
#' plot(net)
#'
#' # When lines are spatially implicit
#' net = as_sfnetwork(roxel, edges_as_lines = FALSE)
#' plot(net)
#'
#' # Changing plot parameters like `col` will affect
#' # both edges and nodes, while e.g. `lwd` only affects
#' # the edges and `pch` and `cex` the nodes.
#'
#' plot(net, col = 'blue', pch = 18, lwd = 1, cex = 2)
#'
#' @importFrom graphics plot
#' @importFrom sf st_geometry
#' @export
plot.sfnetwork = function(x, draw_lines = TRUE, ...) {
  dots = list(...)
  # Get geometries of nodes.
  nsf = st_geometry(x, "nodes")
  # Combine node geometries with edge geometries if needed.
  use_edges = TRUE
  if (! has_spatially_explicit_edges(x)) {
    if (draw_lines) {
      x = explicitize_edges(x)
    } else {
      use_edges = FALSE
    }
  }
  dots$x = if (use_edges) c(nsf, st_geometry(x, "edges")) else nsf
  # Use pch of 20 by default.
  pch_missing = is.null(dots$pch)
  dots$pch = if (pch_missing) 20 else dots$pch
  # Plot.
  do.call(plot, dots)
}

#' @importFrom sf st_as_sf
autoplot.sfnetwork = function(x) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = st_as_sf(x, 'nodes')) +
    ggplot2::geom_sf(data = st_as_sf(x, 'edges'))
}

# from: https://github.com/r-spatial/sf/blob/master/R/tidyverse.R
# 2020-11-01 17:03:36 CET

# nocov start

.onLoad = function(...) {
  # has_ggplot2_3.0 =
  #   requireNamespace("ggplot2", quietly = TRUE) &&
  #   utils::packageVersion("ggplot2") >= "3.0.0"
  #
  # if (has_ggplot2_3.0)

  register_s3_method("ggplot2", "autoplot", "sfnetwork")
  invisible()
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
