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

#' autoplot method for sfnetworks
#'
#' Plot the geometries of an object of class \code{\link{sfnetwork}} 
#' automatically as a \code{\link[ggplot2]{ggplot}} object. Use this method 
#' without the .sfnetwork suffix and after loading the ggplot2 package.
#'
#' @param object An object of class \code{\link{sfnetwork}}.
#' 
#' @param ... Ignored.
#'
#' @details See \code{\link[ggplot2]{autoplot}}.
#'
#' @examples
#' library(ggplot2)
#' library(sf)
#' net = as_sfnetwork(roxel)
#'
#' # Quick overview of the network in `ggplot2` style
#' autoplot(net)
#'
#' # Other `ggplot2` elements can be added
#' points = net %>%
#'   st_bbox() %>%
#'   st_as_sfc() %>%
#'   st_sample(10, type = 'random') %>%
#'   st_set_crs(4326) %>%
#'   st_cast('POINT')
#'
#' autoplot(net) +
#'    # The theme can be customized
#'    theme_minimal() +
#'    # Labels can be added
#'    labs(title = 'Nice ggplot') +
#'    # And extra `geom_sf` layers can be included
#'    geom_sf(data = points, color = 'red', size = 2)
#'
#' @name autoplot
#' @importFrom sf st_as_sf
autoplot.sfnetwork = function(object, ...) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = st_as_sf(object, 'nodes')) +
    ggplot2::geom_sf(data = st_as_sf(object, 'edges'))
}

# nocov start
.onLoad = function(...) {
  has_ggplot2_3.0 =
    requireNamespace("ggplot2", quietly = TRUE) &&
    utils::packageVersion("ggplot2") >= "3.0.0"

  if (has_ggplot2_3.0)
    s3_register("ggplot2::autoplot", "sfnetwork")

  invisible()
}

# Register S3 helper
# Code copied from: https://github.com/r-lib/vctrs/blob/master/R/register-s3.R
# As suggested here: https://vctrs.r-lib.org/reference/s3_register.html
s3_register = function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces = strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package = pieces[[1]]
  generic = pieces[[2]]

  caller = parent.frame()

  get_method_env = function() {
    top = topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method = function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  method_fn = get_method(method)
  stopifnot(is.function(method_fn))

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns = asNamespace(package)

      # Refresh the method, it might have been updated by `devtools::load_all()`
      method_fn = get_method(method)

      registerS3method(generic, class, method_fn, envir = ns)
    }
  )

  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }

  envir = asNamespace(package)

  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }

  invisible()
}
# nocov end
