#' s2 methods for sfnetworks
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#' @param ... Arguments passed on the corresponding \code{s2} function.
#'
#' @name s2
#' @export
as_s2_geography.sfnetwork = function(x, ...) {
  s2::as_s2_geography(st_geometry(x))
}
