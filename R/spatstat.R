# Define an auxiliary function which is used to test that the relevant spatstat
# packages are installed and that the spatstat version is 2.0.0 or greater
# Check https://github.com/rubak/spatstat.revdep/blob/main/README.md and
# https://github.com/luukvdmeer/sfnetworks/issues/137 for more details
#' @importFrom utils packageVersion
check_spatstat <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "required; please install it (or the full spatstat package) first."))
  } else {
    spst_ver <- try(packageVersion("spatstat"), silent = TRUE)
    if (!inherits(spst_ver, "try-error") && spst_ver < 2.0-0) {
      stop(paste0(
        "You have an old version of spatstat installed which is incompatible with ",
        pkg,
        ". Please update spatstat (or uninstall it)."
      ))
    }
  }
}

#' Convert a sfnetwork into a linnet
#'
#' A method to convert an object of class \code{\link{sfnetwork}} into
#' \code{\link[spatstat.linnet]{linnet}} format and enhance the interoperability
#' between \code{sfnetworks} and \code{spatstat}. Use this method without the
#' .sfnetwork suffix and after loading the \code{spatstat} package.
#'
#' @param X An object of class \code{\link{sfnetwork}} with a projected CRS.
#'
#' @param ... Arguments passed to \code{\link[spatstat.linnet]{linnet}}.
#'
#' @return An object of class \code{\link[spatstat.linnet]{linnet}}.
#'
#' @seealso \code{\link{as_sfnetwork}} to convert objects of class
#' \code{\link[spatstat.linnet]{linnet}} into objects of class
#' \code{\link{sfnetwork}}.
#'
#' @name as.linnet
as.linnet.sfnetwork <- function(X, ...) {
  # Check the presence and the version of spatstat.geom and spatstat.linnet
  check_spatstat("spatstat.geom")
  check_spatstat("spatstat.linnet")

  # Extract the vertices of the sfnetwork.
  X_vertices_ppp = spatstat.geom::as.ppp(nodes_as_sf(X))
  # Extract the edge list.
  X_edge_list = as.matrix(
    (as.data.frame(activate(X, "edges")))[, c("from", "to")]
  )
  # Build linnet.
  spatstat.linnet::linnet(
    vertices = X_vertices_ppp,
    edges = X_edge_list,
    ...
  )
}
