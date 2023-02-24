# Auxiliary function which is used to test that:
# --> The relevant spatstat packages are installed.
# --> The spatstat version is 2.0.0 or greater.
# For details, see:
# --> https://github.com/rubak/spatstat.revdep/blob/main/README.md
# --> https://github.com/luukvdmeer/sfnetworks/issues/137
#' @importFrom utils packageVersion
check_spatstat = function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package ",
      pkg,
      "required; please install it (or the full spatstat package) first",
      call. = FALSE
    )
  } else {
    spst_ver = try(packageVersion("spatstat"), silent = TRUE)
    if (!inherits(spst_ver, "try-error") && spst_ver < 2.0-0) {
      stop(
        "You have an old version of spatstat which is incompatible with ",
        pkg,
        "; please update spatstat (or uninstall it)",
        call. = FALSE
      )
    }
  }
  check_spatstat_sf()
}

# Auxiliary function which is used to test that:
# --> The sf version is compatible with the new spatstat structure
# For details, see:
# --> https://github.com/luukvdmeer/sfnetworks/pull/138#issuecomment-803430686
#' @importFrom utils packageVersion
check_spatstat_sf = function() {
  if (packageVersion("sf") < "0.9.8") {
    stop(
      "spatstat code requires sf >= 0.9.8; please update sf",
      call. = FALSE
    )
  }
}

#' Convert a sfnetwork into a linnet
#'
#' A method to convert an object of class \code{\link{sfnetwork}} into
#' \code{\link[spatstat.linnet]{linnet}} format and enhance the
#' interoperability between \code{sfnetworks} and \code{spatstat}. Use
#' this method without the .sfnetwork suffix and after loading the
#' \code{spatstat} package.
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
as.linnet.sfnetwork = function(X, ...) {
  # Check the presence and the version of spatstat.geom and spatstat.linnet
  check_spatstat("spatstat.geom")
  check_spatstat("spatstat.linnet")
  # Extract the vertices of the sfnetwork.
  X_vertices_ppp = spatstat.geom::as.ppp(pull_node_geom(X))
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
