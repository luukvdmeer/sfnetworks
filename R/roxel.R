#' Road network of Münster Roxel
#'
#' A dataset containing the road network (roads, bikelanes, footpaths, etc.) of
#' Roxel, a neighborhood in the city of Münster, Germany. The data are taken
#' from OpenStreetMap, querying by key = 'highway'.
#' See `data-raw/roxel.R` for code on its creation.
#'
#' @format An object of class \code{\link[sf]{sf}} with \code{LINESTRING}
#' geometries, containing 851 features and three columns:
#' \describe{
#'   \item{name}{the name of the road, if it exists}
#'   \item{type}{the type of the road, e.g. cycleway}
#'   \item{geometry}{the geometry list column}
#' }
#' @source \url{https://www.openstreetmap.org}
"roxel"
