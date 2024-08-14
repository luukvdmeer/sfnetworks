#' Point locations for places about W. A. Mozart in Salzburg, Austria
#'
#' A dataset containing point locations (museums, sculptures, squares,
#' universities, etc.) of places named after Wolfgang Amadeus Mozart
#' in the city of Salzburg, Austria.
#' The data are taken from OpenStreetMap.
#' See `data-raw/mozart.R` for code on its creation.
#'
#' @format An object of class \code{\link[sf]{sf}} with \code{LINESTRING}
#' geometries, containing 851 features and three columns:
#' \describe{
#'   \item{name}{the name of the point location}
#'   \item{type}{the type of location, e.g. museum, artwork, cinema, etc.}
#'   \item{website}{the website URL for more information
#'   about the place, if available}
#'   \item{geometry}{the geometry list column}
#' }
#' @source \url{https://www.openstreetmap.org}
"mozart"
