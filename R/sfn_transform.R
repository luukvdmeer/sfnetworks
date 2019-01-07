#' Transform or convert coordinates of sfn_network object
#'
#' Uses the function \link{st_transform} from the package \link{sf} to transform or convert coordinates
#' of both the edges and nodes element of an object of class \code{sfn_network},
#' or of both the route, edges and nodes element of an object of class \code{sfn_route}.
#'
#' @param x object of class \code{sfn_network} or \code{sfn_route}
#' @param crs the target coordinate reference system; must be either an integer with the EPSG code, or a character with proj4string
#' @param ... further specification, passed on to \link{st_transform}
#' @details For a detailed description, see \link{st_transform}.
#' @importFrom magrittr %>%
#' @importFrom purrr pluck
#' @importFrom sf st_transform
#' @export
sfn_transform = function(x, crs, ...) UseMethod("sfn_transform")

#' @name sfn_transform
#' @export
sfn_transform.sfn_network = function(x, crs, ...) {
  # Transform the edges element of the sfn_network object
  x$edges = x %>%
    pluck('edges') %>%
    sf::st_transform(crs, ...)

  # Transform the nodes element of the sfn_network object
  x$nodes = x %>%
    pluck('nodes') %>%
    sf::st_transform(crs, ...)

  return(x)
}

#' @name sfn_transform
#' @export
sfn_transform.sfn_route = function(x, crs, ...) {
  # Transform the route element of the sfn_route object
  x$route = x %>%
    pluck('route') %>%
    sf::st_transform(crs, ...)

  # Transform the nodes element of the sfn_route object
  x$edges = x %>%
    pluck('edges') %>%
    sf::st_transform(crs, ...)

  # Transform the nodes element of the sfn_route object
  x$nodes = x %>%
    pluck('nodes') %>%
    sf::st_transform(crs, ...)

  return(x)
}
