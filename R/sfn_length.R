#' Total length of a spatial network
#'
#' Computes the total length of the edges in either a spatial network of class \code{sfn_network},
#' or a specific route in such a spatial network, of class \code{sfn_route}.
#'
#' @param x object of class \code{sfn_network} or \code{sfn_route}
#' @return Returns the total length in meters.
#' @importFrom magrittr %>%
#' @importFrom purrr pluck
#' @importFrom sf st_length
#' @export
sfn_length = function(x) UseMethod("sfn_length")

#' @name sfn_length
#' @export
sfn_length.sfn_network = function(x) {
  x %>%
    pluck('edges') %>%
    sf::st_length() %>%
    sum()
}

#' @name sfn_length
#' @export
sfn_length.sfn_route = function(x) {
  x %>%
    pluck('edges') %>%
    sf::st_length() %>%
    sum()
}
