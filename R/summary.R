#' @importFrom magrittr %>%
#' @importFrom purrr pluck
#' @importFrom sf st_crs
#' @export
summary.sfn_network = function(object, ...) {
  # Define number of edges and number of nodes
  n_edges = object %>% pluck('edges') %>% nrow()
  n_nodes = object %>% pluck('nodes') %>% nrow()

  # Define the total length
  length = sfn_length(object)

  # Define the CRS
  crs = (object %>% pluck('edges') %>% sf::st_crs())[[1]]

  # Create a named matrix as output
  output = round(matrix(c(n_edges, n_nodes, length, crs), ncol = 1), digits = 0)
  rownames(output) = c("Number of edges", "Number of nodes", "Total length (m)", "CRS EPSG")
  colnames(output) = "value"

  return(output)
}

#' @export
summary.sfn_route = function(object, ...) {
  # Define number of edges and number of nodes
  n_edges = object %>% pluck('edges') %>% nrow()
  n_nodes = object %>% pluck('nodes') %>% nrow()

  # Define the total length
  weight = object %>% pluck('weight')

  # Define the CRS
  crs = (object %>% pluck('edges') %>% sf::st_crs())[[1]]

  # Create a named matrix as output
  output = round(matrix(c(n_edges, n_nodes, weight, crs), ncol = 1), digits = 0)
  rownames(output) = c("Number of edges", "Number of nodes", "Total weight", "CRS EPSG")
  colnames(output) = "value"

  return(output)
}
