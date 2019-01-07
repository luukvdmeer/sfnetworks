#' @importFrom magrittr %>%
#' @importFrom purrr pluck
#' @export
print.sfn_network = function(x, ...) {
  # Define number of edges and number of nodes
  n_edges = x %>% pluck('edges') %>% nrow()
  n_nodes = x %>% pluck('nodes') %>% nrow()

  # Title
  cat(paste('A spatial network with', n_edges, 'edges and', n_nodes, 'nodes', '\n'))

  # Edges element
  cat(paste0('\n', '--> Edges element; an sf object with linestring geometry', '\n'))
  print(x$edges, n = 5)

  # Nodes element
  cat(paste0('\n', '--> Nodes element; an sf object with point geometry', '\n'))
  print(x$nodes, n = 5)

  invisible(x)
}

#' @export
print.sfn_route = function(x, ...) {
  # Define number of edges and number of nodes
  n_edges = x %>% pluck('edges') %>% nrow()
  n_nodes = x %>% pluck('nodes') %>% nrow()

  # Title
  cat(paste('A route with', n_edges, 'edges and', n_nodes, 'nodes', '\n'))

  # Route element
  cat(paste0('\n', '--> Route element; a sf geometrycollection', '\n'))
  print(x$route, n = 5)

  # Edges element
  cat(paste0('\n', '--> Edges element; an sf object with linestring geometry', '\n'))
  print(x$edges, n = 5)

  # Nodes element
  cat(paste0('\n', '--> Nodes element; an sf object with point geometry', '\n'))
  print(x$nodes, n = 5)

  # Weight element
  cat(paste0('\n', '--> Weight element; the total weight of the route', '\n'))
  cat(paste(x$weight))

  invisible(x)
}
