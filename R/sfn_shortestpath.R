#' Shortest path between two nodes in a sfn_network
#'
#' Calculates the shortest path between two nodes in a spatial network of class \code{sfn_network}.
#'
#' @param x object of class \code{sfn_network}
#' @param start unique node ID of the node from which the shortest path should be calculated
#' @param end unique node ID of the node to which the shortest path should lead
#' @param weights either a string specifying the name of a column in the edges element of the sfn_network object,
#' which values should be used as edge weights;
#' or a numeric vector of the same length as the edge element of the sfn_network object,
#' containing the values that should be used as edge weights;
#' by default set to 'length', which is the name of the column containing the length in meters of each edge
#' @param directed logical; defines whether or not to use a directed graph for calculating the shortest path;
#' by default set to \code{FALSE}
#' @return Returns an object of class \code{sfn_route},
#' which is a list representing a specific route in a spatial network,
#' containing an sf object with the course of the route (geometrycollection),
#' an sf object with the edges that the route is made up of (linestring geometry),
#' an sf object with the nodes that the route is made up of (point geometry),
#' and a value representing the total weight of the route.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map2 pluck
#' @importFrom dplyr arrange filter slice
#' @importFrom sf st_crs st_geometry st_geometrycollection st_linestring st_set_crs st_sf st_sfc
#' @importFrom igraph edge_attr shortest_paths V
#' @importFrom stats setNames
#' @export
sfn_shortestpath = function(x, start, end, weights = 'length', directed = FALSE) {
  # A shortest path can be computed only from one single node at a time
  # A shortest path can be computed only to one single node at a time
  # If more than one nodeID values are given to 'from' and/or 'to', give an error
  if(length(start) > 1 | length(end) > 1){
    stop("Shortest path can only be computed between two nodes at a time")
  }

  # Convert the sfn_network object into an igraph object
  graph = sfn_network2graph(
    x,
    weights = weights,
    directed = directed
  )

  # Retrieve the 'vertex sequence' from the igraph object
  # A 'vertex sequence' is a class in igraph, and contains all nodes of the graph
  vertex_seq = igraph::V(graph)

  # Compute the shortest path with the igraph package
  path = igraph::shortest_paths(
    graph = graph,
    from = vertex_seq[vertex_seq$name == start],
    to = vertex_seq[vertex_seq$name == end],
    output = 'both'
  )

  # Retrieve the indices of the edges in the shortest path
  edge_ids = path %>%
    pluck('epath') %>%
    unlist()

  # If the shortest path has no edges, give an error
  if(length(edge_ids) == 0) {
    stop("There is no connection between these nodes")
  }

  # Retrieve the indices of the nodes in the shortest path
  node_ids = path %>%
    pluck('vpath') %>%
    unlist() %>%
    names() %>%
    as.numeric

  # Select the shortest path edges from the edges element in the sfn_network object
  # Arrange the selected edges in the order that they appear in the shortest path
  edges = x %>%
    pluck('edges') %>%
    filter(.data$edgeID %in% edge_ids) %>%
    arrange(factor(.data$edgeID, levels = edge_ids))

  # Select the shortest path nodes from the nodes element in the sfn_network object
  # Arrange the selected nodes in the order that they appear in the shortest path
  nodes = x %>%
    pluck('nodes') %>%
    filter(.data$nodeID %in% node_ids) %>%
    arrange(factor(.data$nodeID, levels = node_ids))

  # Get the geometries from the nodes element
  # These will later be stored in an extra geometrycollection element
  geom_nodes = nodes %>% sf::st_geometry()

  # Get the geometries from the edges element
  # These will later be stored in an extra geometrycollection element
  geom_edges = edges %>% sf::st_geometry()

  # Since a route always has one more node than edge, add an empty edge to the end
  # This is just to give the edges element the same length as the nodes element
  # Otherwise, the 'map2' function will not work
  # Later, the empty edge will be removed again
  empty_line = sf::st_linestring() %>%
    sf::st_sfc() %>%
    sf::st_set_crs(sf::st_crs(edges))

  geom_edges = c(geom_edges, empty_line)

  # Run the map2 function
  # In this step, the geometry of each node and edge will be extracted one by one
  # The extracted geometry for nodes will be of class ("XY", "POINT", "sfg")
  # The extracted geometry for edges will be of class ("XY", "LINESTRING", "sfg")
  # The extraction takes place in the order that the nodes and edges appears in the shortest path
  # Each (node, edge)-combination is stored in a list
  # These lists will then, by the map2 function, be stored in a list themselves
  geom_all = purrr::map2(
    .x = geom_nodes,
    .y = geom_edges,
    .f = function(x, y) {
      node = x %>% sf::st_geometry() %>% pluck(1)
      edge = y %>% sf::st_geometry() %>% pluck(1)
      list(node, edge)
    }
  )

  # Each node and edge geometry is an element in a list for the (node-edge)-combination within one overall list
  # This (node-edge)-combination list is on its own an element (sublist) in one overall list
  # Unlist the sublist structure, such that each node and edge geometry is an element in the overall list
  geom_all = geom_all %>%
    unlist(recursive = FALSE)

  # Remove the empty linestring
  geom_all = geom_all[-which(sapply(geom_all, is.null))]

  # Create a geometrycollection out of the list
  # Convert into sf object
  # The CRS should be the same as the elements of the inputted sfn_network object
  geometry = geom_all %>%
    sf::st_geometrycollection() %>%
    sf::st_sfc() %>%
    sf::st_sf() %>%
    sf::st_set_crs(sf::st_crs(x$edges))

  # Select the weights corresponding to the edges in the shortest path
  # Sum the selected weights
  weights_sum = igraph::edge_attr(graph = graph, name = "weight") %>%
    as.data.frame() %>%
    slice(edge_ids) %>%
    sum()

  # Return an object of class sfn_route
  structure(list(geometry, edges, nodes, weights_sum), class = "sfn_route") %>%
    setNames(nm = c("route", "edges", "nodes", "weight"))
}
