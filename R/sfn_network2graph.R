#' Convert sfn_network object to igraph object
#'
#' Converts an object of class \code{sfn_network} to an object of class \code{igraph}.
#'
#' @param x object of class \code{sfn_network}
#' @param weights either a string specifying the name of a column in the edges element of the sfn_network object,
#' which values should be used as edge weights;
#' or a numeric vector of the same length as the edge element of the sfn_network object,
#' containing the values that should be used as edge weights;
#' by default set to 'length', which is the name of the column containing the length in meters of each edge
#' @param directed logical; defines whether or not to create a directed graph;
#' by default set to \code{FALSE}
#' @return Returns an object of class \code{igraph}, which is a class for mathematical graphs in the package \link{igraph}.
#' @details The spatial locations of the edges and nodes of the sfn_network object will not be maintained in the returned igraph object.
#' The returned graph is simply a set of weighted connections between unlocated nodes.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr pluck
#' @importFrom dplyr pull select
#' @importFrom sf st_set_geometry
#' @importFrom igraph graph.data.frame set_edge_attr
#' @export
sfn_network2graph = function(x, weights = 'length', directed = FALSE) {
  # Extract the nodeID column from the nodes element of the sfnetwork object
  # Only keep the nodeID, drop the geometry column
  nodes = x %>%
    pluck('nodes') %>%
    select(.data$nodeID) %>%
    sf::st_set_geometry(NULL)

  # Extract the nodeID_source column from the edges element of the sfnetwork object
  # Extract the nodeID_target column from the edges element of the sfnetwork object
  # Only keep the nodeID columns, drop the geometry column
  edges = x %>%
    pluck('edges') %>%
    select(.data$nodeID_source, .data$nodeID_target) %>%
    sf::st_set_geometry(NULL)

  # Create an igraph object from the two dataframes
  graph = igraph::graph.data.frame(
    edges,
    directed = directed,
    vertices = nodes
  )

  # Define the weights vector
  # If the weights variable is a character, extract the values of corresponding column
  # Give an error if more than one character is given
  if(is.character(weights)) {
    if(length(weights) > 1) {
      stop("Weights must either be a single character specifying a column name in the edges data, or a numeric vector containing a weight value for each edge")
    } else{
      weight_vector = x %>%
        pluck('edges') %>%
        pull(weights)
    }
  }

  # If the weights variable is a numeric vector, use this as weights vector
  # Give an error if length of the numeric vector is not equal to number of edges
  else if(is.numeric(weights)) {
    if(length(weights) != nrow(x$edges)) {
      stop("The length of the weights vector must equal the number of edges")
    } else{
      weight_vector = weights
    }
  }

  # If the weights variable is none of the above, give an error
  else{
    stop("Weights must either be a single character specifying a column name in the edges data, or a numeric vector containing a weight value for each edge")
  }

  # Set the weights as attribute of the edges
  graph %>%
    igraph::set_edge_attr("weight", value = weight_vector)
}
