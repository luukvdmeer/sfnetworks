#' Convert foreign object to sfn_network object
#'
#' Converts a foreign object to an an object of class \code{sfn_network}
#'
#' @param x object to be converted into an sfn_network object; must be either of class \code{sf}, \code{SpatialLinesDataFrame}, \code{osmdata} or \code{sfn_route}
#' @param attributes logical;
#' if \code{TRUE}, copy attribute columns from the input object to the output object of class \code{sfn_network};
#' if \code{FALSE}, only take the geometry from the input object, and drop the attribute columns;
#' by default set to \code{TRUE}
#' @return Returns an object of class \code{sfn_network},
#' which is a list representing a spatial network,
#' containing an sf object with edges (linestring geometry) and an sf object with nodes (point geometry)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr pluck
#' @importFrom dplyr distinct filter group_by group_indices mutate n pull select slice ungroup
#' @importFrom sf st_as_sf st_coordinates st_crs st_geometry st_length st_set_crs st_sf
#' @importFrom stats setNames
#' @export
sfn_asnetwork = function(x, attributes) UseMethod("sfn_asnetwork")

#' @name sfn_asnetwork
#' @export
sfn_asnetwork.sf = function(x, attributes = TRUE) {
  # Make sure the sf object has a linestring geometry
  if(class(sf::st_geometry(x))[1] != "sfc_LINESTRING") {
    stop("The sf object must have a LINESTRING geometry type")
  }

  # The edges of the network are simply the linestrings from the sf object
  edges = x

  # If attributes is set to FALSE, only take geometry; drop other fields
  if(!attributes) {
    edges = edges %>%
      sf::st_geometry() %>%
      sf::st_sf()
  }

  ## Add a unique ID to each edge
  edges = edges %>%
    mutate(edgeID = c(1:n()))

  # The nodes are the startpoints and endpoints of the linestrings
  # The function st_coordinates gets the coordinates of all the points of a line
  # The column L1 specifies to which line a set of point coordinates belongs
  # Filter start and endpoints by grouping by L1 and selecting first and last entry of each group
  # Then, assign a value of 0 to startpoints (source) and 1 to endpoints (target)
  nodes = x %>%
    sf::st_coordinates() %>%
    as.data.frame() %>%
    group_by(.data$L1) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(startend = rep(c(0, 1), times = n()/2))

  # If lines share startpoint and/or endpoint, this point should be one single node
  # That is, duplicated points in the nodes data should have the same unique node ID
  # For this, group the nodes data by the combination (X, Y)
  # Then, retrieve group indices, that form unique IDs for each node
  node_ids = nodes %>%
    group_by(.data$X, .data$Y) %>%
    group_indices()

  # Add this unique node ID as a column to the nodes data
  nodes = nodes %>%
    mutate(nodeID = node_ids)

  # Add two columns to the edges data
  # One column represents the unique node ID of the startnode (source) of each edge
  # The other column represents the unique node ID of the endnode (target) of each edge
  sources = nodes %>%
    filter(.data$startend == 0) %>%
    pull(.data$nodeID)

  targets = nodes %>%
    filter(.data$startend == 1) %>%
    pull(.data$nodeID)

  edges = edges %>%
    mutate(nodeID_source = sources, nodeID_target = targets)

  # The source and target node IDs are added to the edges data
  # Now, duplicate points can be removed from the nodes data
  # That is, each node will only occur once in the nodes data
  # Then, columns L1 and startend are not appropriate anymore and should be removed
  # Finally, convert the nodes data into an sf object with point geometry
  # The CRS of the nodes sf object should be the same as of the inputted sf object
  nodes = nodes %>%
    distinct(.data$nodeID, .keep_all = TRUE) %>%
    select(-c(.data$L1, .data$startend)) %>%
    as.data.frame() %>%
    sf::st_as_sf(coords = c("X", "Y")) %>%
    sf::st_set_crs(sf::st_crs(x))

  # Finally, add a length column to the edges data
  # This column will represent the length in meters of each edge
  edges = edges %>%
    mutate(length = sf::st_length(.data$geometry))

  # Return an object of class sfn_network
  structure(list(edges, nodes), class = "sfn_network") %>%
    setNames(nm = c("edges", "nodes"))
}

#' @name sfn_asnetwork
#' @export
sfn_asnetwork.SpatialLinesDataFrame = function(x, attributes = TRUE) {
  # Transform sp SpatialLinesDataFrame into sf object
  x = sf::st_as_sf(x)

  # Run the sfn_asnetwork function for objects of class sf
  sfn_asnetwork(x, attributes = attributes)
}

#' @name sfn_asnetwork
#' @export
sfn_asnetwork.osmdata_sf = function(x, attributes = TRUE) {
  # This function uses a function from the osmdata package
  # The sfnetworks packages does not depend on osmdata, it only suggests it
  # If the user has the osm package not installed, give an error
  if (!requireNamespace("osmdata", quietly = TRUE)) {
    stop("Package \"osmdata\" is needed to convert an osmdata object into a sfn_network. Please install it.",
         call. = FALSE)
  }

  # Transform polygons into lines
  x = x %>% osmdata::osm_poly2line()

  # Extract the osm_lines sf object with LINESTRING geometry from the osmdata_sf list
  lines = x %>% pluck('osm_lines')

  # Run the sfn_asnetwork function for objects of class sf
  sfn_asnetwork(lines, attributes = attributes)
}

#' @name sfn_asnetwork
#' @export
sfn_asnetwork.sfn_route = function(x, attributes = TRUE) {
  # Select the edges element from the sfn_route object
  edges = x %>%
    pluck('edges')

  # Select the nodes element from the sfn_route object
  nodes = x %>%
    pluck('nodes')

  # If attributes is set to FALSE, drop the attribute columns
  # That is, select only the 'core' columns
  # Geometry will be selected automatically because it is 'sticking' to the elements
  if(!attributes) {
    edges = edges %>%
      select(.data$edgeID, .data$nodeID_source, .data$nodeID_target, .data$length)

    nodes = nodes %>%
      select(.data$nodeID)
  }

  # Return an object of class sfn_network
  structure(list(edges, nodes), class = "sfn_network") %>%
    setNames(nm = c("edges", "nodes"))
}
