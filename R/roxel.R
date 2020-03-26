#' Road network of Münster Roxel
#'
#' A dataset containing the road network (roads, bikelanes, footpaths, et cetera) of
#' Roxel, a neighborhood in the city of Münster, Germany. The data is taken from
#' OpenStreetMap, querying by key = 'highway'. The topology is cleaned with the
#' v.clean tool in GRASS GIS.
#'
#' @format An sf object with linestring geometry, containing 851 features and three columns:
#' \describe{
#'   \item{name}{the name of the road, if it exists}
#'   \item{type}{the road type, one of (cyclway, footway, path, pedestrian, residential, secondary, service, track, unclassified)}
#'   \item{geometry}{the sf geometry list column}
#' }
#' @source \url{https://www.openstreetmap.org}
"roxel"
