% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edge.R
\name{spatial_edge_measures}
\alias{spatial_edge_measures}
\alias{edge_circuity}
\alias{edge_length}
\alias{edge_displacement}
\title{Query spatial edge measures}
\usage{
edge_circuity()

edge_length()

edge_displacement()
}
\value{
A numeric vector of the same length as the number of edges in the
graph.
}
\description{
These functions are a collection of specific spatial edge measures, that
form a spatial extension to edge measures in
\code{\link[tidygraph:tidygraph-package]{tidygraph}}.
}
\details{
Just as with all query functions in tidygraph, spatial edge
measures are meant to be called inside tidygraph verbs such as
\code{\link[tidygraph]{mutate}} or \code{\link[tidygraph]{filter}}, where
the network that is currently being worked on is known and thus not needed
as an argument to the function. If you want to use an algorithm outside of
the tidygraph framework you can use \code{\link[tidygraph]{with_graph}} to
set the context temporarily while the algorithm is being evaluated.
}
\section{Functions}{
\itemize{
\item \code{edge_circuity}: The ratio of the length of an edge
linestring geometry versus the straight-line distance between its boundary
nodes, as described in
\href{https://journals.sagepub.com/doi/10.1068/b130131p}{Giacomin & Levinson,
2015}.

\item \code{edge_length}: The length of an edge linestring geometry
as calculated by \code{\link[sf]{st_length}}.

\item \code{edge_displacement}: The straight-line distance between the two
boundary nodes of an edge, as calculated by \code{\link[sf]{st_distance}}.
}}

\examples{
library(sf, quietly = TRUE)
library(tidygraph, quietly = TRUE)

net = as_sfnetwork(roxel, directed = FALSE) \%>\%
  st_transform(3035)

net \%>\%
  activate("edges") \%>\%
  mutate(circuity = edge_circuity())

net \%>\%
  activate("edges") \%>\%
  mutate(length = edge_length())

net \%>\%
  activate("edges") \%>\%
  mutate(displacement = edge_displacement())

}