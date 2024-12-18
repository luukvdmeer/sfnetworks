% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ids.R
\name{evaluate_edge_query}
\alias{evaluate_edge_query}
\title{Query specific edge indices from a spatial network}
\usage{
evaluate_edge_query(data, query)
}
\arguments{
\item{data}{An object of class \code{\link{sfnetwork}}.}

\item{query}{The query that defines for which edges to extract indices,
defused into a \code{\link[rlang:topic-quosure]{quosure}}. See Details for
the different ways in which edge queries can be formulated.}
}
\value{
A vector of queried edge indices.
}
\description{
This function is not meant to be called directly, but used inside other
functions that accept an edge query.
}
\details{
There are multiple ways in which edge indices can be queried in
sfnetworks. The query can be formatted as follows:

\itemize{
  \item As spatial features: Spatial features can be given as object of
  class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}. The nearest edge to
  each feature is found by calling \code{\link[sf]{st_nearest_feature}}.
  \item As edge type query function: A
  \link[tidygraph:edge_types]{edge type query function} defines for each
  edge if it is of a given type or not. Nodes that meet the criterium are
  queried.
  \item As edge predicate query function: A
  \link[=spatial_edge_predicates]{edge predicate query function} defines
  for each edge if a given spatial predicate applies to the spatial relation
  between that edge and other spatial features. Nodes that meet the
  criterium are queried.
  \item As column name: The referenced column is expected to have logical
  values defining for each edge if it should be queried or not. Note that
  tidy evaluation is used and hence the column name should be unquoted.
  \item As integers: Integers are interpreted as edge indices. A edge index
  corresponds to a row-number in the edges table of the network.
  \item As characters: Characters are interpreted as edge names. A edge name
  corresponds to a value in a column named "name" in the the edges table of
  the network. Note that this column is expected to store unique names
  without any duplicated values.
  \item As logicals: Logicals should define for each edge if it should be
  queried or not.
}

Queries that can not be evaluated in any of the ways described above will be
forcefully converted to integers using \code{\link{as.integer}}.
}
