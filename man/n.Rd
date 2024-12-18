% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{n}
\alias{n}
\alias{n_nodes}
\alias{n_edges}
\title{Count the number of nodes or edges in a network}
\usage{
n_nodes(x, focused = FALSE)

n_edges(x, focused = FALSE)
}
\arguments{
\item{x}{An object of class \code{\link{sfnetwork}}, or any other network
object inheriting from \code{\link[igraph]{igraph}}.}

\item{focused}{Should only features that are in focus be counted? Defaults
to \code{FALSE}. See \code{\link[tidygraph]{focus}} for more information on
focused networks.}
}
\value{
An integer.
}
\description{
Count the number of nodes or edges in a network
}
\examples{
net = as_sfnetwork(roxel)
n_nodes(net)
n_edges(net)

}
