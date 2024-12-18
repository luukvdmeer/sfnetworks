% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dodgr.R
\name{sfnetwork_to_dodgr}
\alias{sfnetwork_to_dodgr}
\alias{dodgr_to_sfnetwork}
\title{Conversion between dodgr streetnets and sfnetworks}
\usage{
dodgr_to_sfnetwork(x, edges_as_lines = TRUE)

sfnetwork_to_dodgr(x, weights = edge_length(), time = FALSE)
}
\arguments{
\item{x}{For the conversion to sfnetwork: an object of class
\code{\link[dodgr]{dodgr_streetnet}}. For the conversion from sfnetwork: an
object of class \code{\link{sfnetwork}}.}

\item{edges_as_lines}{Should the created edges be spatially explicit, i.e.
have \code{LINESTRING} geometries stored in a geometry list column? Defaults
to \code{TRUE}.}

\item{weights}{The edge weights to be stored in the dodgr streetnet.
Evaluated by \code{\link{evaluate_weight_spec}}. The default is
\code{\link{edge_length}}, which computes the geographic lengths of the
edges. Dual-weights can be provided through \code{\link{dual_weights}}.}

\item{time}{Are the provided weights time values? If \code{TRUE}, they will
be stored in a column named 'time' rather than 'd'. Defaults to \code{FALSE}.}
}
\value{
For the conversion to sfnetwork: An object of class
\code{\link{sfnetwork}}. For the conversion from sfnetwork: an object of
class \code{\link[dodgr]{dodgr_streetnet}}.
}
\description{
The \code{\link[dodgr:dodgr-package]{dodgr}} package is designed for routing
on directed graphs, and is known for its fast computations of cost matrices,
shortest paths, and more. In sfnetwork, dodgr can be chosen as a routing
backend.
}
\note{
The \code{\link[dodgr:dodgr-package]{dodgr}} package is designed for
directed graphs. If the provided \code{\link{sfnetwork}} object is
undirected, it is made directed by duplicating and reversing each edge.
}
