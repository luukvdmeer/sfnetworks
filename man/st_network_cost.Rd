% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cost.R
\name{st_network_cost}
\alias{st_network_cost}
\alias{st_network_distance}
\title{Compute a cost matrix of a spatial network}
\usage{
st_network_cost(
  x,
  from = node_ids(x),
  to = node_ids(x),
  weights = edge_length(),
  direction = "out",
  Inf_as_NaN = FALSE,
  router = getOption("sfn_default_router", "igraph"),
  use_names = FALSE,
  ...
)

st_network_distance(
  x,
  from = node_ids(x),
  to = node_ids(x),
  direction = "out",
  Inf_as_NaN = FALSE,
  router = getOption("sfn_default_router", "igraph"),
  use_names = FALSE,
  ...
)
}
\arguments{
\item{x}{An object of class \code{\link{sfnetwork}}.}

\item{from}{The nodes where the paths should start. Evaluated by
\code{\link{evaluate_node_query}}. By default, all nodes in the network are
included.}

\item{to}{The nodes where the paths should end. Evaluated by
\code{\link{evaluate_node_query}}. By default, all nodes in the network are
included.}

\item{weights}{The edge weights to be used in the shortest path calculation.
Evaluated by \code{\link{evaluate_weight_spec}}. The default is
\code{\link{edge_length}}, which computes the geographic lengths of the
edges.}

\item{direction}{The direction of travel. Defaults to \code{'out'}, meaning
that the direction given by the network is followed and costs are computed
from the points given as argument \code{from}. May be set to \code{'in'},
meaning that the opposite direction is followed an costs are computed
towards the points given as argument \code{from}. May also be set to
\code{'all'}, meaning that the network is considered to be undirected. This
argument is ignored for undirected networks.}

\item{Inf_as_NaN}{Should the cost values of unconnected nodes be stored as
\code{NaN} instead of \code{Inf}? Defaults to \code{FALSE}.}

\item{router}{The routing backend to use for the cost matrix computation.
Currently supported options are \code{'igraph'} and \code{'dodgr'}. See
Details.}

\item{use_names}{If a column named \code{name} is present in the nodes
table, should these names be used as row and column names in the matrix,
instead of the node indices? Defaults to \code{FALSE}. Ignored when the
nodes table does not have a column named \code{name}.}

\item{...}{Additional arguments passed on to the underlying function of the
chosen routing backend. See Details.}
}
\value{
An n times m numeric matrix where n is the length of the \code{from}
argument, and m is the length of the \code{to} argument.
}
\description{
Compute total travel costs of shortest paths between nodes in a spatial
network.
}
\details{
The sfnetworks package does not implement its own routing algorithms
to compute cost matrices. Instead, it relies on "routing backends", i.e.
other R packages that have implemented such algorithms. Currently two
different routing backends are supported.

The default is \code{\link[igraph]{igraph}}. This package supports
many-to-many cost matrix computation with the \code{\link[igraph]{distances}}
function. The igraph router does not support dual-weighted routing.

The second supported routing backend is \code{\link[dodgr]{dodgr}}. This
package supports many-to-many cost matrix computation with the
\code{\link[dodgr]{dodgr_dists}} function. It also supports dual-weighted
routing. The dodgr package is a conditional dependency of sfnetworks. Using
the dodgr router requires the dodgr package to be installed.

The default router can be changed by setting the \code{sfn_default_router}
option.
}
\examples{
library(sf, quietly = TRUE)
library(tidygraph, quietly = TRUE)

net = as_sfnetwork(roxel, directed = FALSE) |>
  st_transform(3035)

# Compute the network cost matrix between node pairs.
# Note that geographic edge length is used as edge weights by default.
st_network_cost(net, from = c(495, 121), to = c(495, 121))

# st_network_distance is a synonym for st_network_cost with default weights.
st_network_distance(net, from = c(495, 121), to = c(495, 121))

# Compute the network cost matrix between spatial point features.
# These are snapped to their nearest node before computing costs.
p1 = st_geometry(net, "nodes")[495] + st_sfc(st_point(c(50, -50)))
st_crs(p1) = st_crs(net)
p2 = st_geometry(net, "nodes")[121] + st_sfc(st_point(c(-10, 100)))
st_crs(p2) = st_crs(net)

st_network_cost(net, from = c(p1, p2), to = c(p1, p2))

# Use a node type query function to specify origins and/or destinations.
st_network_cost(net, from = 499, to = node_is_connected(499))

# Use a spatial edge measure to specify edge weights.
# By default edge_length() is used.
st_network_cost(net, c(p1, p2), c(p1, p2), weights = edge_displacement())

# Use a column in the edges table to specify edge weights.
# This uses tidy evaluation.
net |>
  activate("edges") |>
  mutate(foo = runif(n(), min = 0, max = 1)) |>
  st_network_cost(c(p1, p2), c(p1, p2), weights = foo)

# Compute the cost matrix without edge weights.
# Here the cost is defined by the number of edges, ignoring space.
st_network_cost(net, c(p1, p2), c(p1, p2), weights = NA)

# Use the dodgr router for dual-weighted routing.
paths = st_network_cost(net,
  from = c(p1, p2),
  to = c(p1, p2),
  weights = dual_weights(edge_segment_count(), edge_length()),
  router = "dodgr"
)

# Not providing any from or to points includes all nodes by default.
with_graph(net, graph_order()) # Our network has 701 nodes.
cost_matrix = st_network_cost(net)
dim(cost_matrix)

}
\seealso{
\code{\link{st_network_paths}}, \code{\link{st_network_travel}}
}
