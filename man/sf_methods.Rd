% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf.R
\name{sf_methods}
\alias{sf_methods}
\alias{st_as_sf.sfnetwork}
\alias{st_geometry.sfnetwork}
\alias{st_geometry<-.sfnetwork}
\alias{st_drop_geometry.sfnetwork}
\alias{st_bbox.sfnetwork}
\alias{st_coordinates.sfnetwork}
\alias{st_is.sfnetwork}
\alias{st_is_valid.sfnetwork}
\alias{st_as_s2.sfnetwork}
\alias{st_crs.sfnetwork}
\alias{st_crs<-.sfnetwork}
\alias{st_precision.sfnetwork}
\alias{st_set_precision.sfnetwork}
\alias{st_shift_longitude.sfnetwork}
\alias{st_transform.sfnetwork}
\alias{st_wrap_dateline.sfnetwork}
\alias{st_normalize.sfnetwork}
\alias{st_zm.sfnetwork}
\alias{st_m_range.sfnetwork}
\alias{st_z_range.sfnetwork}
\alias{st_agr.sfnetwork}
\alias{st_agr<-.sfnetwork}
\alias{st_reverse.sfnetwork}
\alias{st_segmentize.sfnetwork}
\alias{st_simplify.sfnetwork}
\alias{st_join.sfnetwork}
\alias{st_join.morphed_sfnetwork}
\alias{st_filter.sfnetwork}
\alias{st_filter.morphed_sfnetwork}
\alias{st_crop.sfnetwork}
\alias{st_crop.morphed_sfnetwork}
\alias{st_difference.sfnetwork}
\alias{st_difference.morphed_sfnetwork}
\alias{st_intersection.sfnetwork}
\alias{st_intersection.morphed_sfnetwork}
\alias{st_intersects.sfnetwork}
\alias{st_sample.sfnetwork}
\alias{st_nearest_points.sfnetwork}
\alias{st_area.sfnetwork}
\title{sf methods for sfnetworks}
\usage{
\method{st_as_sf}{sfnetwork}(x, active = NULL, focused = TRUE, ...)

\method{st_geometry}{sfnetwork}(obj, active = NULL, focused = TRUE, ...)

\method{st_geometry}{sfnetwork}(x) <- value

\method{st_drop_geometry}{sfnetwork}(x, ...)

\method{st_bbox}{sfnetwork}(obj, active = NULL, ...)

\method{st_coordinates}{sfnetwork}(x, active = NULL, ...)

\method{st_is}{sfnetwork}(x, ...)

\method{st_is_valid}{sfnetwork}(x, ...)

\method{st_as_s2}{sfnetwork}(x, active = NULL, focused = TRUE, ...)

\method{st_crs}{sfnetwork}(x, ...)

\method{st_crs}{sfnetwork}(x) <- value

\method{st_precision}{sfnetwork}(x)

\method{st_set_precision}{sfnetwork}(x, precision)

\method{st_shift_longitude}{sfnetwork}(x, ...)

\method{st_transform}{sfnetwork}(x, ...)

\method{st_wrap_dateline}{sfnetwork}(x, ...)

\method{st_normalize}{sfnetwork}(x, ...)

\method{st_zm}{sfnetwork}(x, ...)

\method{st_m_range}{sfnetwork}(obj, active = NULL, ...)

\method{st_z_range}{sfnetwork}(obj, active = NULL, ...)

\method{st_agr}{sfnetwork}(x, active = NULL, ...)

\method{st_agr}{sfnetwork}(x) <- value

\method{st_reverse}{sfnetwork}(x, ...)

\method{st_segmentize}{sfnetwork}(x, ...)

\method{st_simplify}{sfnetwork}(x, ...)

\method{st_join}{sfnetwork}(x, y, ..., ignore_multiple = TRUE)

\method{st_join}{morphed_sfnetwork}(x, y, ...)

\method{st_filter}{sfnetwork}(x, y, ...)

\method{st_filter}{morphed_sfnetwork}(x, y, ...)

\method{st_crop}{sfnetwork}(x, y, ...)

\method{st_crop}{morphed_sfnetwork}(x, y, ...)

\method{st_difference}{sfnetwork}(x, y, ...)

\method{st_difference}{morphed_sfnetwork}(x, y, ...)

\method{st_intersection}{sfnetwork}(x, y, ...)

\method{st_intersection}{morphed_sfnetwork}(x, y, ...)

\method{st_intersects}{sfnetwork}(x, y, ...)

\method{st_sample}{sfnetwork}(x, ...)

\method{st_nearest_points}{sfnetwork}(x, y, ...)

\method{st_area}{sfnetwork}(x, ...)
}
\arguments{
\item{x}{An object of class \code{\link{sfnetwork}}.}

\item{active}{Which network element (i.e. nodes or edges) to activate before
extracting. If \code{NULL}, it will be set to the current active element of
the given network. Defaults to \code{NULL}.}

\item{focused}{Should only features that are in focus be extracted? Defaults
to \code{TRUE}. See \code{\link[tidygraph]{focus}} for more information on
focused networks.}

\item{...}{Arguments passed on the corresponding \code{sf} function.}

\item{obj}{An object of class \code{\link{sfnetwork}}.}

\item{value}{The value to be assigned. See the documentation of the
corresponding sf function for details.}

\item{precision}{The precision to be assigned. See
\code{\link[sf]{st_precision}} for details.}

\item{y}{An object of class \code{\link[sf]{sf}}, or directly convertible to
it using \code{\link[sf]{st_as_sf}}. In some cases, it can also be an object
of \code{\link[sf:st]{sfg}} or \code{\link[sf:st_bbox]{bbox}}. Always look
at the documentation of the corresponding \code{sf} function for details.}

\item{ignore_multiple}{When performing a spatial join with the nodes
table, and there are multiple matches for a single node, only the first one
of them is joined into the network. But what should happen with the others?
If this argument is set to \code{TRUE}, they will be ignored. If this
argument is set to \code{FALSE}, they will be added as isolated nodes to the
returned network. Nodes at equal locations can then be merged using the
spatial morpher \code{\link{to_spatial_unique}}. Defaults to \code{TRUE}.}
}
\value{
The methods for \code{\link[sf]{st_join}},
\code{\link[sf]{st_filter}}, \code{\link[sf]{st_intersection}},
\code{\link[sf]{st_difference}} and \code{\link[sf]{st_crop}}, as well as
the methods for all setter functions and the geometric unary operations
preserve the class of the object it is applied to, i.e. either a
\code{\link{sfnetwork}} object or its morphed equivalent. When dropping node
geometries, an object of class \code{\link[tidygraph]{tbl_graph}} is
returned. All other methods return the same type of objects as their
corresponding sf function. See the \code{\link[sf]{sf}} documentation for
details.
}
\description{
\code{\link[sf]{sf}} methods for \code{\link{sfnetwork}} objects.
}
\details{
See the \code{\link[sf]{sf}} documentation. The following methods
have a special behavior:

\itemize{
  \item \code{st_geometry<-}: The geometry setter requires the replacement
  geometries to have the same CRS as the network. Node replacements should
  all be points, while edge replacements should all be linestrings. When
  replacing node geometries, the boundaries of the edge geometries are
  replaced as well to preserve the valid spatial network structure. When
  replacing edge geometries, new edge boundaries that do not match the
  location of their specified incident node are added as new nodes to the
  network.
  \item \code{st_transform}: No matter if applied to the nodes or edge
  table, this method will update the coordinates of both tables. The same
  holds for all other methods that update the way in which the coordinates
  are encoded without changing their actual location, such as
  \code{st_precision}, \code{st_normalize}, \code{st_zm}, and others.
  \item \code{st_join}: When applied to the nodes table and multiple matches
  exist for the same node, only the first match is joined. A warning will be
  given in this case. If \code{ignore_multiple = FALSE}, multiple mathces
  are instead added as isolated nodes to the returned network.
  \item \code{st_intersection}, \code{st_difference} and \code{st_crop}:
  These methods clip edge geometries when applied to the edges table. To
  preserve a valid spatial network structure, clipped edge boundaries are
  added as new nodes to the network.
  \item \code{st_reverse}: When reversing edge geometries in a directed
  network, the indices in the from and to columns will be swapped as well.
  \item \code{st_segmentize}: When segmentizing edge geometries, the edge
  boundaries are forced to remain the same such that the valid spatial
  network structure is preserved. This may lead to slightly inaccurate
  results.
}

Geometric unary operations are only supported on \code{\link{sfnetwork}}
objects if they do not change the geometry type nor the spatial location
of the original features, since that would break the valid spatial network
structure. When applying the unsupported operations, first extract the
element of interest (nodes or edges) using \code{\link[sf]{st_as_sf}}.
}
\examples{
library(sf, quietly = TRUE)

oldpar = par(no.readonly = TRUE)
par(mar = c(1,1,1,1), mfrow = c(1,2))

net = as_sfnetwork(roxel)

# Extract the active network element as sf object.
st_as_sf(net)

# Extract any network element as sf object.
st_as_sf(net, "edges")

# Get the geometry of the active network element.
st_geometry(net)

# Get the geometry of any network element.
st_geometry(net, "edges")

# Replace the geometry of the nodes.
# This will automatically update edge geometries to match the new nodes.
newnet = net
newnds = rep(st_centroid(st_combine(st_geometry(net))), n_nodes(net))
st_geometry(newnet) = newnds

plot(net)
plot(newnet)

# Drop the geometries of the edges.
# This returns an sfnetwork with spatially implicit edges.
st_drop_geometry(activate(net, "edges"))

# Drop the geometries of the nodes.
# This returns a tbl_graph.
st_drop_geometry(net)

# Get the bounding box of the active network element.
st_bbox(net)

# Get CRS of the network.
st_crs(net)

# Get agr factor of the active network element.
st_agr(net)

# Get agr factor of any network element.
st_agr(net, "edges")

# Spatial join applied to the active network element.
net = st_transform(net, 3035)
codes = st_as_sf(st_make_grid(net, n = c(2, 2)))
codes$post_code = as.character(seq(1000, 1000 + nrow(codes) * 10 - 10, 10))

joined = st_join(net, codes, join = st_intersects)
joined

plot(net, col = "grey")
plot(codes, col = NA, border = "red", lty = 4, lwd = 4, add = TRUE)
text(st_coordinates(st_centroid(st_geometry(codes))), codes$post_code)

plot(st_geometry(joined, "edges"))
plot(st_as_sf(joined, "nodes"), pch = 20, add = TRUE)
par(oldpar)

# Spatial filter applied to the active network element.
p1 = st_point(c(4151358, 3208045))
p2 = st_point(c(4151340, 3207520))
p3 = st_point(c(4151756, 3207506))
p4 = st_point(c(4151774, 3208031))

poly = st_multipoint(c(p1, p2, p3, p4)) |>
  st_cast('POLYGON') |>
  st_sfc(crs = 3035) |>
  st_as_sf()

filtered = st_filter(net, poly, .pred = st_intersects)

plot(net, col = "grey")
plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
plot(filtered)

par(oldpar)

}
