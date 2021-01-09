library(sf)
library(dplyr)

rect = roxel %>%
  st_union() %>%
  st_transform(3857) %>%
  st_centroid() %>%
  st_buffer(dist = 500, endCapStyle = "SQUARE") %>%
  st_transform(4326) %>%
  st_as_sf(foo = "bar")

test_that("sf functions for sfnetworks with spatially implicit edges,
         give an error", {
  net = as_sfnetwork(roxel, edges_as_lines = F) %>% activate("edges")
  # Geometries
  expect_error(st_coordinates(net), "Edges are spatially implicit")
  expect_error(st_is(net, "LINESTRING"), "Edges are spatially implicit")
  # Coordinates
  expect_error(st_shift_longitude(net), "Edges are spatially implicit")
  expect_error(st_wrap_dateline(net), "Edges are spatially implicit")
  expect_error(st_transform(net, 3857), "Edges are spatially implicit")
  expect_error(st_zm(net), "Edges are spatially implicit")
  expect_error(st_m_range(net), "Edges are spatially implicit")
  expect_error(st_z_range(net), "Edges are spatially implicit")
  # Geometry binary predicate
  expect_error(st_intersects(net, rect), "Edges are spatially implicit")
  # Geometry unary operations
  expect_error(st_simplify(net), "Edges are spatially implicit")
  # Join and filter
  expect_error(st_crop(net, rect), "Edges are spatially implicit")
  expect_error(st_filter(net, rect), "Edges are spatially implicit")
})


### st_reverse
node1 = st_point(c(0, 0))
node2 = st_point(c(1, 0))
edge = st_sfc(st_linestring(c(node1, node2)))

dirnet = as_sfnetwork(edge)
undirnet = as_sfnetwork(edge, directed = FALSE)

test_that("st_reverse gives a warning when nodes are active, keeping the same
          coordinates order and from/to columns", {
  expect_warning(reversed <- st_reverse(dirnet), "no effect on nodes")
  expect_setequal(st_coordinates(reversed), st_coordinates(dirnet))
  expect_setequal(
    st_coordinates(activate(reversed, "edges")),
    st_coordinates(activate(dirnet, "edges"))
  )
  expect_equal(
    pull(activate(reversed, "edges"), from),
    pull(activate(dirnet, "edges"), from)
  )
  expect_equal(
    pull(activate(reversed, "edges"), to),
    pull(activate(dirnet, "edges"), to)
  )
})

test_that("st_reverse reverses the order of the to/from columns and the
          order of the coordinates for directed networks", {
  expect_warning(
    reversed <- st_reverse(activate(dirnet, "edges")),
    "st_reverse swaps columns"
  )
  expect_equal(
    st_coordinates(reversed)[1, ],
    st_coordinates(activate(dirnet, "edges"))[2, ]
  )
  expect_equal(
    st_coordinates(reversed)[2, ],
    st_coordinates(activate(dirnet, "edges"))[1, ]
  )
  expect_equal(
    pull(activate(reversed, "edges"), from),
    pull(activate(dirnet, "edges"), to)
  )
  expect_equal(
    pull(activate(reversed, "edges"), to),
    pull(activate(dirnet, "edges"), from)
  )
})

test_that("st_reverse reverses the order of the coordinates for
          undirected networks", {
 reversed <- st_reverse(activate(undirnet, "edges"))
 expect_equal(
   st_coordinates(reversed)[1, ],
   st_coordinates(activate(undirnet, "edges"))[2, ]
 )
 expect_equal(
   st_coordinates(reversed)[2, ],
   st_coordinates(activate(undirnet, "edges"))[1, ]
 )
 expect_equal(
   pull(activate(reversed, "edges"), from),
   pull(activate(undirnet, "edges"), from)
 )
 expect_equal(
   pull(activate(reversed, "edges"), to),
   pull(activate(undirnet, "edges"), to)
 )
})
