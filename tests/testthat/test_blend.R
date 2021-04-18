library(sf)
library(igraph)
library(dplyr)
node1 = st_point(c(0, 0))
node2 = st_point(c(1, 0))
edge = st_sfc(st_linestring(c(node1, node2)), crs = 4326)

net = as_sfnetwork(edge)

pois = data.frame(
    poi_type = c("bakery", "butcher", "market"),
    x = c(0, 0.6, 0.3), y = c(0.1, 0.1, 0)
  ) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

test_that("st_network_blend with tolerance argument too large gives a
          warning and does not perform split", {
  expect_warning(
    blend1 <- st_network_blend(net, pois[1:2, ], tolerance = 0.05),
    "No points were blended"
  )
  expect_equal(vcount(blend1), 2)
  expect_equal(ecount(blend1), 1)
})

test_that("st_network_blend splits edges with nodes on and/or close to
          the network", {
  blend2 <- st_network_blend(net, pois)
  expect_equal(vcount(blend2), 4)
  expect_equal(ecount(blend2), 3)
})

test_that("st_network_blend sorts nodes in the correct way", {
  expect_setequal(
    pull(st_network_blend(net, pois), poi_type),
    c("bakery", NA, "market", "butcher")
  )
})
