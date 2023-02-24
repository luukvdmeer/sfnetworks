library(sf)
library(igraph)
library(dplyr)

# Create toy network
n11 = st_point(c(0, 0))
n12 = st_point(c(1, 1))
e1 = st_sfc(st_linestring(c(n11, n12)), crs = 4326)
n21 = n12
n22 = st_point(c(0, 2))
e2 = st_sfc(st_linestring(c(n21, n22)), crs = 4326)
n31 = n22
n32 = st_point(c(-1, 1))
e3 = st_sfc(st_linestring(c(n31, n32)), crs = 4326)
net = as_sfnetwork(st_as_sf(c(e1, e2, e3)))

# Extract nodes from network as points
pts = st_as_sf(net, "nodes")[1:2, ]

# Create random points inside network bbox
r1 = st_sfc(st_point(c(-0.8567691, 1.019509)), crs = 4326)
r2 = st_sfc(st_point(c(0.26821, 0.8845114)), crs = 4326)
r3 = st_sfc(st_point(c(0.1343684, 1.421266)), crs = 4326)
r4 = st_sfc(st_point(c(0.7515197, 0.5485984)), crs = 4326)
rdm = c(r1, r2, r3, r4)

## st_join
test_that("st_join gives a warning when there are multiple node matches", {
  ptsdup = rbind(pts, pts)
  expect_warning(
    st_join(net, ptsdup),
    "Multiple matches were detected from some nodes. "
  )
})

# Find indices of nearest nodes.
nearest_nodes = st_nearest_feature(rdm, net)

# Replace geometries.
snapped_rdm = st_sf(foo = letters[1:4], rdm) %>%
  st_set_geometry(st_geometry(net)[nearest_nodes])

test_that("st_join results in the correct attributes and number of nodes and
          edges", {
  joined = st_join(net, snapped_rdm[1:3, ])
  expect_equal(vcount(joined), vcount(net))
  expect_equal(ecount(joined), ecount(net))
  expect_setequal(pull(joined, foo), c("c", "a", NA, "b"))
})

# Create line feature
lines = st_sf(bar = letters[1:2], geom = c(e2, e3))

test_that("st_join on the edges results in the correct attributes and number of
          nodes and edges", {
  joined = st_join(activate(net, "edges"), lines, join = st_equals)
  expect_equal(vcount(joined), vcount(net))
  expect_equal(ecount(joined), ecount(net))
  expect_setequal(pull(joined, bar), c(NA, "a", "b"))
})

# st_network_join
test_that("st_network_join creates a sfnetwork before joining an sf to a
          network", {
  rdm_net = as_sfnetwork(rdm)
  expect_true(igraph::identical_graphs(
    st_network_join(net, rdm_net),
    st_network_join(net, rdm)
  ))
  # expect_setequal(st_network_join(net, rdm_net), st_network_join(net, rdm))
})
