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
set.seed(27)
rdm = net %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_sample(4, type = "random")

## st_join
test_that("network breaks when there are multiple
          node matches given to st_join", {
  ptsdup = rbind(pts, pts)
  expect_error(st_join(net, ptsdup), "One or more nodes have multiple matches")
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

## st_network_join
test_that("st_network_join creates an sfnetwork before joining an sf to a
          network", {
  rdm_net = as_sfnetwork(rdm)
  expect_setequal(st_network_join(net, rdm_net), st_network_join(net, rdm))
})
