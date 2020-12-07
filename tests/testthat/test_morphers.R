library(sf)
library(tidygraph)
library(igraph)

# Toy sfnetwork
p1 = st_point(c(0, 1))
p2 = st_point(c(1, 1))
p3 = st_point(c(2, 1))
p4 = st_point(c(3, 1))
p5 = st_point(c(4, 1))
p6 = st_point(c(3, 2))
p7 = st_point(c(3, 0))
p8 = st_point(c(4, 3))
p9 = st_point(c(4, 2))
p10 = st_point(c(4, 0))
p11 = st_point(c(5, 2))
p12 = st_point(c(5, 0))
p13 = st_point(c(5, -1))

l1 = st_sfc(st_linestring(c(p1, p2, p3)))
l2 = st_sfc(st_linestring(c(p3, p4, p5)))
l3 = st_sfc(st_linestring(c(p6, p4, p7)))
l4 = st_sfc(st_linestring(c(p8, p11, p9)))
l5 = st_sfc(st_linestring(c(p9, p5, p10)))
l6 = st_sfc(st_linestring(c(p8, p9)))
l7 = st_sfc(st_linestring(c(p10, p12, p13, p10)))

points = st_sfc(st_multipoint(c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)))
lines = c(l1, l2, l3, l4, l5, l6, l7)

net = as_sfnetwork(lines)

# Perform spatial subdivision
subdivision = convert(net, to_spatial_subdivision)

# Perform spatial smoothing of pseudo nodes
smoothed = convert(subdivision, to_spatial_smooth)

# Perform network simplification
simple = convert(smoothed, to_spatial_simple)

test_that('the created toy network for morpher testing has
          the expected number of nodes, edges and components', {
  expect_equal(vcount(net), 8)
  expect_equal(ecount(net), 7)
  expect_equal(count_components(net), 3)
})

test_that('to_spatial_subdivision morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components', {
  expect_equal(vcount(subdivision), 9)
  expect_equal(ecount(subdivision), 10)
  expect_equal(count_components(subdivision), 1)
})

test_that('to_spatial_smooth morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components', {
  expect_equal(vcount(smoothed), 8)
  expect_equal(ecount(smoothed), 9)
  expect_equal(count_components(smoothed), 1)
})

test_that('to_spatial_simple morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components', {
  expect_equal(vcount(simple), 8)
  expect_equal(ecount(simple), 7)
  expect_equal(count_components(simple), 1)
})
