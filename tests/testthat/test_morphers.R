library(sf)
library(tidygraph)
library(igraph)
library(dplyr)

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


# Create network from lines
lines = c(l1, l2, l3, l4, l5, l6, l7)
net_l = as_sfnetwork(lines)

# Create network from points
points = st_sfc(st_multipoint(c(p1, p2, p3, p4, p5, p6, p4, p7, p5, p8,
                               p11, p9, p5, p10, p12, p13, p10)))
net_p = as_sfnetwork(st_cast(points, "POINT"))

# Create directed Roxel network
net_d = as_sfnetwork(roxel)

# Create undirected Roxel network
net_u = as_sfnetwork(roxel, directed = FALSE)

# Perform network subdivision
suppressWarnings({
  subd_l <- convert(net_l, to_spatial_subdivision)
  subd_p <- convert(net_p, to_spatial_subdivision)
  subd_d <- convert(net_d, to_spatial_subdivision)
  subd_u <- convert(net_u, to_spatial_subdivision)
})

# Perform spatial smoothing of pseudo nodes
smoo_l = convert(subd_l, to_spatial_smooth)
smoo_p = convert(subd_p, to_spatial_smooth)
smoo_d = convert(subd_d, to_spatial_smooth)
smoo_u = convert(subd_u, to_spatial_smooth)

# Perform network simplification
simp_l = convert(smoo_l, to_spatial_simple)
simp_p = convert(smoo_p, to_spatial_simple)
simp_d = convert(smoo_d, to_spatial_simple)
simp_u = convert(smoo_u, to_spatial_simple)

test_that("the created toy network from lines for morpher testing has
          the expected number of nodes, edges and components", {
  expect_equal(vcount(net_l), 8)
  expect_equal(ecount(net_l), 7)
  expect_equal(count_components(net_l), 3)
  expect_equal(vcount(net_p), 17)
  expect_equal(ecount(net_p), 16)
  expect_equal(count_components(net_p), 1)
  expect_equal(vcount(net_d), 701)
  expect_equal(ecount(net_d), 851)
  expect_equal(count_components(net_d), 14)
  expect_equal(vcount(net_u), 701)
  expect_equal(ecount(net_u), 851)
  expect_equal(count_components(net_u), 14)
})

test_that("to_spatial_subdivision morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(vcount(subd_l), 9)
  expect_equal(ecount(subd_l), 10)
  expect_equal(count_components(subd_l), 1)
  expect_equal(vcount(subd_p), 13)
  expect_equal(ecount(subd_p), 16)
  expect_equal(count_components(subd_p), 1)
  expect_equal(vcount(subd_d), 701)
  expect_equal(ecount(subd_d), 876)
  expect_equal(count_components(subd_u), 1)
  expect_equal(vcount(subd_u), 701)
  expect_equal(ecount(subd_u), 876)
  expect_equal(count_components(subd_u), 1)
})

test_that("to_spatial_smooth morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(vcount(smoo_l), 8)
  expect_equal(ecount(smoo_l), 9)
  expect_equal(count_components(smoo_l), 1)
  expect_equal(vcount(smoo_p), 4)
  expect_equal(ecount(smoo_p), 7)
  expect_equal(count_components(smoo_p), 1)
  expect_equal(vcount(smoo_d), 653)
  expect_equal(ecount(smoo_d), 828)
  expect_equal(count_components(smoo_d), 1)
  expect_equal(vcount(smoo_u), 635)
  expect_equal(ecount(smoo_u), 810)
  expect_equal(count_components(smoo_u), 1)
})

test_that("to_spatial_simple morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(vcount(simp_l), 8)
  expect_equal(ecount(simp_l), 7)
  expect_equal(count_components(simp_l), 1)
  expect_equal(vcount(simp_p), 4)
  expect_equal(ecount(simp_p), 4)
  expect_equal(count_components(simp_p), 1)
  expect_equal(vcount(simp_d), 653)
  expect_equal(ecount(simp_d), 822)
  expect_equal(count_components(simp_d), 1)
  expect_equal(vcount(simp_u), 635)
  expect_equal(ecount(simp_u), 800)
  expect_equal(count_components(simp_u), 1)
})

test_that("morphers return a valid sfnetwork", {
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(subd_l)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(subd_p)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(subd_d)))
  expect_true(all(sfnetworks:::nodes_in_edge_boundaries(subd_u)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(smoo_l)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(smoo_p)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(smoo_d)))
  expect_true(all(sfnetworks:::nodes_in_edge_boundaries(smoo_u)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(simp_l)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(simp_p)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(simp_d)))
  expect_true(all(sfnetworks:::nodes_in_edge_boundaries(simp_u)))
})
