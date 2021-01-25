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

set.seed(124)

# Points in Roxel bbox
A = st_sfc(st_point(c(7.5371, 51.9514)), crs = 4326) %>% st_transform(3035)
B = st_sfc(st_point(c(7.5276, 51.9501)), crs = 4326) %>% st_transform(3035)
rect = st_buffer(A, dist = 300, endCapStyle = "SQUARE")

# Create network from lines
lines = c(l1, l2, l3, l4, l5, l6, l7)
net_l = as_sfnetwork(lines) %>%
  mutate(rdm = sample(1:3, 8, replace = T))
net_i = as_sfnetwork(lines, edges_as_lines = F)

# Create network from points
points = st_sfc(st_multipoint(c(p1, p2, p3, p4, p5, p6, p4, p7, p5, p8,
                               p11, p9, p5, p10, p12, p13, p10)))
net_p = as_sfnetwork(st_cast(points, "POINT")) %>%
  mutate(rdm = sample(1:4, 17, replace = T))

# Create directed Roxel network
net_d = as_sfnetwork(roxel) %>%
  st_transform(3035)

# Create undirected Roxel network
net_u = as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035)

# Perform spatial contraction
cont_l = convert(net_l, to_spatial_contracted, rdm)
cont_p = convert(net_p, to_spatial_contracted, rdm)

# Morph to spatial directed
dire_u = convert(net_u, to_spatial_directed)

# Morph to spatial explicit
expl_i = convert(net_i, to_spatial_explicit)

# Perform spatial transformation
tran_u = convert(net_u, to_spatial_transformed, 4326)
st_tran_u = st_transform(net_u, 4326)

# Extract spatial subset
subn_d = convert(net_d, to_spatial_subset, rect, subset_by = "nodes")
sube_d = convert(net_d, to_spatial_subset, rect, subset_by = "edges")

# Extract spatial neighborhood
neigf_d = convert(net_d, to_spatial_neighborhood, A,
                    from = TRUE, threshold = 500)
neigt_d = convert(net_d, to_spatial_neighborhood, A,
                    from = FALSE, threshold = 500)
neigf_u = convert(net_u, to_spatial_neighborhood, A,
                    from = TRUE, threshold = 500)
neigt_u = convert(net_u, to_spatial_neighborhood, A,
                    from = FALSE, threshold = 500)

# Extract shortest path
shpt_d = convert(net_d, to_spatial_shortest_paths, B, A)
shpt_u = convert(net_u, to_spatial_shortest_paths, B, A)

# Perform network subdivision
## Warnings are suppressed on purpose
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

test_that("to_spatial_directed morphs an undirected sfnetwork into directed
          network", {
  expect_false(is_directed(net_u))
  expect_true(is_directed(dire_u))
})

test_that("to_spatial_transform inside a convert works the same
          as applying st_transform to the network", {
  expect_equal(st_crs(st_tran_u), st_crs(tran_u))
  expect_equal(st_coordinates(st_tran_u), st_coordinates(tran_u))
  expect_equal(
    st_coordinates(activate(st_tran_u, "edges")),
    st_coordinates(activate(tran_u, "edges"))
  )
})

test_that("to_spatial_explicit morphs an sfnetwork with spatially implicit
          a network with spatially explicit edges", {
  expect_false(sfnetworks:::has_spatially_explicit_edges(net_i))
  expect_true(sfnetworks:::has_spatially_explicit_edges(expl_i))
})

test_that("to_spatial_contracted morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(vcount(cont_l), 3)
  expect_equal(ecount(cont_l), 7)
  expect_equal(count_components(cont_l), 1)
  expect_equal(vcount(cont_p), 4)
  expect_equal(ecount(cont_p), 16)
  expect_equal(count_components(cont_p), 1)
})

test_that("to_spatial_subset morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(vcount(subn_d), 151)
  expect_equal(ecount(subn_d), 163)
  expect_equal(count_components(subn_d), 10)
  expect_equal(vcount(sube_d), 701)
  expect_equal(ecount(sube_d), 195)
  expect_equal(count_components(sube_d), 529)
})

test_that("to_spatial_neighborhood morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(vcount(neigf_d), 45)
  expect_equal(ecount(neigf_d), 48)
  expect_equal(count_components(neigf_d), 1)
  expect_equal(vcount(neigt_d), 76)
  expect_equal(ecount(neigt_d), 81)
  expect_equal(count_components(neigt_d), 1)
  expect_equal(vcount(neigf_u), 179)
  expect_equal(ecount(neigf_u), 205)
  expect_equal(count_components(neigf_u), 1)
  expect_equal(vcount(neigt_u), 179)
  expect_equal(ecount(neigt_u), 205)
  expect_equal(count_components(neigt_u), 1)
})

test_that("to_spatial_shortest_paths morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(vcount(shpt_d), 22)
  expect_equal(ecount(shpt_d), 21)
  expect_equal(count_components(shpt_d), 1)
  expect_equal(vcount(shpt_u), 17)
  expect_equal(ecount(shpt_u), 16)
  expect_equal(count_components(shpt_u), 1)
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
  expect_equal(count_components(subd_d), 1)
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
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(dire_u)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(expl_i)))
  expect_true(all(sfnetworks:::nodes_in_edge_boundaries(tran_u)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(cont_l)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(cont_p)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(subn_d)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(sube_d)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(neigf_d)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(neigt_d)))
  expect_true(all(sfnetworks:::nodes_in_edge_boundaries(neigf_u)))
  expect_true(all(sfnetworks:::nodes_in_edge_boundaries(neigt_u)))
  expect_true(all(sfnetworks:::nodes_match_edge_boundaries(shpt_d)))
  expect_true(all(sfnetworks:::nodes_in_edge_boundaries(shpt_u)))
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
