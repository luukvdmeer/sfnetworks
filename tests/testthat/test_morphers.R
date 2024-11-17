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
A = st_sfc(st_point(c(7.5371, 51.9514)), crs = 4326) |> st_transform(3035)
B = st_sfc(st_point(c(7.5276, 51.9501)), crs = 4326) |> st_transform(3035)
rect = st_buffer(A, dist = 300, endCapStyle = "SQUARE")

# Create network from lines
lines = c(l1, l2, l3, l4, l5, l6, l7)
net_l = as_sfnetwork(lines) |>
  mutate(rdm = sample(1:3, 8, replace = T))
net_i = as_sfnetwork(lines) |> make_edges_implicit()

# Create directed Roxel network
net_d = as_sfnetwork(roxel) |>
  st_transform(3035)

# Create undirected Roxel network
net_u = as_sfnetwork(roxel, directed = FALSE) |>
  st_transform(3035)

# Perform spatial contraction
cont_l = convert(net_l, to_spatial_contracted, rdm)

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
                    threshold = 500)
neigt_d = convert(net_d, to_spatial_neighborhood, A,
                    threshold = 500, direction = "in")
neigf_u = convert(net_u, to_spatial_neighborhood, A,
                    threshold = 500)
neigt_u = convert(net_u, to_spatial_neighborhood, A,
                    threshold = 500, direction = "in")

# Extract shortest path
shpt_d = convert(net_d, to_spatial_shortest_paths, B, A)
shpt_u = convert(net_u, to_spatial_shortest_paths, B, A)

# Perform network subdivision
subd_l = convert(net_l, to_spatial_subdivision)
subd_d = convert(net_d, to_spatial_subdivision)
subd_u = convert(net_u, to_spatial_subdivision)

# Perform spatial smoothing of pseudo nodes
smoo_l = convert(net_l, to_spatial_smooth)
smoo_d = convert(net_d, to_spatial_smooth)
smoo_u = convert(net_u, to_spatial_smooth)

# Perform network simplification
simp_l = convert(net_l, to_spatial_simple)
simp_d = convert(net_d, to_spatial_simple)
simp_u = convert(net_u, to_spatial_simple)

test_that("the created toy network from lines for morpher testing has
          the expected number of nodes, edges and components", {
  expect_equal(n_nodes(net_l), 8)
  expect_equal(n_edges(net_l), 7)
  expect_equal(count_components(net_l), 3)
  expect_equal(n_nodes(net_d), 987)
  expect_equal(n_edges(net_d), 1215)
  expect_equal(count_components(net_d), 9)
  expect_equal(n_nodes(net_u), 987)
  expect_equal(n_edges(net_u), 1215)
  expect_equal(count_components(net_u), 9)
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
  expect_false(sfnetworks:::has_explicit_edges(net_i))
  expect_true(sfnetworks:::has_explicit_edges(expl_i))
})

test_that("to_spatial_contracted morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(n_nodes(cont_l), 3)
  expect_equal(n_edges(cont_l), 5)
  expect_equal(count_components(cont_l), 1)
})

test_that("to_spatial_subset morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(n_nodes(subn_d), 168)
  expect_equal(n_edges(subn_d), 183)
  expect_equal(count_components(subn_d), 4)
  expect_equal(n_nodes(sube_d), 987)
  expect_equal(n_edges(sube_d), 215)
  expect_equal(count_components(sube_d), 792)
})

test_that("to_spatial_neighborhood morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(n_nodes(neigf_d), 56)
  expect_equal(n_edges(neigf_d), 61)
  expect_equal(count_components(neigf_d), 1)
  expect_equal(n_nodes(neigt_d), 95)
  expect_equal(n_edges(neigt_d), 101)
  expect_equal(count_components(neigt_d), 1)
  expect_equal(n_nodes(neigf_u), 238)
  expect_equal(n_edges(neigf_u), 271)
  expect_equal(count_components(neigf_u), 1)
  expect_equal(n_nodes(neigt_u), 238)
  expect_equal(n_edges(neigt_u), 271)
  expect_equal(count_components(neigt_u), 1)
})

test_that("to_spatial_shortest_paths morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(n_nodes(shpt_d), 25)
  expect_equal(n_edges(shpt_d), 24)
  expect_equal(count_components(shpt_d), 1)
  expect_equal(n_nodes(shpt_u), 28)
  expect_equal(n_edges(shpt_u), 27)
  expect_equal(count_components(shpt_u), 1)
})

test_that("to_spatial_subdivision morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(n_nodes(subd_l), 9)
  expect_equal(n_edges(subd_l), 10)
  expect_equal(count_components(subd_l), 1)
  expect_equal(n_nodes(subd_d), 987)
  expect_equal(n_edges(subd_d), 1215)
  expect_equal(count_components(subd_d), 9)
  expect_equal(n_nodes(subd_u), 987)
  expect_equal(n_edges(subd_u), 1215)
  expect_equal(count_components(subd_u), 9)
})

test_that("to_spatial_smooth morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(n_nodes(smoo_l), 7)
  expect_equal(n_edges(smoo_l), 6)
  expect_equal(count_components(smoo_l), 3)
  expect_equal(n_nodes(smoo_d), 987)
  expect_equal(n_edges(smoo_d), 1215)
  expect_equal(count_components(smoo_d), 9)
  expect_equal(n_nodes(smoo_u), 965)
  expect_equal(n_edges(smoo_u), 1193)
  expect_equal(count_components(smoo_u), 9)
})

test_that("to_spatial_simple morphs the sfnetwork into a new network
          with the expected number of nodes, edges and components", {
  expect_equal(n_nodes(simp_l), 8)
  expect_equal(n_edges(simp_l), 5)
  expect_equal(count_components(simp_l), 3)
  expect_equal(n_nodes(simp_d), 987)
  expect_equal(n_edges(simp_d), 1211)
  expect_equal(count_components(simp_d), 9)
  expect_equal(n_nodes(simp_u), 987)
  expect_equal(n_edges(simp_u), 1207)
  expect_equal(count_components(simp_u), 9)
})

test_that("morphers return same network when there is no morphing
          necessary", {
  expect_equal(n_nodes(smoo_d), n_nodes(convert(smoo_d, to_spatial_smooth)))
  expect_equal(n_edges(smoo_d), n_edges(convert(smoo_d, to_spatial_smooth)))
  expect_equal(n_nodes(smoo_l), n_nodes(convert(smoo_l, to_spatial_smooth)))
  expect_equal(n_edges(smoo_l), n_edges(convert(smoo_l, to_spatial_smooth)))
  expect_equal(n_nodes(smoo_u), n_nodes(convert(smoo_u, to_spatial_smooth)))
  expect_equal(n_edges(smoo_u), n_edges(convert(smoo_u, to_spatial_smooth)))
  expect_equal(n_nodes(simp_d), n_nodes(convert(simp_d, to_spatial_simple)))
  expect_equal(n_edges(simp_d), n_edges(convert(simp_d, to_spatial_simple)))
  expect_equal(n_nodes(simp_l), n_nodes(convert(simp_l, to_spatial_simple)))
  expect_equal(n_edges(simp_l), n_edges(convert(simp_l, to_spatial_simple)))
  expect_equal(n_nodes(simp_u), n_nodes(convert(simp_u, to_spatial_simple)))
  expect_equal(n_edges(simp_u), n_edges(convert(simp_u, to_spatial_simple)))
  expect_equal(
    n_nodes(subd_d),
    suppressWarnings(n_nodes(convert(subd_d, to_spatial_subdivision)))
  )
  expect_equal(
    n_edges(subd_d),
    suppressWarnings(n_edges(convert(subd_d, to_spatial_subdivision)))
  )
  expect_equal(
    n_nodes(subd_l),
    suppressWarnings(n_nodes(convert(subd_l, to_spatial_subdivision)))
  )
  expect_equal(
    n_edges(subd_l),
    suppressWarnings(n_edges(convert(subd_l, to_spatial_subdivision)))
  )
  expect_equal(
    n_nodes(subd_u),
    suppressWarnings(n_nodes(convert(subd_u, to_spatial_subdivision)))
  )
  expect_equal(
    n_edges(subd_u),
    suppressWarnings(n_edges(convert(subd_u, to_spatial_subdivision)))
  )
  expect_equal(
    is_directed(net_d),
    is_directed(convert(net_d, to_spatial_directed))
  )
  expect_setequal(
    st_geometry(activate(net_l, "edges")),
    st_geometry(activate(convert(net_l, to_spatial_explicit), "edges"))
  )
})

mess = "Spatial network structure is valid"
test_that("morphers return a valid sfnetwork", {
  expect_message(validate_network(dire_u), mess)
  expect_message(validate_network(expl_i), mess)
  expect_message(validate_network(tran_u), mess)
  expect_message(validate_network(cont_l), mess)
  expect_message(validate_network(subn_d), mess)
  expect_message(validate_network(sube_d), mess)
  expect_message(validate_network(neigf_d), mess)
  expect_message(validate_network(neigt_d), mess)
  expect_message(validate_network(neigf_u), mess)
  expect_message(validate_network(neigt_u), mess)
  expect_message(validate_network(shpt_d), mess)
  expect_message(validate_network(shpt_u), mess)
  expect_message(validate_network(subd_l), mess)
  expect_message(validate_network(subd_d), mess)
  expect_message(validate_network(subd_u), mess)
  expect_message(validate_network(smoo_l), mess)
  expect_message(validate_network(smoo_d), mess)
  expect_message(validate_network(smoo_u), mess)
  expect_message(validate_network(simp_l), mess)
  expect_message(validate_network(simp_d), mess)
  expect_message(validate_network(simp_u), mess)
})
