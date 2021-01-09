library(sf)
library(dplyr)
library(igraph)

# toynet
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

points = st_sfc(st_multipoint(c(p1, p2, p3, p4, p5, p6, p7,
                                p8, p9, p10, p11, p12, p13)))
lines = c(l1, l2, l3, l4, l5, l6, l7)
square = st_sfc(st_cast(st_multipoint(c(p6, p7, p12, p11)), "POLYGON"))

net = as_sfnetwork(lines)

## Edge measures
circuity = net %>%
  activate("edges") %>%
  mutate(circuity = edge_circuity()) %>%
  pull(circuity)

length = net %>%
  activate("edges") %>%
  mutate(length = edge_length()) %>%
  pull(length)

displacement = net %>%
  activate("edges") %>%
  mutate(disp = edge_displacement()) %>%
  pull(disp)

test_that("spatial_edge_measures return correct (known) values", {
  expect_setequal(
    round(circuity, 6),
    c(1.000000, 1.000000, 1.000000, 2.414214, 1.000000, 1.000000, Inf)
  )
  expect_setequal(
    round(length, 6),
    c(2.000000, 2.000000, 2.000000, 2.414214, 2.000000, 1.000000, 3.414214)
  )
  expect_setequal(
    displacement,
    c(2, 2, 2, 1, 2, 1, 0)
  )
})

## spatial predicates
# Edge predicates
edgeint = net %>%
  activate("edges") %>%
  filter(edge_intersects(square))
edgecross = net %>%
  activate("edges") %>%
  filter(edge_crosses(square))
edgecov = net %>%
  activate("edges") %>%
  filter(edge_is_covered_by(square))
edgedisj = net %>%
  activate("edges") %>%
  filter(edge_is_disjoint(square))
edgetouch = net %>%
  activate("edges") %>%
  filter(edge_touches(square))

test_that("spatial edge predicates return correct edges", {
  expect_true(
    all(diag(
      st_geometry(st_as_sf(edgeint, "edges")) %>%
        st_equals(c(l2, l3, l4, l5, l6, l7), sparse = FALSE)
    ))
  )
  expect_true(
    st_geometry(st_as_sf(edgecross, "edges")) %>%
      st_equals(l2, sparse = FALSE)
  )
  expect_true(
    all(diag(
      st_geometry(st_as_sf(edgecov, "edges")) %>%
        st_equals(c(l3, l5), sparse = FALSE)
    ))
  )
  expect_true(
    st_geometry(st_as_sf(edgedisj, "edges")) %>%
      st_equals(l1, sparse = FALSE)
  )
  expect_true(
    all(diag(
      st_geometry(st_as_sf(edgetouch, "edges")) %>%
        st_equals(c(l3, l4, l6, l7), sparse = FALSE)
    ))
  )
})

test_that("spatial edge predicates always return the total number of nodes", {
  expect_equal(vcount(edgeint), vcount(net))
  expect_equal(vcount(edgecross), vcount(net))
  expect_equal(vcount(edgecov), vcount(net))
  expect_equal(vcount(edgedisj), vcount(net))
  expect_equal(vcount(edgetouch), vcount(net))
})

# Node predicates
nodeint = net %>%
  filter(node_intersects(square))
nodewithin = net %>%
  filter(node_is_within(square))
nodecov = net %>%
  filter(node_is_covered_by(square))
nodedisj = net %>%
  filter(node_is_disjoint(square))
nodetouch = net %>%
  filter(node_touches(square))

test_that("spatial node predicates return correct nodes and edges", {
  expect_true(
    all(diag(
      st_geometry(st_as_sf(nodeint, "nodes")) %>%
        st_equals(st_sfc(p5, p6, p7, p9, p10), sparse = FALSE)
    ))
  )
  expect_true(
    all(diag(
      st_geometry(st_as_sf(nodeint, "edges")) %>%
        st_equals(c(l3, l5, l7), sparse = FALSE)
    ))
  )
  expect_true(
    st_geometry(st_as_sf(nodewithin, "nodes")) %>%
      st_equals(p5, sparse = FALSE)
  )
  expect_true(
    all(diag(
      st_geometry(st_as_sf(nodecov, "nodes")) %>%
        st_equals(st_sfc(p5, p6, p7, p9, p10), sparse = FALSE)
    ))
  )
  expect_true(
    all(diag(
      st_geometry(st_as_sf(nodecov, "edges")) %>%
        st_equals(c(l3, l5, l7), sparse = FALSE)
    ))
  )
  expect_true(
    all(diag(
      st_geometry(st_as_sf(nodedisj, "nodes")) %>%
        st_equals(st_sfc(p1, p3, p8), sparse = FALSE)
    ))
  )
  expect_true(
    st_geometry(st_as_sf(nodedisj, "edges")) %>%
      st_equals(l1, sparse = FALSE)
  )
  expect_true(
    all(diag(
      st_geometry(st_as_sf(nodetouch, "nodes")) %>%
        st_equals(st_sfc(p6, p7, p9, p10), sparse = FALSE)
    ))
  )
  expect_true(
    all(diag(
      st_geometry(st_as_sf(nodetouch, "edges")) %>%
        st_equals(c(l3, l5, l7), sparse = FALSE)
    ))
  )
})
