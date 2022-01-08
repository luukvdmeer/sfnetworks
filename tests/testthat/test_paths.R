library(sf)
library(dplyr)
library(igraph)
library(tidygraph)
net = as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035)
sub1 = net %>%
  convert(to_spatial_neighborhood, 15, 150) %>%
  activate("edges") %>%
  st_set_geometry(NULL)

sub2 = net %>%
  slice(1:5)

# Create random points inside network bbox
rdm = net %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_sample(4, type = "random")

test_that("Only the first from argument
          is used for shortest paths calculations", {
  from_indices = c(98, 23, 12)
  expect_warning(paths <- st_network_paths(
    net,
    from = from_indices,
    to = rdm,
    type = "all_shortest"), "only the first element is used")
  resulting_from_nodes = paths %>%
    rowwise() %>%
    mutate(node_from = first(node_paths)) %>%
    pull(node_from)
  expect_setequal(resulting_from_nodes, first(from_indices))
})

test_that("NA indices for in from and/or to arguments give an error", {
  expect_error(st_network_paths(
    net,
    from = as.numeric(NA),
    to = c(3, 28, 98)
  ), "NA values present")
  expect_error(st_network_cost(
    net,
    from = 2,
    to = as.numeric(c(NA, 3, NA))
  ), "NA values present")
  expect_error(st_network_paths(
    net,
    from = rdm[1],
    to = c(rdm, st_sfc(st_point()))
  ), "NA values present")
})

test_that("Duplicated to nodes are removed from st_network_cost calculations
          with a warning", {
    expect_warning(
      costmat <- st_network_cost(net, 1, c(3, 2, 3)),
      "Duplicated values in argument 'to' were removed"
    )
    expect_equal(ncol(costmat), 2)
    # Test with duplicated sf to points that have same nearest node
    p1 = st_geometry(net, "nodes")[1]
    p2 = st_geometry(net, "nodes")[9]
    p3 = st_sfc(p1[[1]] + st_point(c(500, 500)), crs = st_crs(p1))
    p4 = st_sfc(p1[[1]] + st_point(c(500.2, 500.2)), crs = st_crs(p1))
    p5 = st_sfc(p2[[1]] + st_point(c(-500, -500)), crs = st_crs(p2))
    pts1 = c(p1, p5)
    pts2 = c(p2, p3, p4)
    expect_warning(
      costmatsf <- st_network_cost(net, pts1, pts2),
      "Duplicated values in argument 'to' were removed"
    )
    expect_equal(ncol(costmatsf), 2)
})

test_that("st_network_paths weights argument is passed implicitly,
          explicitly and automatically", {
  # Set weights to a named column
  expect_silent(
    nodepaths_exp <- net %>%
      activate("edges") %>%
      mutate(length = edge_length()) %>%
      st_network_paths(8, 3, weights = "length") %>%
      pull(node_paths) %>%
      unlist()
  )
  # Set weights to a column called weight
  expect_silent(
    nodepaths_imp <- net %>%
      activate("edges") %>%
      mutate(weight = edge_length()) %>%
      st_network_paths(8, 3) %>%
      pull(node_paths) %>%
      unlist()
  )
  # Do not set weight but expect it is computed internally
  expect_silent(
    nodepaths_aut <- net %>%
      st_network_paths(8, 3) %>%
      pull(node_paths) %>%
      unlist()
  )
  expect_setequal(
    nodepaths_exp, nodepaths_imp
  )
  expect_setequal(
    nodepaths_exp, nodepaths_aut
  )
  expect_setequal(
    nodepaths_aut, nodepaths_imp
  )
})

test_that("Unexisting 'weights' column passed to st_network_paths
          gives an error", {
  expect_error(
    st_network_paths(net, 1, 12, weights = "invented_column"),
    "not found"
  )
})

test_that("node_paths without set weight is equal or shorter than
          node_paths with set weight", {
  nodepaths_weight <- net %>%
    activate("edges") %>%
    mutate(weight = edge_length()) %>%
    st_network_paths(8, 3) %>%
    pull(node_paths) %>%
    unlist()

  nodepaths_noweight <- net %>%
    st_network_paths(8, 3, weights = NA) %>%
    pull(node_paths) %>%
    unlist()

  expect_true(length(nodepaths_noweight) <= length(nodepaths_weight))
})

test_that("All simple paths wrapper gives a known number of paths", {
  expect_equal(
    net %>%
      convert(to_spatial_directed) %>%
      st_network_paths(1, 12, type = "all_simple") %>%
      nrow(),
    6
  )
})

p1 = st_point(c(7, 51))
p2 = st_point(c(7, 52))
p3 = st_point(c(8, 52))
p4 = st_point(c(8, 51.5))

l1 = st_sfc(st_linestring(c(p1, p2)))
l2 = st_sfc(st_linestring(c(p1, p4, p3)))
l3 = st_sfc(st_linestring(c(p3, p2)))

edges = st_as_sf(c(l1, l2, l3), crs = 4326)
nodes = st_as_sf(c(st_sfc(p1), st_sfc(p2), st_sfc(p3)), crs = 4326)

edges$from = c(1, 1, 3)
edges$to = c(2, 3, 2)

net_unnamed = sfnetwork(nodes, edges)

paths_unnamed = st_network_paths(net_unnamed, 1, c(2,3))

nodes$name = c("city", "village", "farm")
edges$from = c("city", "city", "farm")
edges$to = c("village", "farm", "village")

net_named = sfnetwork(nodes, edges)

paths_named = st_network_paths(net_named, "city", c("farm","village"))

test_that("st_network_paths returns a named vector or not
          when node_key is set or not", {
  expect_named(paths_named$node_paths[[1]], c("city", "farm"))
  expect_named(paths_named$node_paths[[2]], c("city", "village"))
  expect_named(paths_named$edge_paths[[1]], c("city|farm"))
  expect_named(paths_named$edge_paths[[2]], c("city|village"))
  expect_named(paths_unnamed$node_paths[[1]], NULL)
  expect_named(paths_unnamed$node_paths[[2]], NULL)
  expect_named(paths_unnamed$edge_paths[[1]], NULL)
  expect_named(paths_unnamed$edge_paths[[2]], NULL)
})


sub1_c1 = c(0, 2, 3, 1, 2, 2, 0, 1, 1, 2, 3, 1, 0, 2, 3, 1, 1, 2, 0, 1,
           2, 2, 3, 1, 0)
sub2_c1 = c(0, 1, Inf, Inf, Inf, 1, 0, Inf, Inf, Inf, Inf, Inf, 0, 1, Inf,
           Inf, Inf, 1, 0, Inf, Inf, Inf, Inf, Inf, 0)
sub2_c2 = c(0, 1, NaN, NaN, NaN, 1, 0, NaN, NaN, NaN, NaN, NaN, 0, 1, NaN,
            NaN, NaN, 1, 0, NaN, NaN, NaN, NaN, NaN, 0)

cost1.1 = st_network_cost(sub1, weights = NA)
cost2.1 = st_network_cost(sub2, weights = NA)
cost2.2 = st_network_cost(sub2, weights = NA, Inf_as_NaN = T)

test_that("st_network_cost outputs matrix with known values", {
  expect_setequal(as.vector(cost1.1), sub1_c1)
  expect_setequal(as.vector(cost2.1), sub2_c1)
  expect_setequal(as.vector(cost2.2), sub2_c2)
  expect_equal(sum(cost1.1, na.rm = T), 36)
  expect_equal(sum(cost2.1, na.rm = T), Inf)
  expect_equal(sum(cost2.2, na.rm = T), 4)
})
