library(sf)
library(dplyr)
library(igraph)
library(tidygraph)

# Call some data for testing
net = as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035)
net_dir = as_sfnetwork(roxel, directed = TRUE) %>%
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

# Calculate some cost matrices and set non outputs
sub1_c1 = c(0, 2, 3, 1, 2, 2, 0, 1, 1, 2, 3, 1, 0, 2, 3, 1, 1, 2, 0, 1,
            2, 2, 3, 1, 0)
sub2_c1 = c(0, 1, Inf, Inf, Inf, 1, 0, Inf, Inf, Inf, Inf, Inf, 0, 1, Inf,
            Inf, Inf, 1, 0, Inf, Inf, Inf, Inf, Inf, 0)
sub2_c2 = c(0, 1, NaN, NaN, NaN, 1, 0, NaN, NaN, NaN, NaN, NaN, 0, 1, NaN,
            NaN, NaN, 1, 0, NaN, NaN, NaN, NaN, NaN, 0)

cost1.1 = st_network_cost(sub1, weights = NA)
cost2.1 = st_network_cost(sub2, weights = NA)
cost2.2 = st_network_cost(sub2, weights = NA, Inf_as_NaN = T)

# Tests for st_network_paths()
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

# Tests for st_networks_cost()
test_that("st_network_cost outputs matrix with known values", {
  expect_setequal(as.vector(cost1.1), sub1_c1)
  expect_setequal(as.vector(cost2.1), sub2_c1)
  expect_setequal(as.vector(cost2.2), sub2_c2)
  expect_equal(sum(cost1.1, na.rm = T), 36)
  expect_equal(sum(cost2.1, na.rm = T), Inf)
  expect_equal(sum(cost2.2, na.rm = T), 4)
})

test_that("st_network_cost calculates distance matrix
          including duplicated 'to' nodes", {
  from_idx = c(1,2)
  to_idx = c(3,2,3)
  # Test with duplicated to nodes and multiple from nodes
  costmat = st_network_cost(net, from_idx, to_idx)
  expect_equal(nrow(costmat), length(from_idx))
  expect_equal(ncol(costmat), length(to_idx))
  # Test with unique to nodes and single from node
  costmat = st_network_cost(net, from_idx[1], to_idx[1:2])
  expect_equal(nrow(costmat), length(from_idx[1]))
  expect_equal(ncol(costmat), length(to_idx[1:2]))
  # Test with duplicated sf to points that have same nearest node
  p1 = st_geometry(net, "nodes")[1]
  p2 = st_geometry(net, "nodes")[9]
  p3 = st_sfc(p1[[1]] + st_point(c(500, 500)), crs = st_crs(p1))
  p4 = st_sfc(p1[[1]] + st_point(c(500.2, 500.2)), crs = st_crs(p1))
  p5 = st_sfc(p2[[1]] + st_point(c(-500, -500)), crs = st_crs(p2))
  pts1 = c(p1, p5)
  pts2 = c(p2, p3, p4)
  costmatsf = st_network_cost(net, pts1, pts2)
  expect_equal(ncol(costmatsf), length(pts2))
  expect_equal(nrow(costmatsf), length(pts1))
})

test_that("st_network_cost passes the direction argument to the
          mode argument in igraph::distances for directed networks
          and ignores it for undirected", {
  dist_out = st_network_cost(net_dir, from = 1, to = 10, direction = "out")
  dist_out2 = st_network_cost(net_dir, from = 1, to = 10)
  dist_in = st_network_cost(net_dir, from = 1, to = 10, direction = "in")
  dist_all = st_network_cost(net_dir, from = 1, to = 10, direction = "all")
  dist_all_undir1 = st_network_cost(net, from = 1, to = 10)
  dist_all_undir2 = st_network_cost(net, from = 1, to = 10, direction = "out")
  dist_all_undir3 = st_network_cost(net, from = 1, to = 10, direction = "in")
  dist_all_undir4 = st_network_cost(net, from = 1, to = 10, direction = "all")
  expect_false(isTRUE(all.equal(dist_out, dist_in)))
  expect_false(isTRUE(all.equal(dist_out, dist_all)))
  expect_false(isTRUE(all.equal(dist_in, dist_all)))
  expect_equal(dist_out, dist_out2)
  expect_equal(dist_all, dist_all_undir1)
  expect_equal(dist_all, dist_all_undir2)
  expect_equal(dist_all, dist_all_undir3)
  expect_equal(dist_all, dist_all_undir4)
})

test_that("st_network_cost handles Inf_as_Nan correctly", {
  costmat1 = st_network_cost(net, Inf_as_NaN = FALSE)
  costmat2 = st_network_cost(net, Inf_as_NaN = TRUE)
  costmat3 = st_network_cost(net, 1, c(377,378,377), Inf_as_NaN = FALSE)
  costmat4 = st_network_cost(net, 1, c(377,378,377), Inf_as_NaN = TRUE)
  # Test that matrix contains Inf values instead of NaN
  expect_gt(length(costmat1[is.infinite(costmat1)]), 0)
  expect_equal(length(costmat1[is.nan(costmat1)]), 0)
  expect_gt(length(costmat3[is.infinite(costmat3)]), 0)
  expect_equal(length(costmat3[is.nan(costmat3)]), 0)
  # Test that matrix contains NaN values instead of Inf
  expect_gt(length(costmat2[is.nan(costmat2)]), 0)
  expect_equal(length(costmat2[is.infinite(costmat2)]), 0)
  expect_gt(length(costmat4[is.nan(costmat4)]), 0)
  expect_equal(length(costmat4[is.infinite(costmat4)]), 0)
  # Test that all Inf values are NaN values
  expect_equal(
    length(costmat1[is.infinite(costmat1)]),
    length(costmat2[is.nan(costmat2)])
  )
  expect_equal(
    length(costmat3[is.infinite(costmat3)]),
    length(costmat4[is.nan(costmat4)])
  )
})

test_that("... ignores mode argument with a warning", {
  expect_warning(
    st_network_cost(net, from = 1, to = 10, mode = "in"),
    "Argument 'mode' is ignored"
  )
  expect_warning(
    st_network_cost(net_dir, from = 1, to = 10, mode = "out"),
    "Argument 'mode' is ignored"
  )
})

test_that("... is passed correcly onto igraph::distances", {
  expect_silent(cost_dijkstra <- st_network_cost(net_dir, from = 1, to = 10,
                                  direction = "in", algorithm = "dijkstra"))
  expect_silent(cost_johnson <- st_network_cost(net_dir, from = 1, to = 10,
                                  direction = "in", algorithm = "johnson"))
  expect_false(isTRUE(all.equal(cost_dijkstra, cost_johnson)))
})

