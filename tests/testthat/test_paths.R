library(sf)
library(dplyr)
library(igraph)
library(tidygraph)
net = as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035)

# Create random points inside network bbox
rdm = net %>% st_bbox() %>% st_as_sfc() %>% st_sample(4, type = 'random')

test_that('Only the first from argument
          is used for shortest paths calculations', {
  from_indices = c(98, 23, 12)
  expect_warning(paths <- st_network_paths(
    net,
    from = from_indices,
    to = rdm,
    all = TRUE), 'only the first element is used')
  resulting_from_nodes = paths %>%
    rowwise() %>%
    mutate(node_from = first(node_paths)) %>%
    pull(node_from)
  expect_setequal(resulting_from_nodes, first(from_indices))
})

test_that('Empty geometries for all from and/or to arguments
          give an error', {
  expect_error(st_network_cost(
      net,
      from = st_sfc(st_point(), crs = st_crs(net)),
      to = rdm[1:2]
  ), 'are all empty')
  expect_error(st_network_paths(
    net,
    from = rdm[1],
    to = st_sfc(st_point(), crs = st_crs(net)),
    all = TRUE
  ), 'are all empty')
})

test_that('Empty geometries for some from and/or to arguments are
          ignored with a warning', {
  expect_warning(st_network_paths(
    net,
    from = c(st_sfc(st_point(), crs = st_crs(net)), rdm[3]),
    to = rdm[1:2]
  ), 'are ignored')
  expect_warning(st_network_cost(
    net,
    from = rdm[1],
    to = c(rdm[3:4], st_sfc(st_point()))
  ), 'are ignored')
})

test_that('NA indices for all from and/or to arguments give an error', {
  expect_error(st_network_paths(
    net,
    from = as.numeric(NA),
    to = c(3,28,98)
  ), 'are all NA')
  expect_error(st_network_paths(
    net,
    from = 2,
    to = as.numeric(c(NA, NA, NA))
  ), 'are all NA')
})

test_that('NA indices for some from and/or to arguments are ignored with
          a warning', {
  expect_warning(st_network_cost(
    net,
    from = as.numeric(c(27,NA)),
    to = c(3,28,98)
  ), 'are ignored')
  expect_warning(st_network_paths(
    net,
    from = 2,
    to = as.numeric(c(NA, 32, 29, NA))
  ), 'are ignored')
})

test_that('Objects in the to and/or from argument that are not numeric, sf,
          or sfc give an error', {
    expect_error(st_network_cost(
      net,
      from = 4,
      to = c(TRUE, FALSE, TRUE)
    ), 'not accepted')
})

test_that('Duplicated to nodes are removed from st_network_cost calculations
          with a warning', {
    expect_warning(
      costmat <- st_network_cost(net, 1, c(3, 2, 3)),
      'node indices were removed'
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
      'node indices were removed'
    )
    expect_equal(ncol(costmatsf), 2)
})

test_that('st_network_paths weights argument is passed implicitly
          and explicitly',{
  expect_silent(
    nodepaths_exp <- net %>%
      activate('edges') %>%
      mutate(length = edge_length()) %>%
      st_network_paths(8, 3, weights = 'length') %>%
      pull(node_paths) %>% unlist()
  )
  expect_silent(
    nodepaths_imp <- net %>%
      activate('edges') %>%
      mutate(weight = edge_length()) %>%
      st_network_paths(8, 3) %>%
      pull(node_paths) %>% unlist()
  )
  expect_setequal(
    nodepaths_exp, nodepaths_imp
  )
})

test_that('edge_path without set weight is equal or shorter than
          edge_path with set weight', {
  edgepaths_weight <- net %>%
      activate('edges') %>%
      mutate(weight = edge_length()) %>%
      st_network_paths(8, 3) %>%
      pull(node_paths) %>% unlist()

  edgepaths_noweight <- net %>%
      st_network_paths(8, 3) %>%
      pull(node_paths) %>% unlist()

  expect_true(length(edgepaths_noweight) <= length(edgepaths_weight))
})
