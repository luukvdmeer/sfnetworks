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
