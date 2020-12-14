library(igraph)
library(sf)

test_that('sfnetwork created from Roxel example dataset has
          the expected number of nodes, edges and components
          for a directed network construction', {
  net = as_sfnetwork(roxel)
  expect_equal(vcount(net), 701)
  expect_equal(ecount(net), 851)
  expect_equal(count_components(net), 14)
})

test_that('sfnetwork created with spatially implicit edges has no geometry
          column for the edges', {
  net = as_sfnetwork(roxel, edges_as_lines = F)
  expect_null(sf_attr(net, 'sf_column', 'edges'))
})

test_that('sfnetwork created from POLYGON-geometry sf gives and error', {
  suppressWarnings({
    rdm  = st_sample(st_as_sfc(st_bbox(roxel)), 4, type = 'random')
    rdmpoly = st_buffer(rdm, 0.5)
  })
  expect_error(
    as_sfnetwork(rdmpoly),
    'Geometries are not all of type LINESTRING, or all of type POINT'
  )
})
