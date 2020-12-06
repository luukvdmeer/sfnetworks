library(sf)
rect = roxel %>%
  st_union() %>%
  st_transform(3857) %>%
  st_centroid() %>%
  st_buffer(dist = 500, endCapStyle = 'SQUARE') %>%
  st_transform(4326) %>%
  st_as_sf(foo = 'bar')

test_that('sf functions for sfnetworks with spatially implicit edges,
         give an error', {
  net = as_sfnetwork(roxel, edges_as_lines = F) %>% activate('edges')
  # Geometries
  expect_error(st_coordinates(net), 'Edges are spatially implicit')
  expect_error(st_is(net, 'LINESTRING'), 'Edges are spatially implicit')
  # Coordinates
  expect_error(st_shift_longitude(net), 'Edges are spatially implicit')
  expect_error(st_wrap_dateline(net), 'Edges are spatially implicit')
  expect_error(st_transform(net, 3857), 'Edges are spatially implicit')
  expect_error(st_zm(net), 'Edges are spatially implicit')
  expect_error(st_m_range(net), 'Edges are spatially implicit')
  expect_error(st_z_range(net), 'Edges are spatially implicit')
  # Geometry binary predicate
  expect_error(st_intersects(net, rect), 'Edges are spatially implicit')
  # Geometry unary operations
  expect_error(st_simplify(net), 'Edges are spatially implicit')
  # Join and filter
  expect_error(st_crop(net, rect), 'Edges are spatially implicit')
  expect_error(st_filter(net, rect), 'Edges are spatially implicit')
})
