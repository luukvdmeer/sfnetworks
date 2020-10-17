# This is not true anymore as long as the replacement geometry is equal to the original
# test_that('st_set_geometry gives an error when replacing nodes geometry', {
#   net = roxel %>% as_sfnetwork()
#   new_geom = net %>% st_geometry()
#   expect_error(net %>% sf::st_set_geometry(new_geom))
# })

test_that('st_set_geometry(NULL) for activated nodes changes the class to tbl_graph', {
  net = roxel %>% as_sfnetwork()
  expect_s3_class(net %>% sf::st_set_geometry(NULL), 'tbl_graph')
})

test_that('st_set_geometry gives an error when replacing edges geometry type', {
  net = roxel %>% as_sfnetwork()
  new_geom = st_geometry(sf::st_centroid(roxel))
  expect_error(net %>% activate('edges') %>% sf::st_set_geometry(new_geom))
})

test_that('st_set_geometry gives an error when replacing edges geometry CRS', {
  net = roxel %>% as_sfnetwork()
  new_geom = st_geometry(st_transform(roxel, 3035))
  expect_error(net %>% activate('edges') %>% sf::st_set_geometry(new_geom))
})

# Cannot test because GEOS does not load correctly with my sf library
# test_that('st_set_geometry gives an error when replacing edges geometry endpoints', {
#   net = roxel %>% as_sfnetwork()
#   new_geom = st_geometry(sf::st_reverse(roxel, 5))
#   expect_error(net %>% activate('edges') %>% sf::st_set_geometry(new_geom))
# })
