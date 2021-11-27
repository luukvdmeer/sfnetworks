test_that("st_set_geometry(NULL) for activated nodes changes the class to
          tbl_graph", {
  net = roxel %>% as_sfnetwork()
  expect_s3_class(net %>% sf::st_set_geometry(NULL), "tbl_graph")
})

test_that("st_set_geometry gives an error when replacing edges geometry
          type", {
  net = roxel %>% as_sfnetwork()
  # warnings are suppressed since they relate to the sf package
  # warning: st_centroid assumes attributes are constant over geometries of x
  centroids = suppressWarnings(sf::st_centroid(roxel))
  new_geom = st_geometry(centroids)
  expect_error(activate(net, "edges") %>% sf::st_set_geometry(new_geom))
})

test_that("st_set_geometry gives an error when replacing edges geometry CRS", {
  skip_if_not(sf::sf_extSoftVersion()["PROJ"] >= "7.0.0")
  net = roxel %>% as_sfnetwork()
  new_geom = st_geometry(st_transform(roxel, 3035))
  expect_error(activate(net, "edges") %>% sf::st_set_geometry(new_geom))
})

test_that("st_set_geometry gives an error when replacing edges geometry
          endpoints", {
  skip_if_not(sf::sf_extSoftVersion()["GEOS"] >= "3.7.0")
  net = roxel %>% as_sfnetwork()
  new_geom = sf::st_geometry(sf::st_reverse(roxel))
  expect_error(activate(net, "edges") %>% sf::st_set_geometry(new_geom))
})
