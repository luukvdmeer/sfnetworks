library(sf)

test_that("st_set_crs sets the crs for edges and nodes", {
  skip_if_not(sf::sf_extSoftVersion()["PROJ"] >= "7.0.0")
  net = as_sfnetwork(roxel) %>%
    st_set_crs(NA)
  expect_equal(st_crs(activate(net, "nodes")), st_crs(activate(net, "edges")))
})

test_that("st_transform changes crs for edges and nodes", {
  skip_if_not(sf::sf_extSoftVersion()["PROJ"] >= "7.0.0")
  net = as_sfnetwork(roxel) %>%
    st_transform(3857)
  expect_equal(st_crs(activate(net, "nodes")), st_crs(activate(net, "edges")))
})
