## Need to add this line to set roxel CRS again
## to comply with different PROJ versions
if(sf::sf_extSoftVersion()["PROJ"] < "7.0.0"){
  sf::st_crs(roxel) = sf::st_crs('EPSG:4326')
}

library(sf)
test_that("st_set_crs sets the crs for edges and nodes", {
  net = as_sfnetwork(roxel) %>%
    st_set_crs(NA)
  expect_equal(st_crs(activate(net, "nodes")), st_crs(activate(net, "edges")))
})

test_that("st_transform changes crs for edges and nodes", {
  net = as_sfnetwork(roxel) %>%
    st_transform(3857)
  expect_equal(st_crs(activate(net, "nodes")), st_crs(activate(net, "edges")))
})
