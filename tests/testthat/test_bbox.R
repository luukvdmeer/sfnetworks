## Need to add this line to set roxel CRS again
## to comply with different PROJ versions
if(sf::sf_extSoftVersion()["PROJ"] < "7.0.0"){
  sf::st_crs(roxel) = sf::st_crs('EPSG:4326')
}

library(sf)
test_that("st_network_bbox combines the bounding box of nodes and edges", {
  rdm_rox = roxel[sample(nrow(roxel), 50), ]
  net = as_sfnetwork(rdm_rox)
  expect_true(
    all(names(st_network_bbox(net)) == c("xmin", "ymin", "xmax", "ymax"))
  )
  expect_true(
    st_network_bbox(net)["xmin"] <= st_bbox(activate(net, "nodes"))["xmin"]
  )
  expect_true(
    st_network_bbox(net)["xmin"] <= st_bbox(activate(net, "edges"))["xmin"]
  )
  expect_true(
    st_network_bbox(net)["ymin"] <= st_bbox(activate(net, "nodes"))["ymin"]
  )
  expect_true(
    st_network_bbox(net)["ymin"] <= st_bbox(activate(net, "edges"))["ymin"]
  )
  expect_true(
    st_network_bbox(net)["xmax"] >= st_bbox(activate(net, "nodes"))["xmax"]
  )
  expect_true(
    st_network_bbox(net)["xmax"] >= st_bbox(activate(net, "edges"))["xmax"]
  )
  expect_true(
    st_network_bbox(net)["ymax"] >= st_bbox(activate(net, "nodes"))["ymax"]
  )
  expect_true(
    st_network_bbox(net)["ymax"] >= st_bbox(activate(net, "edges"))["ymax"]
  )
})
