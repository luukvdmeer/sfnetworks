## Need to add this line to set roxel CRS again
## to comply with different PROJ versions
if(sf::sf_extSoftVersion()["PROJ"] < "7.0.0"){
  sf::st_crs(roxel) = sf::st_crs('EPSG:4326')
}

library(igraph)
library(sf)
library(dplyr)
library(tidygraph)

test_that("sfnetwork created from Roxel example dataset has
          the expected number of nodes, edges and components
          for a directed network construction", {
  net = as_sfnetwork(roxel)
  expect_equal(vcount(net), 701)
  expect_equal(ecount(net), 851)
  expect_equal(count_components(net), 14)
})

test_that("sfnetwork created with spatially implicit edges has no geometry
          column for the edges", {
  net = as_sfnetwork(roxel, edges_as_lines = F)
  expect_null(sf_attr(net, "sf_column", "edges"))
})

test_that("sfnetwork created from POLYGON-geometry sf gives and error", {
  suppressWarnings({
    rdm  = st_sample(st_as_sfc(st_bbox(roxel)), 4, type = "random")
    rdmpoly = st_buffer(rdm, 0.5)
  })
  expect_error(
    as_sfnetwork(rdmpoly),
    "Geometries are not all of type LINESTRING, or all of type POINT"
  )
})

test_that("Creating an sfnetwork from an sf LINESTRING object with
          from and to columns overwrites them with a warning.", {
  column_from = rep(letters[1:3], 2)
  column_to = c("c", "a", "b", "b", "c", "c")
  expect_warning(
    net <- roxel[25:30, ] %>%
      mutate(from = column_from, to = column_to) %>%
      as_sfnetwork(),
    "Overwriting column"
  )
  expect_false(all(pull(activate(net, "edges"), "from") == column_from))
  expect_false(all(pull(activate(net, "edges"), "to") == column_to))
})

test_that("Creating an sfnetwork from an sf LINESTRING object with
          weight columns, when length_as_weigtht, overwrites it
          with a warning.", {
  set.seed(213)
  column_weight = runif(6)
  expect_warning(
    net <- roxel[25:30, ] %>%
      mutate(weight = column_weight) %>%
      as_sfnetwork(length_as_weight = T),
    "Overwriting column"
  )
  expect_false(all(as.numeric(pull(activate(net, "edges"), "weight"))
                   == column_weight))
})
