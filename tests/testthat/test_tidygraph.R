## Need to add this line to set roxel CRS again
## to comply with different PROJ versions
if(sf::sf_extSoftVersion()["PROJ"] < "7.0.0"){
  sf::st_crs(roxel) = sf::st_crs('EPSG:4326')
}

library(tidygraph)
test_that("Morphing works for sfnetwork and tbl_graph objects", {
  expect_s3_class(
    roxel %>%
      as_sfnetwork() %>%
      morph(to_components),
    "morphed_sfnetwork"
  )
  expect_s3_class(
    roxel %>%
      as_sfnetwork() %>%
      as_tbl_graph() %>%
      morph(to_components),
    "morphed_tbl_graph"
  )
  expect_error(
    roxel %>%
      morph(to_components),
    "no applicable method for 'morph' applied to"
  )
})
