if (requireNamespace("spatstat") && packageVersion("spatstat") >= "2.0.1") {
  library(spatstat)
  ## Need to add this line to set Roxel CRS again
  ## to comply with different PROJ versions
  st_crs(roxel) = "EPSG:4326"
  test_that("Converting sfnetwork to linnet works with projected coords", {
    roxel_sfn <- as_sfnetwork(roxel) %>% st_transform(3857)
    # I added suppressWarnings since as.linnet returns a few warning messages
    # related to the structure of the network (network not connected and
    # duplicated segments) which are irrelevant here
    suppressWarnings(roxel_linnet <- as.linnet(roxel_sfn))
    # I need expect_lte since the linnet methods removes the duplicated edges
    expect_lte(nsegments(roxel_linnet), nrow(roxel))
  })
  test_that("Converting sfnetwork to linnet fails with latlong coords", {
    roxel_sfn <- as_sfnetwork(roxel)
    expect_error(as.linnet(roxel_sfn))
  })
}

