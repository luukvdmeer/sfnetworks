if (requireNamespace("spatstat.linnet") && requireNamespace("spatstat.geom")) {
  library(spatstat.linnet)
  library(spatstat.geom)
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


