library(tidygraph)
test_that("Morphing works for sfnetwork and tbl_graph objects", {
  expect_s3_class(
    roxel |>
      as_sfnetwork() |>
      morph(to_components),
    "morphed_sfnetwork"
  )
  expect_s3_class(
    roxel |>
      as_sfnetwork() |>
      as_tbl_graph() |>
      morph(to_components),
    "morphed_tbl_graph"
  )
  expect_error(
    roxel |>
      morph(to_components),
    "no applicable method for 'morph' applied to"
  )
})
