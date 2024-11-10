net_exp = as_sfnetwork(roxel)
net_imp = as_sfnetwork(roxel) |> make_edges_implicit()

test_that("plot accepts sfnetworks with spatially implicit edges", {
  pdf(NULL)
  expect_silent(plot(net_imp, draw_lines = TRUE))
  expect_silent(plot(net_imp, draw_lines = FALSE))
})
test_that("autplot returns a ggplot with two layers", {
  skip_if_not_installed("ggplot2")
  g = ggplot2::autoplot(net_exp)
  expect_s3_class(g, "gg")
  expect_s3_class(g, "ggplot")
  expect_equal(length(g$layers), 2)
})

