pkgload::load_all()
# remotes::install_github("luukvdmeer/sfnetworks@develop")
# library(sfnetworks)

bench::mark(check = FALSE,
            as_sfnetwork(roxel[1:10, ]),
            as_sfnetwork(roxel[1:100, ]),
            as_sfnetwork(roxel[1:200, ]),
            as_sfnetwork(roxel)
            )

# See https://github.com/r-lib/bench/issues/88
# bench::press(n = c(10, 100, 200, nrow(roxel)),
#              {
#                bench::mark(check = FALSE,
#                  as_sfnetwork(roxel[1:n, ]),
#                )
#              }
#              )

# # comparison with stplanr (commented out to reduce dependencies)
# library(sfnetworks)
# system.time({
#   net = as_sfnetwork(roxel)
# })
# system.time({
#   net2 = stplanr::SpatialLinesNetwork(roxel)
# })
# pryr::object_size(net)
# pryr::object_size(net2)
#
# res = bench::press(n = seq(from = 10, to = nrow(roxel), length.out = 5),
#              {
#                bench::mark(
#                  check = FALSE,
#                  time_unit = "ms",
#                  stplanr::SpatialLinesNetwork(roxel[1:n, ]),
#                  sfnetworks::as_sfnetwork(roxel[1:n, ])
#                )
#              }
# )
# ggplot2::autoplot(res)
