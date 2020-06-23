pkgload::load_all()

library(sf)
library(tidygraph)

# net = as_sfnetwork(roxel, directed = FALSE)
#
# short_path_1_9 = net %>%
#   activate("edges") %>%
#   dplyr::mutate(weight = edge_length()) %>%
#   tidygraph::convert(to_shortest_path, 1, 9)
#
# plot(roxel$geometry)
# plot(sf::st_as_sf(short_path_1_9)$geometry, lwd = 5, add = TRUE)

sp = function(net, from, to) {
  net %>%
    activate("edges") %>%
    dplyr::mutate(weight = edge_length()) %>%
    tidygraph::convert(to_shortest_path, from, to)
}
sp(net, 1, 9)
bench::press(n = seq(from = 9, to = 99, length.out = 5),
             {
               bench::mark(
                 sp(net, 9, n)
               )
             }
             )

# with stplanr # work in progress, not currently working
# library(stplanr)
# sln = SpatialLinesNetwork(roxel)
# p1 = net %>%
#   activate(nodes) %>%
#   st_as_sf() %>%
#   slice(1)
# p9 = net %>%
#   activate(nodes) %>%
#   st_as_sf() %>%
#   slice(1)
#
# stplanr::route_local(sln = sln, from = c(st_coordinates(p1)), to = c(st_coordinates(p9)))
#
# bench::press(n = seq(from = 9, to = 99, length.out = 5),
#              {
#                bench::mark(
#                  sp(net, 9, n)
#                )
#              }
# )
