library(sf)
# Create toy network
n11 = st_point(c(0,0))
n12 = st_point(c(1,1))
e1 = st_sfc(st_linestring(c(n11, n12)), crs = 4326)
n21 = n12
n22 = st_point(c(0,2))
e2 = st_sfc(st_linestring(c(n21, n22)), crs = 4326)
n31 = n22
n32 = st_point(c(-1,1))
e3 = st_sfc(st_linestring(c(n31, n32)), crs = 4326)
net = as_sfnetwork(st_as_sf(c(e1,e2,e3)))

# Extract nodes from network as points
pts = st_as_sf(net, 'nodes')[1:2,]

# Create random points inside network bbox
rdm = net %>% st_bbox() %>% st_as_sfc() %>% st_sample(4, type = 'random')

test_that('network breaks when there are multiple node matches given to st_join', {
  ptsdup = rbind(pts, pts)
  expect_error(st_join(net, ptsdup))
})

test_that('st_network_join creates an sfnetwork before joining an sf to a network', {
  rdm_net = as_sfnetwork(rdm)
  expect_setequal(st_network_join(net, rdm_net), st_network_join(net, rdm))
})
