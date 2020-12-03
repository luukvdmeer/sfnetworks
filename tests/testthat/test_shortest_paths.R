library(sf)
net = as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035)

# Create random points inside network bbox
rdm = net %>% st_bbox() %>% st_as_sfc() %>% st_sample(4, type = 'random')

st_cost(net, rdm[1:2,], rdm[3:4,])
