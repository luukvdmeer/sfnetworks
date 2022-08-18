library(sf)
library(dplyr)

rect = roxel %>%
  st_union() %>%
  st_transform(3857) %>%
  st_centroid() %>%
  st_buffer(dist = 500, endCapStyle = "SQUARE") %>%
  st_transform(4326) %>%
  st_as_sf(foo = "bar")

test_that("sf functions for sfnetworks with spatially implicit edges,
         give an error", {
  message = "This call requires spatially explicit edges"
  net = as_sfnetwork(roxel, edges_as_lines = F) %>% activate("edges")
  # Geometries
  expect_error(st_coordinates(net), message)
  expect_error(st_is(net, "LINESTRING"), message)
  # Coordinates
  expect_error(st_shift_longitude(net), message)
  expect_error(st_wrap_dateline(net), message)
  expect_error(st_normalize(net), message)
  expect_error(st_transform(net, 3857), message)
  expect_error(st_zm(net), message)
  expect_error(st_m_range(net), message)
  expect_error(st_z_range(net), message)
  # Geometry binary predicate
  expect_error(st_intersects(net, rect), message)
  # Geometry unary operations
  expect_error(st_simplify(net), message)
  # Join and filter
  expect_error(st_crop(net, rect), message)
  expect_error(st_intersection(net, rect), message)
  expect_error(st_difference(net, rect), message)
  expect_error(st_filter(net, rect), message)
})

### crs

test_that("st_set_crs sets the crs for edges and nodes", {
  net = as_sfnetwork(roxel) %>%
    st_set_crs(NA)
  expect_equal(st_crs(activate(net, "nodes")), st_crs(activate(net, "edges")))
})

test_that("st_transform changes crs for edges and nodes", {
  net = as_sfnetwork(roxel) %>%
    st_transform(3857)
  expect_equal(st_crs(activate(net, "nodes")), st_crs(activate(net, "edges")))
})

### precision

test_that("st_set_precision sets the precision for edges and nodes", {
  net = as_sfnetwork(roxel) %>%
    st_set_precision(1)
  expect_equal(
    st_precision(activate(net, "nodes")),
    st_precision(activate(net, "edges"))
  )
})

### clipping
test_that("st_crop gives a warning and returns a valid network", {
  net <- as_sfnetwork(roxel, directed = F)
  expect_warning(
    crop <- st_crop(net, rect),
    "assumed to be spatially constant"
  )
  expect_null(sfnetworks:::require_valid_network_structure(crop))
})

test_that("st_intersection gives a warning and returns a valid network", {
  net <- as_sfnetwork(roxel, directed = F)
  expect_warning(
    intersection <- st_intersection(net, rect),
    "assumed to be spatially constant"
  )
  expect_null(sfnetworks:::require_valid_network_structure(intersection))
})

test_that("st_difference gives a warning and returns a valid network", {
  net <- as_sfnetwork(roxel, directed = F)
  expect_warning(
    difference <- st_difference(net, rect),
    "assumed to be spatially constant"
  )
  expect_null(sfnetworks:::require_valid_network_structure(difference))
})

### st_reverse
node1 = st_point(c(0, 0))
node2 = st_point(c(1, 0))
edge = st_sfc(st_linestring(c(node1, node2)))

dirnet = as_sfnetwork(edge)
undirnet = as_sfnetwork(edge, directed = FALSE)

current_geos = numeric_version(sf::sf_extSoftVersion()["GEOS"])
required_geos = numeric_version("3.7.0")

test_that("st_reverse returns valid networks", {
  skip_if_not(current_geos >= required_geos)
  reversed_D <- suppressWarnings(st_reverse(activate(dirnet, "edges")))
  reversed_U <- st_reverse(activate(undirnet, "edges"))
  expect_null(sfnetworks:::require_valid_network_structure(reversed_D))
  expect_null(sfnetworks:::require_valid_network_structure(reversed_U))
})

test_that("st_reverse gives a warning when nodes are active, keeping the same
          coordinates order and from/to columns", {
  skip_if_not(current_geos >= required_geos)
  expect_warning(reversed <- st_reverse(dirnet), "no effect on nodes")
  expect_setequal(st_coordinates(reversed), st_coordinates(dirnet))
  expect_setequal(
    st_coordinates(activate(reversed, "edges")),
    st_coordinates(activate(dirnet, "edges"))
  )
  expect_equal(
    pull(activate(reversed, "edges"), from),
    pull(activate(dirnet, "edges"), from)
  )
  expect_equal(
    pull(activate(reversed, "edges"), to),
    pull(activate(dirnet, "edges"), to)
  )
})

test_that("st_reverse reverses the order of the to/from columns and the
          order of the coordinates for directed networks", {
  skip_if_not(current_geos >= required_geos)
  expect_warning(
    reversed <- st_reverse(activate(dirnet, "edges")),
    "st_reverse swaps columns"
  )
  expect_equal(
    st_coordinates(reversed)[1, ],
    st_coordinates(activate(dirnet, "edges"))[2, ]
  )
  expect_equal(
    st_coordinates(reversed)[2, ],
    st_coordinates(activate(dirnet, "edges"))[1, ]
  )
  expect_equal(
    pull(activate(reversed, "edges"), from),
    pull(activate(dirnet, "edges"), to)
  )
  expect_equal(
    pull(activate(reversed, "edges"), to),
    pull(activate(dirnet, "edges"), from)
  )
})

test_that("st_reverse reverses the order of the coordinates for
          undirected networks", {
  skip_if_not(current_geos >= required_geos)
  reversed <- st_reverse(activate(undirnet, "edges"))
  expect_equal(
    st_coordinates(reversed)[1, ],
    st_coordinates(activate(undirnet, "edges"))[2, ]
  )
  expect_equal(
    st_coordinates(reversed)[2, ],
    st_coordinates(activate(undirnet, "edges"))[1, ]
  )
  expect_equal(
    pull(activate(reversed, "edges"), from),
    pull(activate(undirnet, "edges"), from)
  )
  expect_equal(
    pull(activate(reversed, "edges"), to),
    pull(activate(undirnet, "edges"), to)
  )

})

### geometry

test_that("dropping geometry for activated nodes changes the class to
          tbl_graph", {
  net = roxel %>% as_sfnetwork()
  expect_s3_class(net %>% sf::st_drop_geometry(), "tbl_graph")
  expect_s3_class(net %>% sf::st_set_geometry(NULL), "tbl_graph")
})

test_that("dropping geometry for activated edges remains an sfnetwork", {
  net = roxel %>% as_sfnetwork() %>% activate("edges")
  expect_s3_class(net %>% sf::st_drop_geometry(), "sfnetwork")
  expect_s3_class(net %>% sf::st_set_geometry(NULL), "sfnetwork")
})

test_that("st_set_geometry gives an error when replacing edges geometry
          type", {
  net = roxel %>% as_sfnetwork()
  # warnings are suppressed since they relate to the sf package
  # warning: st_centroid assumes attributes are constant over geometries of x
  centroids = suppressWarnings(sf::st_centroid(roxel))
  new_geom = st_geometry(centroids)
  expect_error(activate(net, "edges") %>% sf::st_set_geometry(new_geom))
})

test_that("st_set_geometry gives an error when replacing edges geometry CRS", {
  net = roxel %>% as_sfnetwork()
  new_geom = st_geometry(st_transform(roxel, 3035))
  expect_error(activate(net, "edges") %>% sf::st_set_geometry(new_geom))
})

test_that("st_set_geometry gives an error when replacing edges geometry
          endpoints", {
  skip_if_not(current_geos >= required_geos)
  net = roxel %>% as_sfnetwork()
  new_geom = sf::st_geometry(sf::st_reverse(roxel))
  expect_error(activate(net, "edges") %>% sf::st_set_geometry(new_geom))
})
