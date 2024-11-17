library(osmdata)
library(sf)
library(tidyverse)
library(sfnetworks)
library(tidygraph)

# set a bounding box to query with OSM
bb = c(xmin = 7.522594, ymin = 51.941512,
       xmax = 7.546705, ymax = 51.961194)

# set a polygon to crop the resulting lines
poly = st_as_sfc(
  "POLYGON ((7.522624 51.95441, 7.522594 51.95372, 7.522746 51.94778, 7.527507 51.94151, 7.527601 51.94152, 7.5318 51.94213, 7.532369 51.94222, 7.533006 51.9424, 7.540591 51.94474, 7.543329 51.9463, 7.543709 51.94653, 7.544452 51.9471, 7.546705 51.95124, 7.546326 51.95408, 7.544203 51.95952, 7.543794 51.95971, 7.543638 51.95978, 7.527494 51.9612, 7.522632 51.95446, 7.522624 51.95441))",
  crs = 4326
)

# query with osmdata
roxel_query = opq(bbox = bb) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

# intersect with polygon, transmute to keep only name and
# highway renamed as type, remove unwanted types
# and cast multilinestrings to linestrings
# to pass to sfnetwork
roxel_lines = roxel_query$osm_lines |>
  st_intersection(poly) |>
  transmute(
    name = name,
    type = highway
  ) |>
  filter(!(type %in% c("construction", "motorway", "bridelway"))) |>
  st_cast("LINESTRING") |>
  mutate(across(where(is.character), .fns = function(x){return(`Encoding<-`(x, "UTF-8"))}))

# pre-processing:
# -> reduce the components
# -> smooth and subdivide
# -> extract edges with corresponding attributes
roxel_clean = as_sfnetwork(roxel_lines) |>
  # filter(group_components() <= 180) |>
  convert(to_spatial_smooth) |>
  convert(to_spatial_subdivision) |>
  st_as_sf("edges") |>
  select(name, type) |>
  st_set_agr(c(name = "constant", type = "constant"))

# reorder the dataset to have more names on the first 10 rows
set.seed(92612)
roxel = roxel_clean[sample(1:nrow(roxel_clean)), ]

# save as lazy data
usethis::use_data(roxel, overwrite = TRUE)
