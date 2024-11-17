library(osmdata)
library(sf)
library(tidyverse)

# obtain salzburg bounding box
bb = getbb("Salzburg, Austria", format_out = "sf_polygon")
# get the city boundary and not the province boundary
bb = bb |>
  mutate(area = st_area(geometry)) |>
  filter(area == min(area, na.rm = TRUE))

# query features with the name mozart using osmdata
mozart_query = opq(bbox = st_bbox(bb)) |>
  add_osm_feature(key = "name", value = "mozart",
                  value_exact = FALSE, match_case = FALSE) |>
  osmdata_sf()

# extract relevant POIs from points, polygons and multipolygons
pts = mozart_query$osm_points |>
  filter(str_detect(name, "Mozart")) |>
  filter(name %in% c(
    "Café Mozart",
    "Spirit of Mozart",
    "Mozart-Eine Hommage",
    "Haus für Mozart",
    "Mozartkugel",
    "Altstadthotel Kasererbraeu Mozartkino GmbH", #rename just Mozartkino
    "Mozartsteg/Imbergstraße",
    "Mozartsteg/Rudolfskai"
  )) |>
  mutate(name = case_when(
    str_detect(name, "Mozartkino") ~ "Mozartkino",
    TRUE ~ name
  )) |>
  as_tibble() |>
  st_as_sf()
pls = mozart_query$osm_polygons |>
  filter(str_detect(name, "Mozart")) |>
  filter(!str_detect(name, "Solit")) |>
  filter(name != "Mozarteum") |>
  filter(!(`addr:street` %in% c("Schrannengasse", "Paris-Lodron-Straße"))) |>
  st_centroid() |>
  as_tibble() |>
  st_as_sf()

# combine datasets, select relevant attributes and combine into
# a single attribute called type
# compute x and y coordinates to order the points
mozart = bind_rows(pts, pls) |>
  select(name, amenity, tourism, craft, building,
         highway, man_made, website) |>
  mutate(
    type = coalesce(amenity, tourism, craft, building, highway, man_made),
    x = st_coordinates(geometry)[,"X"],
    y = st_coordinates(geometry)[,"Y"]
  ) |>
  arrange(y, x) |>
  select(name, type, website) |>
  st_set_agr(c(name = "constant", type = "constant", website = "constant")) |>
  st_transform(3035) |>
  mutate(across(where(is.character), .fns = function(x){return(`Encoding<-`(x, "UTF-8"))}))

st_crs(mozart)$wkt <- gsub("ü", "\\\u00fc", st_crs(mozart)$wkt)

# save as lazy data
usethis::use_data(mozart, overwrite = TRUE)
