library(hexSticker)
library(osmdata)
library(sf)
library(ggplot2)
library(showtext)
library(magick)
# Get data from OSM
r = opq(bbox = c(7.602,51.952,7.646,51.978)) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()
road = r$osm_lines
w = opq(bbox = c(7.602,51.952,7.646,51.978)) %>%
  add_osm_feature(key = 'water') %>%
  osmdata_sf()
water = w$osm_polygons

# Create a buffered zoom point
zoom = st_point(c(7.622,51.9603)) %>%
  st_sfc(crs = 4326) %>%
  st_transform(3857) %>%
  st_buffer(2100) %>%
  st_transform(4326)
roadz = road %>%
  st_intersection(zoom)
waterz = water %>%
  st_intersection(zoom)

# Subplot with ggplot
gg = ggplot() +
  geom_sf(data = roadz, size = 0.25, color = 'grey10') +
  geom_sf(data = waterz, fill = 'grey30', color = NA) +
  theme_void() + theme_transparent()

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Orbitron",  "orbitron")
## Automatically use showtext to render text for future devices
showtext_auto()

sticker(
  subplot = gg,
  s_x = 1, s_y = 1, s_width = 2.3, s_height = 2.3,
  package = 'sfnetworks',
  p_size = 58, p_color = '#fffefe',  p_fontface = 'bold',
  p_x= 1, p_y = 1, p_family = 'orbitron',
  #hexagon
  h_fill = 'grey50', h_size = 1.5, h_color = '#ff9000',
  url = '© OpenStreetMap   contributors',
  u_size = 5, u_color = 'grey10', u_x = 1, u_y = 0.045,
  spotlight = T,
  l_x = 1, l_y = 0.75, l_alpha = 0.1, l_width = 4, l_height = 4,
  dpi = 900,  white_around_sticker = TRUE,
  filename = 'man/figures/templogo.png'
)

# Make white around sticker transparent with magick
hex = image_read('sfnetworks.png')
hextransparent = hex %>%
  image_transparent('white')
image_write(hextransparent, path = 'man/figures/logo.png')
file.remove('man/figures/templogo.png')
