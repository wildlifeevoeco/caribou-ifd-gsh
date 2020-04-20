

### Packages ----
libs <- c('sf', 'dadta.table',
          'osmdata', 'ggplot2')
lapply(libs, require, character.only = TRUE)

## Load group size data
obs <- fread("data/group-size-spatial.csv")


### Download OSM data ----
# Set up bounding box - order: xmin, ymin, xmax, ymax
bb <- c(xmin = min(obs$longitude),
        ymin = min(obs$latitude),
        xmax = max(obs$longitude),
        ymax = max(obs$latitude))

## Middle Ridge Area
zz <- opq(bb) %>%
  add_osm_feature(key = 'place', value = 'island') %>%
  osmdata_sf()

lns <- zz$osm_lines

# Union -> polygonize -> cast lines = geometry set
castpolys <- st_cast(st_polygonize(st_union(lns)))

# Combine geometries and cast as sf
nl <- st_as_sf(castpolys)

### Reproject islands ----
# Projections
utm <- st_crs('+proj=utm +zone=21 ellps=WGS84')

# Project to UTM
utmNL <- st_transform(nl, utm)

### Output ----
#st_write(utmNL, 'output/newfoundland-polygons.gpkg')


### Theme ----
# Colors
source('code/00-palette.R')

# Theme
themeMap <- theme(panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = watercol),
                  panel.grid = element_line(color = 'black', size = 0.2),
                  axis.text = element_text(size = 11, color = 'black'),
                  axis.title = element_blank())

### Plot ----
# NOTE: this figure only has the main island's coastline (eg missing Fogo)
# Base NL
(gnl <- ggplot(nl) +
   geom_sf(fill = islandcol, color = coastcol, size = 0.3) +
   themeMap)

ggplot(obs) +
  geom_point(aes(longitude, latitude, size = group.size), alpha = 0.25)
