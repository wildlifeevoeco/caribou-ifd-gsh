

### Packages ----
libs <- c('sf', 'data.table',
          'osmdata', 'ggplot2')
lapply(libs, require, character.only = TRUE)

## Load group size data
obs <- readRDS("data/group-size-spatial.RDS")

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

ggplot(obs) +
   geom_sf(aes(size = group.size), alpha = 0.25) +
   themeMap

### Output ----
ggsave(
  'graphics/FigS6-map.png',
  gnl,
  width = 7,
  height = 7,
  dpi = 320
)


ggplot(obs) +
  geom_point(aes(longitude, latitude, size = group.size), alpha = 0.25)
