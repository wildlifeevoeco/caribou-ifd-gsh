

### Packages ----
libs <- c('sf', 'data.table',
          'osmdata', 'ggplot2')
lapply(libs, require, character.only = TRUE)

## Load group size data
obs <- readRDS("data/group-size-spatial.RDS")

## Load collar data
DT <- readRDS("output/1-caribou-all.RDS")
DT <- DT[Year == "2012"]

### Theme ----

# Theme
themeMap <- theme(panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = "#d0c2a9"),
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
