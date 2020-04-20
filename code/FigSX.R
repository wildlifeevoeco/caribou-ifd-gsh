

### Packages ----
libs <- c('data.table',
          'gridExtra', 'ggplot2',
          'sp', 'adehabitatHR')
lapply(libs, require, character.only = TRUE)

## Load group size data
obs <- readRDS("data/group-size-spatial.RDS")

## Load collar data
DT <- fread("data/raw_all_NNdist.csv")
DT <- DT[Year == "2012"]

DT[, .N, by = "CalvingGround"]

DTSP <- st_as_sf(DT, coords = c("X_COORD", "Y_COORD"), 
                 crs = "+proj=longlat +ellps=GRS80 +no_defs")

## Generate home range for animals on the calving ground
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

source("functions/GetHRBy.R")

coords <- c('X_COORD', 'Y_COORD')

### generate ranges for communities
pts <- SpatialPointsDataFrame(DT[, ..coords],
                              proj4string = CRS(utm21N),
                              data = DT[, .(CalvingGround)])

ud <- kernelUD(pts, grid = 900, extent = 9)
vertices <- getverticeshr(ud, 99.5)
df <- data.table(fortify(vertices))
df <- df[id == "On"]


### Theme ----
# Theme
themeMap <- theme(legend.key = element_blank(),
                  panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = "#d0c2a9"),
                  panel.grid = element_line(color = 'black', size = 0.2),
                  axis.text = element_text(size = 11, color = 'black'),
                  axis.title = element_blank())

### Plot ----
png("graphics/FigS6.1.png", 
    width = 5000, height = 2500, units = "px", res = 600)
aa <- ggplot(obs) +
   geom_sf(aes(size = group.size), alpha = 0.25) +
   #geom_polygon(data = df, aes(x = long, y = lat), 
  #              color = "black", 
  #              fill = "dodgerblue",
  #              alpha = 0.15) +
   ylim(47.5, 48.7) +
   xlim(-55.7, -54.5) +
   scale_size_continuous(breaks=c(1, 10, 50, 100)) +
   themeMap

bb <- ggplot(DTSP) +
   geom_sf(aes(color = CalvingGround), alpha = 0.25) +
   ylim(47.5, 48.7) +
   xlim(-55.7, -54.5) +
   themeMap

grid.arrange(aa,bb, ncol = 2)
dev.off()

png("graphics/FigS6.2.png", 
    width = 2500, height = 2500, units = "px", res = 600)
ggplot(obs) +
  geom_sf(aes(size = group.size), alpha = 0.25) +
  geom_polygon(data = df, aes(x = long, y = lat), 
                color = "black", 
                fill = "dodgerblue",
                alpha = 0.15) +
  ylim(47.5, 48.7) +
  xlim(-55.7, -54.5) +
  scale_size_continuous(breaks=c(1, 10, 50, 100)) +
  themeMap
dev.off()