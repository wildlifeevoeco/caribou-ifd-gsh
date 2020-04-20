

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


### Theme ----

# Theme
themeMap <- theme(legend.key = element_blank(),
                  panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = "#d0c2a9"),
                  panel.grid = element_line(color = 'black', size = 0.2),
                  axis.text = element_text(size = 11, color = 'black'),
                  axis.title = element_blank())

### Plot ----
png("graphics/FigS6.png", 
    width = 5000, height = 2500, units = "px", res = 600)
aa <- ggplot(obs) +
   geom_sf(aes(size = group.size), alpha = 0.25) +
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

## Generate home range for animals on the calving ground
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

source("functions/GetHRBy.R")

coords <- c('X_COORD', 'Y_COORD')

### generate ranges for communities
pts <- SpatialPointsDataFrame(DT[, ..coords],
                                      proj4string = CRS(utm21N),
                                      data = DT[, .(CalvingGround)])

ud <- kernelUD(pts, grid = 700, extent = 4)
vertices <- getverticeshr(ud, 99)
df <- fortify(vertices)

ggplot(subset(df, id == "On"), aes(x = long, y = lat)) +
  geom_polygon(alpha = 0.25, size = 0.5, color = "black") +
  coord_equal() 
  
  
  ylab("Northing") +
  xlab("Easting") +
  xlim(685000, 720000) +
  theme(axis.title = element_text(size = 14, color = 'black'),
        axis.text.y = element_text(size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black', angle = 45, hjust = 1),
        plot.title=element_text(size = 12, hjust=0),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        strip.text = element_text(size = 12))

