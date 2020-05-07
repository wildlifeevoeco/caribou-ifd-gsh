

### Packages ----
libs <- c('data.table', 'sf',
          'gridExtra', 'ggplot2',
          'sp', 'adehabitatHR', 'dismo')
lapply(libs, require, character.only = TRUE)

## Load group size data
obs <- readRDS("data/group size/group-size-spatial.RDS")

## Load collar data
DT <- fread("data/figures/Fig2/raw_all_NNdist.csv")
DT <- DT[Year == "2012"]

DT[, .N, by = "CalvingGround"]

DTSP <- st_as_sf(DT, coords = c("X_COORD", "Y_COORD"), 
                 crs = "+proj=longlat +ellps=GRS80 +no_defs")

## Generate home range for animals on the calving ground
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

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
themeMap <- theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), 
                  legend.key = element_blank(),
                  panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = "#d0c2a9"), 
                  panel.grid = element_line(color = 'black', size = 0.1),
                  axis.text.y = element_text(size = 11, color = 'black'),
                  axis.text.x = element_text(angle = 45, hjust = 1, 
                                             size = 11, color = 'black'), 
                  axis.title = element_blank())

obs$`Group Size` <- obs$group.size
DTSP$`Calving Ground` <- DTSP$CalvingGround

### Plot ----
png("graphics/FigS6.png", 
    width = 6000, height = 4000, units = "px", res = 600)
aa <- ggplot() +
   geom_sf(data = obs, aes(size = `Group Size`), alpha = 0.25) +
   geom_polygon(data = df, aes(x = long, y = lat), 
                 color = "black", 
                fill = "dodgerblue",
                alpha = 0.15) +
   ggtitle("A)") +
   ylim(47.7, 48.7) +
   xlim(-55.7, -54.5) +
   scale_size_continuous(breaks=c(1, 10, 50, 100)) +
   themeMap

bb <- ggplot() +
   geom_sf(data = DTSP, aes(color = `Calving Ground`), alpha = 0.15) +
   ggtitle("B)") +
   ylim(47.7, 48.7) +
   xlim(-55.7, -54.5) +
   themeMap

grid.arrange(aa,bb, ncol = 2)

dev.off()

##### EXTRA FOR VORONOI POLYGONS 

### generate voronoi polygons 
# for the voronoi polygons
raw_all_NNdist <- fread("data/raw_all_NNdist.csv")
MR2012 <- raw_all_NNdist[Year == "2012"]

MR2012_sp <- SpatialPointsDataFrame(MR2012[, .(X_COORD,Y_COORD)], 
                                    data=data.frame(id=MR2012$ANIMAL_ID),
                                    proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF
## Get centroid data
MR2012[, c('centX2', 'centY2') := .(mean(X_COORD),  mean(Y_COORD)), by = ANIMAL_ID]

MR2012mean <- MR2012[, .(mean(X_COORD),  mean(Y_COORD)), by = c("ANIMAL_ID", "CalvingGround")]
colnames(MR2012mean) <- c("ANIMAL_ID", "CalvingGround" ,"centX2", "centY2")


### make the voronoi polygons
SpatialPointsDataFrame(unique(MR2012[, .(centX2, centY2, ANIMAL_ID)])[, .(centX2, centY2)],
                       data=unique(MR2012[, .(centX2, centY2, ANIMAL_ID)])[, .(ANIMAL_ID)])


#list by year
ls.MR <- list(MR2012)

#function to create voronoi polygons
ls.voronoi <- lapply(1:length(ls.MR),FUN = function(dt){
   # ls.MR[[dt]]
   d <- unique(ls.MR[[dt]][,.(centX2, centY2, ANIMAL_ID, CalvingGround)])
   s <- SpatialPointsDataFrame(d[,.(centX2, centY2)], data = d[,.(ANIMAL_ID, CalvingGround)])
   voronoi(s)
}) 

#geom_polygon(data = fortify(ls.voronoi[[1]]), 
#             aes(long,lat, group = group), 
#             colour = "black", fill = NA,
#             size = 0.5, alpha = 0.25) +
#geom_point(data = MR2012mean, aes(centX2, centY2, color = CalvingGround),
#            size = 2, alpha = 0.75) +