# Libraries
libs <- c('data.table','plyr','ggplot2','adehabitatHR', 'cowplot',
          'gridExtra', 'sp','dismo','maptools', 'lme4', 'grid', 'gtable', 'segmented')

lapply(libs, require, character.only = TRUE)

# Read in the data
#for the broken stick
yravglocs <- read.csv('data/yravglocs_ranked.csv')
# for the voronoi polygons
raw_all_NNdist <- read.csv("data/raw_all_NNdist.csv")

#### Broken stick plots ----

yravglocs2009 <- yravglocs[yravglocs$Year == "2009",]
yravglocs2010 <- yravglocs[yravglocs$Year == "2010",]
yravglocs2011 <- yravglocs[yravglocs$Year == "2011",]
yravglocs2012 <- yravglocs[yravglocs$Year == "2012",]
yravglocs2013 <- yravglocs[yravglocs$Year == "2013",]

### 2009
# x <- yravglocs2009$idrank
# y <- yravglocs2009$valkm
# 
# lin.mod <- lm(y~x)
# segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi = 14)
# summary(segmented.mod)
# 
# plot(x,y, pch=16)
# plot(segmented.mod, add=T)
# 
# fit <- numeric(length(x)) * NA
# fit[complete.cases(rowSums(cbind(y, x)))] <- broken.line(segmented.mod)$fit
# 
# yravglocs2009$fit <- fit
# yravglocs2009$breakpoint <- 15.903
# f <- 15.903
# predict(segmented.mod, data.frame(x=f))

bstick2009 <-ggplot(yravglocs2009, aes(x = idrank, y = valkm)) + 
  geom_point(aes(color = CalvingGround), size = 0.75) +
  geom_line(aes(x = idrank, y = fit, group = idrank < breakpoint), color = 'black', size = 0.5)+
  ylab("NN dist. (km)")+
  xlab(NULL)+
  theme(legend.position = "none",
        axis.text.x=element_text(size=10, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 10, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 0.5))+
  scale_x_continuous(limits = c(-2, 21), breaks = c(0,5,10,15,20))+
  scale_y_continuous(limits = c(15,60))

### Designate on and off values for calving ground based on broken stick Merge
# yravglocs2009$CalvingGround <- ifelse(yravglocs2009$idrank >= 16, "Off","On")

### 2010
# x <- yravglocs2010$idrank
# y <- yravglocs2010$valkm
# 
# lin.mod <- lm(y~x)
# segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=c(14))
# summary(segmented.mod)
# 
# plot(x,y, pch=16)
# plot(segmented.mod, add=T)
# 
# fit <- numeric(length(x)) * NA
# fit[complete.cases(rowSums(cbind(y, x)))] <- broken.line(segmented.mod, link = F)$fit
# 
# yravglocs2010$fit <- fit
# yravglocs2010$breakpoint <- 19.553
# f <- 19.553
# predict(segmented.mod, data.frame(x=f))

bstick2010 <-ggplot(yravglocs2010, aes(x = idrank, y = valkm)) + 
  geom_point(aes(color = CalvingGround), size = 0.75) +
  geom_line(aes(x = idrank, y = fit, group = idrank < breakpoint), color = 'black', size=0.5)+
  ylab("")+
  xlab(NULL)+
  theme(legend.position = "none",
        axis.text.x=element_text(size=10, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 10, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 0.5))+
  scale_x_continuous(limits = c(-2, 22), breaks = c(0,5,10,15,20))

### Designate on and off values for calving ground based on broken stick Merge
# yravglocs2010$CalvingGround <- ifelse(yravglocs2010$idrank >= 20, "Off","On")

### 2011
# x <- yravglocs2011$idrank
# y <- yravglocs2011$valkm
# 
# lin.mod <- lm(y~x)
# segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=c(14))
# summary(segmented.mod)
# 
# plot(x,y, pch=16)
# plot(segmented.mod, add=T)
# 
# fit <- numeric(length(x)) * NA
# fit[complete.cases(rowSums(cbind(y, x)))] <- broken.line(segmented.mod, link = F)$fit
# 
# yravglocs2011$fit <- fit
# yravglocs2011$breakpoint <- 21.633
# f <- 21.633
# predict(segmented.mod, data.frame(x=f))

bstick2011 <-ggplot(yravglocs2011, aes(x = idrank, y = valkm)) + 
  geom_point(aes(color = CalvingGround), size = 0.75) +
  geom_line(aes(x = idrank, y = fit, group = idrank < breakpoint), color = 'black', size =0.5)+
  ylab("NN dist. (km)")+
  xlab(NULL)+
  theme(legend.position = "none",
        axis.text.x=element_text(size=10, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 10, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 0.5))+
  scale_x_continuous(limits = c(-2, 24), breaks = c(0,5,10,15,20))

### Designate on and off values for calving ground based on broken stick Merge
# yravglocs2011$CalvingGround <- ifelse(yravglocs2011$idrank >= 22, "Off","On")

### 2012
# x <- yravglocs2012$idrank
# y <- yravglocs2012$valkm
# 
# lin.mod <- lm(y~x)
# segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=c(14))
# summary(segmented.mod)
# 
# plot(x,y, pch=16)
# plot(segmented.mod, add=T)
# 
# fit <- numeric(length(x)) * NA
# fit[complete.cases(rowSums(cbind(y, x)))] <- broken.line(segmented.mod, link = F)$fit
# 
# yravglocs2012$fit <- fit
# yravglocs2012$breakpoint <- 16.503
# f <- 16.503
# predict(segmented.mod, data.frame(x=f))

bstick2012 <-ggplot(yravglocs2012, aes(x = idrank, y = valkm)) + 
  geom_point(aes(color = CalvingGround), size = 0.75) +
  geom_line(aes(x = idrank, y = fit, group = idrank < breakpoint), color = 'black', size = 0.5)+
  ylab("")+
  xlab("Ranked Individual") +
  theme(legend.position = "none",
        axis.text.x=element_text(size=10, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 10, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 0.5))+
  scale_x_continuous(limits = c(-2, 20),breaks = c(0,5,10,15,20))+
  scale_y_continuous(limits = c(15, 60))

### Designate on and off values for calving ground based on broken stick Merge
# yravglocs2012$CalvingGround <- ifelse(yravglocs2012$idrank >= 17, "Off","On")

### 2013
# x <- yravglocs2013$idrank
# y <- yravglocs2013$valkm
# 
# lin.mod <- lm(y~x)
# segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=c(10))
# summary(segmented.mod)
# 
# plot(x,y, pch=16)
# plot(segmented.mod, add=T)
# 
# fit <- numeric(length(x)) * NA
# fit[complete.cases(rowSums(cbind(y, x)))] <- broken.line(segmented.mod, link = F)$fit
# 
# yravglocs2013$fit <- fit
# yravglocs2013$breakpoint <- 10.328
# f <- 10.328
# predict(segmented.mod, data.frame(x=f))

bstick2013 <-ggplot(yravglocs2013, aes(x = idrank, y = valkm)) + 
  geom_point(aes(color = CalvingGround), size =0.75) +
  geom_line(aes(x = idrank, y = fit, group = idrank < breakpoint), color = 'black', size = 0.5)+
  ylab("NN dist. (km)") +
  xlab("Ranked Individual")+
  theme(legend.position = "none",
        axis.text.x=element_text(size=10, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 10, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 10),
        axis.title.y = element_text(size =10),
        axis.line = element_line(size = 0.5),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(limits = c(-2, 15),breaks = c(0,5,10,15))

# plot
plot_grid(bstick2009, bstick2010, bstick2011, bstick2012, bstick2013 ,
          labels=c("a", "b", "c", "d", "e"), ncol = 3, nrow = 2, label_size = 14)

#### Voronoi polygon plots ----
setDT(raw_all_NNdist)
#Split them up by year
MR2009 <- raw_all_NNdist[Year == "2009",]
MR2010 <- raw_all_NNdist[Year == "2010",]
MR2011 <- raw_all_NNdist[Year == "2011",]
MR2012 <- raw_all_NNdist[Year == "2012",]
MR2013 <- raw_all_NNdist[Year == "2013",]

# Set the coordinates of input data
MR2009_sp <- SpatialPointsDataFrame(MR2009[, .(EASTING,NORTHING)], 
                                    data=data.frame(id=MR2009$ANIMAL_ID),
                                    proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2010_sp <- SpatialPointsDataFrame(MR2010[, .(EASTING,NORTHING)], 
                                    data=data.frame(id=MR2010$ANIMAL_ID),
                                    proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2011_sp <- SpatialPointsDataFrame(MR2011[, .(EASTING,NORTHING)], 
                                    data=data.frame(id=MR2011$ANIMAL_ID),
                                    proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2012_sp <- SpatialPointsDataFrame(MR2012[, .(EASTING,NORTHING)], 
                                    data=data.frame(id=MR2012$ANIMAL_ID),
                                    proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2013_sp <- SpatialPointsDataFrame(MR2013[, .(EASTING,NORTHING)], 
                                    data=data.frame(id=MR2013$ANIMAL_ID),
                                    proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

## Get centroid data
MR2009[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2010[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2011[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2012[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2013[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]

### Change mr2009a31 in year 2013 to "On" instead of "Off"
MR2013$CalvingGround[MR2013$ANIMAL_ID=="mr2009a31"] <- "On"

### make the voronoi polygons
SpatialPointsDataFrame(unique(MR2009[, .(centX2, centY2, ANIMAL_ID)])[, .(centX2, centY2)],
                       data=unique(MR2009[, .(centX2, centY2, ANIMAL_ID)])[, .(ANIMAL_ID)])


#list by year
ls.MR <- list(MR2009, MR2010, MR2011, MR2012, MR2013)

#function to create voronoi polygons
ls.voronoi <- lapply(1:length(ls.MR),FUN = function(dt){
  # ls.MR[[dt]]
  d <- unique(ls.MR[[dt]][,.(centX2, centY2, ANIMAL_ID)])
  s <- SpatialPointsDataFrame(d[,.(centX2, centY2)], data = d[,.(ANIMAL_ID)])
  voronoi(s)
}) 

#function to create plots ### QUINN: if you uncomment the panel.border line that
#creates the rectangle you will see just how much each of the insets overlaps on
#the broken stick figures
ls.plots.voronoi <- lapply(1:length(ls.MR), FUN = function(yr){
  g = ggplot() + 
    geom_polygon(aes(long,lat, group=group), colour="black", fill=NA, size=0.25, alpha = 0.5, 
                 data=fortify(ls.voronoi[[yr]])) +
    geom_point(aes(centX2, centY2, colour = CalvingGround), data = ls.MR[[yr]], alpha = 0.5, size=0.5)+
    #geom_text(aes(600000,5390000),label=labels[[yr]])+
    ylab("Northing") + 
    xlab("Easting") +
    xlim(600000,690000)+
    ylim(5290000,5390000)+
    theme(legend.position = 'none',
          plot.title = element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          strip.text = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          #panel.border = element_rect(colour = 'black', fill=NA, size=0.5))
          panel.border = element_blank())
  
  g
})

# plot voronoi polygons
do.call(grid.arrange, c(ls.plots.voronoi, nrow = 3, ncol = 2, left="Northing", bottom="Easting"))

### Insetting the voronoi polygons with the broken stick regressions
# Make each voronoi its own object
vor2009 <- ls.plots.voronoi[[1]]
vor2010 <- ls.plots.voronoi[[2]]
vor2011 <- ls.plots.voronoi[[3]]
vor2012 <- ls.plots.voronoi[[4]]
vor2013 <- ls.plots.voronoi[[5]]

#Make insets custom annotations 
### QUINN: I think here is maybe where the problem might be with the insets. 
#Something within the ggplotGrob (which is part of ggtable) function might be able to fix it
inset2009 <- bstick2009 + annotation_custom(ggplotGrob(vor2009), 
                                            xmin = -6, xmax = 14, ymin=30, ymax=68)

inset2010 <- bstick2010 + annotation_custom(ggplotGrob(vor2010), 
                                            xmin = -6, xmax = 14, ymin=30, ymax=68)

inset2011 <- bstick2011 + annotation_custom(ggplotGrob(vor2011), 
                                            xmin = -6, xmax = 14, ymin=30, ymax=68)

inset2012 <- bstick2012 + annotation_custom(ggplotGrob(vor2012), 
                                            xmin = -6, xmax = 14, ymin=30, ymax=68)

inset2013 <- bstick2013 + annotation_custom(ggplotGrob(vor2013), 
                                            xmin = -4, xmax = 7, ymin=32, ymax=42)

# Plot broken stick with insets
#pdf("graphics/Fig2_brokenStickPoly.pdf",width = 5, height = 5, pointsize = 10)
#jpeg("/Users/maegwinbonar/Desktop/Vander Wal Lab/Bonar_et_al_fig2.jpeg",width = 7.0, height = 7.0, pointsize = 12, res = 300, units = "in")
png("graphics/Fig2_brokenStickPoly.png", height = 4500, width = 3000, res = 600, units = "px")

plot_grid(inset2009, inset2010, inset2011, inset2012, inset2013 ,
          labels=c("a", "b", "c", "d", "e"), ncol = 2, nrow = 3, label_size = 14)
dev.off()

