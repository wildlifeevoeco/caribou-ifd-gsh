libs <- c('sqldf','data.table','plyr','ggplot2','adehabitatHR',
          'gridExtra', 'sp','readr')

lapply(libs, require, character.only = TRUE)
library(dismo)
library(dplyr)
avg_daily_NNdist <- read_csv("~/Downloads/Honours Readings/Statistics /data/avg_daily_NNdist.csv")
avg_season_NNdist <- read_csv("~/Downloads/Honours Readings/Statistics /data/avg_season_NNdist.csv")
raw_all_NNdist <- read_csv("~/Downloads/Honours Readings/Statistics /data/raw_all_NNdist.csv")
View(raw_all_NNdist)

raw_all_NNdist$Year2 <- substr(raw_all_NNdist$FIX_DATE, 1,4)
#Remove individuals with not enough data 
raw_all_NNdist2 <- raw_all_NNdist[!(raw_all_NNdist$ANIMAL_ID == "mr2009a27" & raw_all_NNdist$Year2 == 2011),]
raw_all_NNdist2 <- raw_all_NNdist2[!(raw_all_NNdist2$ANIMAL_ID == "mr2009a14" & raw_all_NNdist2$Year2 == 2011),]
raw_all_NNdist2 <- raw_all_NNdist2[!(raw_all_NNdist2$ANIMAL_ID == "mr2013a15" & raw_all_NNdist2$Year2 == 2013),]

summary(raw_all_NNdist2)

raw_all_NNdist2$IDyear <- paste(raw_all_NNdist2$ANIMAL_ID, raw_all_NNdist2$Year, sep = "_")
raw_all_NNdist2$ANIMAL_ID <- as.character(raw_all_NNdist2$ANIMAL_ID)
str(MR2009)

setDT(raw_all_NNdist2)

#Split them up by year
MR2009 <- raw_all_NNdist2[Year == "2009",]
MR2010 <- raw_all_NNdist2[Year == "2010",]
MR2011 <- raw_all_NNdist2[Year == "2011",]
MR2012 <- raw_all_NNdist2[Year == "2012",]
MR2013 <- raw_all_NNdist2[Year == "2013",]

##Find daily values?

library(tidyr)
MR2009N <- separate(MR2009, "FIX_DATE", c("Year", "Month", "Day"), sep = "-")
MR2010N <- separate(MR2010, "FIX_DATE", c("Year", "Month", "Day"), sep = "-")
MR2011N <- separate(MR2011, "FIX_DATE", c("Year", "Month", "Day"), sep = "-")
MR2012N <- separate(MR2012, "FIX_DATE", c("Year", "Month", "Day"), sep = "-")
MR2013N <- separate(MR2013, "FIX_DATE", c("Year", "Month", "Day"), sep = "-")

MR2009N$IDmonth <- paste(MR2009N$ANIMAL_ID, MR2009N$Month, sep = "_")
MR2010N$IDmonth <- paste(MR2010N$ANIMAL_ID, MR2010N$Month, sep = "_")
MR2011N$IDmonth <- paste(MR2011N$ANIMAL_ID, MR2011N$Month, sep = "_")
MR2012N$IDmonth <- paste(MR2012N$ANIMAL_ID, MR2012N$Month, sep = "_")
MR2013N$IDmonth <- paste(MR2013N$ANIMAL_ID, MR2013N$Month, sep = "_")


##2009
MR2009_05 <- MR2009N[Month == "05",]
MR2009_06 <- MR2009N[Month == "06",]
MR2009_07 <- MR2009N[Month == "07",]

##2010
MR2010_05 <- MR2010N[Month == "05",]
MR2010_06 <- MR2010N[Month == "06",]
MR2010_07 <- MR2010N[Month == "07",]

##2011
MR2011_05 <- MR2011N[Month == "05",]
MR2011_06 <- MR2011N[Month == "06",]
MR2011_07 <- MR2011N[Month == "07",]

##2012
MR2012_05 <- MR2012N[Month == "05",]
MR2012_06 <- MR2012N[Month == "06",]
MR2012_07 <- MR2012N[Month == "07",]

##2013
MR2013_05 <- MR2013N[Month == "05",]
MR2013_06 <- MR2013N[Month == "06",]
MR2013_07 <- MR2013N[Month == "07",]

# Set the coordinates of input data
##Maria--BY MONTH test

##2009
MR2009_05sp <- SpatialPointsDataFrame(MR2009_05[, .(EASTING,NORTHING)], 
                                     data=data.frame(id=MR2009_05$ANIMAL_ID),
                                     proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2009_06sp <- SpatialPointsDataFrame(MR2009_06[, .(EASTING,NORTHING)], 
                                    data=data.frame(id=MR2009_06$ANIMAL_ID),
                                    proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2009_07sp <- SpatialPointsDataFrame(MR2009_07[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2009_07$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

##2010
MR2010_05sp <- SpatialPointsDataFrame(MR2010_05[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2010_05$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2010_06sp <- SpatialPointsDataFrame(MR2010_06[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2010_06$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2010_07sp <- SpatialPointsDataFrame(MR2010_07[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2010_07$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

##2011
MR2011_05sp <- SpatialPointsDataFrame(MR2011_05[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2011_05$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2011_06sp <- SpatialPointsDataFrame(MR2011_06[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2011_06$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2011_07sp <- SpatialPointsDataFrame(MR2011_07[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2011_07$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

##2012
MR2012_05sp <- SpatialPointsDataFrame(MR2012_05[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2012_05$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2012_06sp <- SpatialPointsDataFrame(MR2012_06[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2012_06$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2012_07sp <- SpatialPointsDataFrame(MR2012_07[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2012_07$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

##2013
MR2013_05sp <- SpatialPointsDataFrame(MR2013_05[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2013_05$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2013_06sp <- SpatialPointsDataFrame(MR2013_06[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2013_06$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF

MR2013_07sp <- SpatialPointsDataFrame(MR2013_07[, .(EASTING,NORTHING)], 
                                      data=data.frame(id=MR2013_07$ANIMAL_ID),
                                      proj4string = CRS("+proj=utm +zone=21 ellps=WGS84")) #create SPDF




## run MCP 
## ID NEEDS TO BE CHARACTER
##Maria--BY MONTH test 
#2009
MR2009_05_mcp95 <- mcp(MR2009_05sp, 95)
MR2009_05_mcp50 <- mcp(MR2009_05sp, 50)

MR2009_06_mcp95 <- mcp(MR2009_06sp, 95)
MR2009_06_mcp50 <- mcp(MR2009_06sp, 50)

MR2009_07_mcp95 <- mcp(MR2009_07sp, 95)
MR2009_07_mcp50 <- mcp(MR2009_07sp, 50)

#2010
MR2010_05_mcp95 <- mcp(MR2010_05sp, 95)
MR2010_05_mcp50 <- mcp(MR2010_05sp, 50)

MR2010_06_mcp95 <- mcp(MR2010_06sp, 95)
MR2010_06_mcp50 <- mcp(MR2010_06sp, 50)

MR2010_07_mcp95 <- mcp(MR2010_07sp, 95)
MR2010_07_mcp50 <- mcp(MR2010_07sp, 50)

#2011
MR2011_05_mcp95 <- mcp(MR2011_05sp, 95)
MR2011_05_mcp50 <- mcp(MR2011_05sp, 50)

MR2011_06_mcp95 <- mcp(MR2011_06sp, 95)
MR2011_06_mcp50 <- mcp(MR2011_06sp, 50)

MR2011_07_mcp95 <- mcp(MR2011_07sp, 95)
MR2011_07_mcp50 <- mcp(MR2011_07sp, 50)

#2012
MR2012_05_mcp95 <- mcp(MR2012_05sp, 95)
MR2012_05_mcp50 <- mcp(MR2012_05sp, 50)

MR2012_06_mcp95 <- mcp(MR2012_06sp, 95)
MR2012_06_mcp50 <- mcp(MR2012_06sp, 50)

MR2012_07_mcp95 <- mcp(MR2012_07sp, 95)
MR2012_07_mcp50 <- mcp(MR2012_07sp, 50)

#2013
MR2013_05_mcp95 <- mcp(MR2013_05sp, 95)
MR2013_05_mcp50 <- mcp(MR2013_05sp, 50)

MR2013_06_mcp95 <- mcp(MR2013_06sp, 95)
MR2013_06_mcp50 <- mcp(MR2013_06sp, 50)

MR2013_07_mcp95 <- mcp(MR2013_07sp, 95)
MR2013_07_mcp50 <- mcp(MR2013_07sp, 50)

## Get centroid data
## get centroid for calving date
MR2009_05[as.IDate(FIX_DATE)==as.IDate(CalvingDate), c('centX', 'centY') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2009_06[as.IDate(FIX_DATE)==as.IDate(CalvingDate), c('centX', 'centY') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2009_07[as.IDate(FIX_DATE)==as.IDate(CalvingDate), c('centX', 'centY') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]


##Maria--BY MONTH test
MR2009_05[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2009_06[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2009_07[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]

MR2010_05[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2010_06[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2010_07[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]

MR2011_05[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2011_06[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2011_07[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]

MR2012_05[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2012_06[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2012_07[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]

MR2013_05[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2013_06[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
MR2013_07[, c('centX2', 'centY2') := .(mean(EASTING), mean(NORTHING)), by = ANIMAL_ID]
          


### Change mr2009a31 in year 2013 to "On" instead of "Off"
MR2013$CalvingGround[MR2013$ANIMAL_ID=="mr2009a31"] <- "On"

# Maria--MCP 50 
#2009

ls.yrs09 <- data.table(mcps = list(MR2009_05_mcp50, MR2009_06_mcp50, MR2009_07_mcp50),
                     centroids = list(MR2009_05, MR2009_06, MR2009_07))

##Maria-New Monthly Data 
ls.plots09 <- lapply(1:nrow(ls.yrs09), FUN = function(yr){
  g = ggplot() + 
    #geom_polygon(aes(long,lat, fill = as.factor(id),
    #alpha = 0.2), 
    #data=fortify(ls.yrs[yr,mcps][[1]])) +
    geom_point(aes(centX2, centY2, colour = CalvingGround), data = ls.yrs09[yr,centroids][[1]])+
    #geom_text(aes(centX, centY, label=ANIMAL_ID),hjust=0, vjust=0, data = ls.yrs[yr,centroids][[1]])+
    ylab('NORTHING') + 
    xlab('EASTING') +
    xlim(610000,690000)+
    ylim(5300000,5380000)+
    theme(legend.position = 'none',
          plot.title = element_text(size = 10, face = "bold"),
          axis.text=element_text(size=10,face = "bold", color = "black"),
          axis.title=element_text(size=10,face = "bold"),
          strip.text = element_text(size=10,face = "bold"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
})

do.call(grid.arrange, c(ls.plots09, nrow = 3, ncol = 2))


### make the voronoi polygons
##Maria
SpatialPointsDataFrame(unique(MR2009_05[, .(centX2, centY2, ANIMAL_ID)])[, .(centX2, centY2)],
                       data=unique(MR2009_05[, .(centX2, centY2, ANIMAL_ID)])[, .(ANIMAL_ID)])

##Maria 
library(deldir)
ls.MR09 <- list(MR2009_05, MR2009_06, MR2009_07)

ls.voronoi09 <- lapply(1:length(ls.MR09),FUN = function(dt){
  # ls.MR[[dt]]
  d09 <- unique(ls.MR09[[dt]][,.(centX2, centY2, ANIMAL_ID)])
  s09 <- SpatialPointsDataFrame(d09[,.(centX2, centY2)], data = d09[,.(ANIMAL_ID)])
  voronoi(s09)
}) 

noY <- c(2,4)
noX <-c(1,2)
labels=list("a","b","c","d","e")
ls.plots.voronoi09 <- lapply(1:length(ls.MR09), FUN = function(yr){
  g = ggplot() + 
    geom_polygon(aes(long,lat, group=group), colour="black", fill= NA, size=0.25, 
                 data=fortify(ls.voronoi09[[yr]])) +
    geom_point(aes(centX2, centY2, colour = CalvingGround), data = ls.MR09[[yr]])+
    #geom_text(aes(600000,5390000),label=labels[[yr]])+
    ylab(NULL) + 
    xlab(NULL) +
   # xlim(600000,690000)+
   # ylim(5200000,5390000)+
    theme(legend.position = "bottom",
          plot.title = element_text(size = 10),
          axis.text=element_text(size=10, color = "black"),
          axis.title=element_text(size=10),
          strip.text = element_text(size=10),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, size=0.5))
  
  g
})


do.call(grid.arrange, c(ls.plots.voronoi09, nrow = 2, ncol = 2, left="Northing", bottom="Easting"))


### Get area of the polygons into attribute table for spatial dataframe
#Also add year
ls.mn09 <- list("05","06","07")

# Now put the values in the @data slot of each spatial data frame
ls.voronoi09.1 <- lapply(1:length(ls.voronoi09), FUN = function(x){
  ls.voronoi09[[x]]@data$Month <- ls.mn09[[x]]
  ls.voronoi09[[x]]@data$Area <- area(ls.voronoi09[[x]])
  ls.voronoi09[[x]]
})

# Take the dataframes out
voronoi.data<-lapply(1:length(ls.voronoi09.1), FUN = function(x){
  data.table(ls.voronoi09.1[[x]]@data)
})

voronoi.data <- do.call(rbind, voronoi.data)
dev.off()

voronoi.data$Year <- 2009
voronoi.data$IDyear <- paste(voronoi.data$ANIMAL_ID, voronoi.data$Year, sep = "_")
voronoi.data$ID.yr.mn <- paste(voronoi.data$IDyear, voronoi.data$Month, sep = "_")



#2010

ls.yrs10 <- data.table(mcps = list(MR2010_05_mcp50, MR2010_06_mcp50),
                      centroids = list(MR2010_05, MR2010_06 ))

##Maria-New Monthly Data 
ls.plots10 <- lapply(1:nrow(ls.yrs10), FUN = function(yr){
  g = ggplot() + 
    #geom_polygon(aes(long,lat, fill = as.factor(id),
    #alpha = 0.2), 
    #data=fortify(ls.yrs[yr,mcps][[1]])) +
    geom_point(aes(centX2, centY2, colour = CalvingGround), data = ls.yrs10[yr,centroids][[1]])+
    #geom_text(aes(centX, centY, label=ANIMAL_ID),hjust=0, vjust=0, data = ls.yrs[yr,centroids][[1]])+
    ylab('NORTHING') + 
    xlab('EASTING') +
    xlim(610000,690000)+
    ylim(5300000,5380000)+
    theme(legend.position = 'none',
          plot.title = element_text(size = 10, face = "bold"),
          axis.text=element_text(size=10,face = "bold", color = "black"),
          axis.title=element_text(size=10,face = "bold"),
          strip.text = element_text(size=10,face = "bold"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
})

do.call(grid.arrange, c(ls.plots10, nrow = 3, ncol = 2))

### make the voronoi polygons
##Maria
SpatialPointsDataFrame(unique(MR2010_05[, .(centX2, centY2, ANIMAL_ID)])[, .(centX2, centY2)],
                       data=unique(MR2010_05[, .(centX2, centY2, ANIMAL_ID)])[, .(ANIMAL_ID)])

##Maria 
ls.MR10 <- list(MR2010_05, MR2010_06)

ls.voronoi10 <- lapply(1:length(ls.MR10),FUN = function(dt){
  # ls.MR[[dt]]
  d10 <- unique(ls.MR10[[dt]][,.(centX2, centY2, ANIMAL_ID)])
  s10 <- SpatialPointsDataFrame(d10[,.(centX2, centY2)], data = d10[,.(ANIMAL_ID)])
  voronoi(s10)
}) 

noY <- c(2,4)
noX <-c(1,2)
labels=list("a","b","c","d","e")
ls.plots.voronoi10 <- lapply(1:length(ls.MR10), FUN = function(yr){
  g = ggplot() + 
    geom_polygon(aes(long,lat, group=group), colour="black", fill=NA, size=0.25, 
                 data=fortify(ls.voronoi10[[yr]])) +
    geom_point(aes(centX2, centY2, colour = CalvingGround), data = ls.MR10[[yr]])+
    #geom_text(aes(600000,5390000),label=labels[[yr]])+
    ylab(NULL) + 
    xlab(NULL) +
   # xlim(610000,690000)+
   # ylim(5200000,5390000)+
    theme(legend.position = 'none',
          plot.title = element_text(size = 10),
          axis.text=element_text(size=10, color = "black"),
          axis.title=element_text(size=10),
          strip.text = element_text(size=10),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
  g
})

do.call(grid.arrange, c(ls.plots.voronoi10, nrow = 2, ncol = 1, left="Northing", bottom="Easting"))

ls.mn10 <- list("05","06")

ls.voronoi10.1 <- lapply(1:length(ls.voronoi10), FUN = function(x){
  ls.voronoi10[[x]]@data$Month <- ls.mn10[[x]]
  ls.voronoi10[[x]]@data$Area <- area(ls.voronoi10[[x]])
  ls.voronoi10[[x]]
})

# Take the dataframes out
voronoi.data10<-lapply(1:length(ls.voronoi10.1), FUN = function(x){
  data.table(ls.voronoi10.1[[x]]@data)
})

voronoi.data1<- do.call(rbind, voronoi.data10)
dev.off()

voronoi.data1$Year <- 2010
voronoi.data1$IDyear <- paste(voronoi.data1$ANIMAL_ID, voronoi.data1$Year, sep = "_")
voronoi.data1$ID.yr.mn <- paste(voronoi.data1$IDyear, voronoi.data1$Month, sep = "_")


#2011

ls.yrs11 <- data.table(mcps = list(MR2011_05_mcp50, MR2011_06_mcp50, MR2011_07_mcp50),
                       centroids = list(MR2011_05, MR2011_06, MR2011_07))

##Maria-New Monthly Data 
ls.plots11 <- lapply(1:nrow(ls.yrs11), FUN = function(yr){
  g = ggplot() + 
    #geom_polygon(aes(long,lat, fill = as.factor(id),
    #alpha = 0.2), 
    #data=fortify(ls.yrs[yr,mcps][[1]])) +
    geom_point(aes(centX2, centY2, colour = CalvingGround), data = ls.yrs11[yr,centroids][[1]])+
    #geom_text(aes(centX, centY, label=ANIMAL_ID),hjust=0, vjust=0, data = ls.yrs[yr,centroids][[1]])+
    ylab('NORTHING') + 
    xlab('EASTING') +
    xlim(610000,690000)+
    ylim(5300000,5380000)+
    theme(legend.position = 'none',
          plot.title = element_text(size = 10, face = "bold"),
          axis.text=element_text(size=10,face = "bold", color = "black"),
          axis.title=element_text(size=10,face = "bold"),
          strip.text = element_text(size=10,face = "bold"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
})

do.call(grid.arrange, c(ls.plots11, nrow = 3, ncol = 2))

### make the voronoi polygons
##Maria
SpatialPointsDataFrame(unique(MR2011_05[, .(centX2, centY2, ANIMAL_ID)])[, .(centX2, centY2)],
                       data=unique(MR2011_05[, .(centX2, centY2, ANIMAL_ID)])[, .(ANIMAL_ID)])

##Maria 
ls.MR11 <- list(MR2011_05, MR2011_06, MR2011_07)

ls.voronoi11 <- lapply(1:length(ls.MR11),FUN = function(dt){
  # ls.MR[[dt]]
  d11 <- unique(ls.MR11[[dt]][,.(centX2, centY2, ANIMAL_ID)])
  s11 <- SpatialPointsDataFrame(d11[,.(centX2, centY2)], data = d11[,.(ANIMAL_ID)])
  voronoi(s11)
}) 

noY <- c(2,4)
noX <-c(1,2)
labels=list("a","b","c","d","e")
ls.plots.voronoi11 <- lapply(1:length(ls.MR11), FUN = function(yr){
  g = ggplot() + 
    geom_polygon(aes(long,lat, group=group), colour="black", fill=NA, size=0.25, 
                 data=fortify(ls.voronoi11[[yr]])) +
    geom_point(aes(centX2, centY2, colour = CalvingGround), data = ls.MR11[[yr]])+
    #geom_text(aes(600000,5390000),label=labels[[yr]])+
    ylab(NULL) + 
    xlab(NULL) +
   # xlim(610000,690000)+
   # ylim(5200000,5390000)+
    theme(legend.position = 'none',
          plot.title = element_text(size = 10),
          axis.text=element_text(size=10, color = "black"),
          axis.title=element_text(size=10),
          strip.text = element_text(size=10),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
  g
})

do.call(grid.arrange, c(ls.plots.voronoi11, nrow = 2, ncol = 2, left="Northing", bottom="Easting"))

ls.mn11 <- list("05","06","07")

ls.voronoi11.1 <- lapply(1:length(ls.voronoi11), FUN = function(x){
  ls.voronoi11[[x]]@data$Month <- ls.mn11[[x]]
  ls.voronoi11[[x]]@data$Area <- area(ls.voronoi11[[x]])
  ls.voronoi11[[x]]
})

# Take the dataframes out
voronoi.data11<-lapply(1:length(ls.voronoi11.1), FUN = function(x){
  data.table(ls.voronoi11.1[[x]]@data)
})

voronoi.data11 <- do.call(rbind, voronoi.data11)
dev.off()

voronoi.data11$Year <- 2011
voronoi.data11$IDyear <- paste(voronoi.data11$ANIMAL_ID, voronoi.data11$Year, sep = "_")
voronoi.data11$ID.yr.mn <- paste(voronoi.data11$IDyear, voronoi.data11$Month, sep = "_")

#2012

ls.yrs12 <- data.table(mcps = list(MR2012_05_mcp50, MR2012_06_mcp50, MR2012_07_mcp50),
                       centroids = list(MR2012_05, MR2012_06, MR2012_07))

##Maria-New Monthly Data 
ls.plots12 <- lapply(1:nrow(ls.yrs12), FUN = function(yr){
  g = ggplot() + 
    #geom_polygon(aes(long,lat, fill = as.factor(id),
    #alpha = 0.2), 
    #data=fortify(ls.yrs[yr,mcps][[1]])) +
    geom_point(aes(centX2, centY2, colour = CalvingGround), data = ls.yrs12[yr,centroids][[1]])+
    #geom_text(aes(centX, centY, label=ANIMAL_ID),hjust=0, vjust=0, data = ls.yrs[yr,centroids][[1]])+
    ylab('NORTHING') + 
    xlab('EASTING') +
    xlim(610000,690000)+
    ylim(5300000,5380000)+
    theme(legend.position = 'none',
          plot.title = element_text(size = 10, face = "bold"),
          axis.text=element_text(size=10,face = "bold", color = "black"),
          axis.title=element_text(size=10,face = "bold"),
          strip.text = element_text(size=10,face = "bold"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
})

do.call(grid.arrange, c(ls.plots12, nrow = 3, ncol = 2))

### make the voronoi polygons
##Maria
SpatialPointsDataFrame(unique(MR2012_05[, .(centX2, centY2, ANIMAL_ID)])[, .(centX2, centY2)],
                       data=unique(MR2012_05[, .(centX2, centY2, ANIMAL_ID)])[, .(ANIMAL_ID)])

##Maria 
ls.MR12 <- list(MR2012_05, MR2012_06, MR2012_07)

ls.voronoi12 <- lapply(1:length(ls.MR12),FUN = function(dt){
  # ls.MR[[dt]]
  d12 <- unique(ls.MR12[[dt]][,.(centX2, centY2, ANIMAL_ID)])
  s12 <- SpatialPointsDataFrame(d12[,.(centX2, centY2)], data = d12[,.(ANIMAL_ID)])
  voronoi(s12)
}) 

noY <- c(2,4)
noX <-c(1,2)
labels=list("a","b","c","d","e")
ls.plots.voronoi12 <- lapply(1:length(ls.MR12), FUN = function(yr){
  g = ggplot() + 
    geom_polygon(aes(long,lat, group=group), colour="black", fill=NA, size=0.25, 
                 data=fortify(ls.voronoi12[[yr]])) +
    geom_point(aes(centX2, centY2, colour = CalvingGround), data = ls.MR12[[yr]])+
    #geom_text(aes(600000,5390000),label=labels[[yr]])+
    ylab(NULL) + 
    xlab(NULL) +
   # xlim(610000,690000)+
   # ylim(5200000,5390000)+
    theme(legend.position = 'none',
          plot.title = element_text(size = 10),
          axis.text=element_text(size=10, color = "black"),
          axis.title=element_text(size=10),
          strip.text = element_text(size=10),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
  g
})

do.call(grid.arrange, c(ls.plots.voronoi12, nrow = 2, ncol = 2, left="Northing", bottom="Easting"))

ls.mn12 <- list("05","06","07")

ls.voronoi12.1 <- lapply(1:length(ls.voronoi12), FUN = function(x){
  ls.voronoi12[[x]]@data$Month <- ls.mn12[[x]]
  ls.voronoi12[[x]]@data$Area <- area(ls.voronoi12[[x]])
  ls.voronoi12[[x]]
})

# Take the dataframes out
voronoi.data12<-lapply(1:length(ls.voronoi12.1), FUN = function(x){
  data.table(ls.voronoi12.1[[x]]@data)
})

voronoi.data12 <- do.call(rbind, voronoi.data12)
dev.off()


voronoi.data12$Year <- 2012
voronoi.data12$IDyear <- paste(voronoi.data12$ANIMAL_ID, voronoi.data12$Year, sep = "_")

voronoi.data12$ID.yr.mn <- paste(voronoi.data12$IDyear, voronoi.data12$Month, sep = "_")

NNnew$Area <- voronoi.data12$Area[match(NNnew$ID.yr.mn, voronoi.data12$ID.yr.mn)]


#2013

ls.yrs13 <- data.table(mcps = list(MR2013_05_mcp50, MR2013_06_mcp50, MR2013_07_mcp50),
                       centroids = list(MR2013_05, MR2013_06, MR2013_07))

##Maria-New Monthly Data 
ls.plots13 <- lapply(1:nrow(ls.yrs13), FUN = function(yr){
  g = ggplot() + 
    #geom_polygon(aes(long,lat, fill = as.factor(id),
    #alpha = 0.2), 
    #data=fortify(ls.yrs[yr,mcps][[1]])) +
    geom_point(aes(centX2, centY2, colour = CalvingGround), data = ls.yrs13[yr,centroids][[1]])+
    #geom_text(aes(centX, centY, label=ANIMAL_ID),hjust=0, vjust=0, data = ls.yrs[yr,centroids][[1]])+
    ylab('NORTHING') + 
    xlab('EASTING') +
    xlim(610000,690000)+
    ylim(5300000,5380000)+
    theme(legend.position = 'none',
          plot.title = element_text(size = 10, face = "bold"),
          axis.text=element_text(size=10,face = "bold", color = "black"),
          axis.title=element_text(size=10,face = "bold"),
          strip.text = element_text(size=10,face = "bold"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
})

do.call(grid.arrange, c(ls.plots13, nrow = 3, ncol = 2))

### make the voronoi polygons
##Maria
SpatialPointsDataFrame(unique(MR2013_05[, .(centX2, centY2, ANIMAL_ID)])[, .(centX2, centY2)],
                       data=unique(MR2013_05[, .(centX2, centY2, ANIMAL_ID)])[, .(ANIMAL_ID)])

##Maria 
ls.MR13 <- list(MR2013_05, MR2013_06, MR2013_07)

ls.voronoi13 <- lapply(1:length(ls.MR13),FUN = function(dt){
  # ls.MR[[dt]]
  d13 <- unique(ls.MR13[[dt]][,.(centX2, centY2, ANIMAL_ID)])
  s13 <- SpatialPointsDataFrame(d13[,.(centX2, centY2)], data = d13[,.(ANIMAL_ID)])
  voronoi(s13)
}) 

noY <- c(2,4)
noX <-c(1,2)
labels=list("a","b","c","d","e")
ls.plots.voronoi13 <- lapply(1:length(ls.MR13), FUN = function(yr){
  g = ggplot() + 
    geom_polygon(aes(long,lat, group=group), colour="black", fill=NA, size=0.25, 
                 data=fortify(ls.voronoi13[[yr]])) +
    geom_point(aes(centX2, centY2, colour = CalvingGround), data = ls.MR13[[yr]])+
    #geom_text(aes(600000,5390000),label=labels[[yr]])+
    ylab(NULL) + 
    xlab(NULL) +
   # xlim(610000,690000)+
  #  ylim(5200000,5390000)+
    theme(legend.position = 'none',
          plot.title = element_text(size = 10),
          axis.text=element_text(size=10, color = "black"),
          axis.title=element_text(size=10),
          strip.text = element_text(size=10),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
  g
})

do.call(grid.arrange, c(ls.plots.voronoi13, nrow = 2, ncol = 2, left="Northing", bottom="Easting"))

ls.mn13 <- list("05","06","07")

ls.voronoi13.1 <- lapply(1:length(ls.voronoi13), FUN = function(x){
  ls.voronoi13[[x]]@data$Month <- ls.mn13[[x]]
  ls.voronoi13[[x]]@data$Area <- area(ls.voronoi13[[x]])
  ls.voronoi13[[x]]
})

# Take the dataframes out
voronoi.data13<-lapply(1:length(ls.voronoi13.1), FUN = function(x){
  data.table(ls.voronoi13.1[[x]]@data)
})

voronoi.data13 <- do.call(rbind, voronoi.data13)
dev.off()

voronoi.data13$Year <- 2013
voronoi.data13$IDyear <- paste(voronoi.data13$ANIMAL_ID, voronoi.data13$Year, sep = "_")
voronoi.data13$ID.yr.mn <- paste(voronoi.data13$IDyear, voronoi.data13$Month, sep = "_")

NNnew <- raw_all_NNdist2
NNnew <- separate(NNnew, "FIX_DATE", c("Year", "Month", "Day"), sep = "-")
NNnew$ID.yr.mn <- paste(NNnew$IDyear, NNnew$Month, sep = "_")

mydata <- rbind(voronoi.data13, voronoi.data12)
mydata <- rbind(mydata, voronoi.data)
mydata <- rbind(mydata, voronoi.data1)
mydata <- rbind(mydata, voronoi.data11)

NNnew$Area <- mydata$Area[match(NNnew$ID.yr.mn, mydata$ID.yr.mn)]

NNnew$MeanNN <- with(NNnew, ave(NNdist, ANIMAL_ID, Month, Year, FUN = mean))
View(NNnew)

ggplot(NNnew) + 
  geom_point(shape=18, aes(x=MeanNN, y=Area, colour = CalvingGround)) +
  geom_smooth(aes(x = MeanNN, y = Area), method=lm, se= FALSE)
  

ggplot(NNnew) +
  geom_point(aes(x = MeanNN, y = Area, colour = CalvingGround)) +
  stat_smooth(method = "lm", formula = y ~ x, se = TRUE, aes(x = MeanNN, y = Area))

lm <- lm(Area ~ MeanNN, data = NNnew)
summary(lm)
NNnew <- NNnew[ , !duplicated(colnames(NNnew))]

NNnew$Year <- as.factor(NNnew$Year)
library(rptR)

AREAMOD <- rpt(Area ~ Year + (1|ANIMAL_ID), data = NNnew, grname = c("ANIMAL_ID"))
summary(AREAMOD)
plot(AREAMOD)

AREAMOD1 <- rpt(Area ~ Year + Month + (1|ANIMAL_ID), data = NNnew, grname = c("ANIMAL_ID"))
summary(AREAMOD1)
plot(AREAMOD1)

AREAMOD2 <- rpt(Area ~ Month + (1|ANIMAL_ID), data = NNnew, grname = c("ANIMAL_ID"))
summary(AREAMOD2)

AREAMOD3 <- rpt(Area ~ (1|ID.yr.mn), data = NNnew, grname = c("ID.yr.mn"))
summary(AREAMOD3)
