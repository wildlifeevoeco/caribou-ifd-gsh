

### Packages ----
libs <- c('data.table', 'sf', 'lubridate',
          'gridExtra', 'ggplot2', 'rgdal',
          'sp', 'adehabitatHR', 'spatsoc')
lapply(libs, require, character.only = TRUE)

## Load group size data
obs <- readRDS("data/group-size-spatial.RDS")
setDT(obs)[, roundtime := round_date(datetime, unit = "hours")]

obs2 <- obs[,c("group.size","longitude", "latitude", "roundtime")]
colnames(obs2) <- c("group.size", "X_COORD", "Y_COORD", "datetime")
obs2$type <- "group"



## Load collar data
DT <- fread("data/raw_all_NNdist.csv")
DT <- DT[Year == "2012"]
DT[, datetime := ymd_hms(paste(FIX_DATE, FIX_TIME))]
DT[, datetime := round_date(datetime, unit = "hours")]
DT$ID <- as.factor(paste(DT$ANIMAL_ID, DT$CalvingGround,sep = "_"))
group_times(DT, datetime = "datetime", threshold = "2 hour")

qq <- edge_dist(DT = DT, id = 'ANIMAL_ID', coords = c('EASTING', 'NORTHING'),
          timegroup = 'timegroup', threshold = 500000, returnDist = TRUE, 
          fillNA = FALSE,
          splitBy = c("Year"))
#DT[, meanDist := mean(NNdist), by = c("ANIMAL_ID")]

DT2 <- DT[, c("ID", "X_COORD", "Y_COORD", "NNdist","datetime")]
DT2$type <- "GPS"

## Merge files
a1 <- rbind(obs2, DT2, fill = TRUE)

#Calculate easting/northing
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
a1[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]


group_times(a1, datetime = "datetime", threshold = "12 hour")

group_pts(a1, threshold = 1000,
          timegroup = 'timegroup',
          id = "type", coords = c('EASTING', 'NORTHING'))

a2 <- a1[!is.na(group.size)]
a3 <- a1[!is.na(NNdist)]


gr <- a2[a2$group %in% a3$group,]
nn <- a3[a3$group %in% a2$group,]

gr2 <- gr[, c("group.size", "group")]
gr2 <- gr2[, mean(group.size), by = "group"]
colnames(gr2)[2] <- "avgGs"
nn2 <- nn[, c("ID", "NNdist" ,"group")]
nn2 <- nn2[, mean(NNdist), by = c("ID", "group")]
colnames(nn2)[3] <- "dist"

a4 <- merge(gr2, nn2, by = "group")

ggplot(a4, aes(avgGs, dist)) +
  geom_jitter(aes(color = ID)) +
  geom_smooth(method = "lm")
