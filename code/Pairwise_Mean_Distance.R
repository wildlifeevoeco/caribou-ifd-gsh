### Pairwise Mean Distance  ----
# Author: Alec Robitaille
# Purpose: To determine pairwise distance in space+time
# Created on: March 22 2017
# Last updated: April 1 2017 
# Inputs: Collar data (x coor, y coor, individual id, etc)
# Outputs: Mean pairwise distance for each individual at each time loc
# License: /LICENSE.md 

### Packages ----
libs <- c("data.table", "ggplot2", 
          "sp", "rgeos", "geosphere",
          "gridExtra", "rdist", "cowplot")
lapply(libs, require, character.only = TRUE)

# library(microbenchmark)

### Input data ----
carlocs <- fread("input/caribou data 2009.csv")
Mrlocs <- fread("~/Dropbox/Vander Wal Lab/Ch2data/MRCaribouFemales.csv")


aa <- Mrlocs[, mean(meanDistance), by = c("ANIMAL_ID", "Year")]

ggplot(aa[V1 < 20000]) +
  geom_histogram(aes(V1)) +
  facet_wrap(~Year)

#Mrlocsall$Year <- substr(Mrlocs$FIX_DATE, 1,4)
#Mrlocsall$MonthDay <- substr(Mrlocs$FIX_DATE, 6,10)

#sublocs <- Mrlocsall[Mrlocsall$MonthDay >= "05-20" & Mrlocsall$MonthDay <= "07-15",]
Mrlocs <- Mrlocs[,. (ANIMAL_ID, FIX_DATE, FIX_TIME, EASTING, NORTHING, X_COORD, Y_COORD, Year)]

### Set variables ----
date.col <- "FIX_DATE"
time.col <- "FIX_TIME"
id.col <- "ANIMAL_ID"
east.col <- "EASTING"
north.col <- "NORTHING"
x.col <- "X_COORD"
y.col <- "Y_COORD"

### Prep data ----
# Create an ITime object [data.table]
# locs[, idate := as.IDate(get(date.col))]
# locs[, itime := as.ITime(get(time.col))]

Mrlocs[, idate := as.IDate(get(date.col))]
Mrlocs[, itime := as.ITime(get(time.col))]

# Return only the hour
# locs[, hour := hour(itime)]

Mrlocs[, hour := hour(itime)]

# Make time groups by rounded hour and date
# locs[, timeGroup := .GRP, by = .(hour, idate)]

Mrlocs[, timeGroup := .GRP, by = .(hour, idate)]

### Check for even hours ----
# ?MB: do we want to include locs that are off of all others by 30 or 45 minutes?
 if(any(locs[, hour %% 2 != 0])){
  warning("some of the locs are below the 2-hour interval (eg 1h59min)")
}
locs[hour %% 2 != 0]

if(any(Mrlocs[, hour %% 2 != 0])){
  warning("some of the locs are below the 2-hour interval (eg 1h59min)")
}
Mrlocs[hour %% 2 != 0]

### Sub the data ----
# locs <- locs[hour %% 2 == 0] #& 
             # get(date.col) == '2009-05-24']

Mrlocs <- Mrlocs[hour %% 2 == 0] #& 
# get(date.col) == '2009-05-24']
summary(Mrlocs)
### Functions ----
# Apply dist by time step
MeanPairwiseDists <- function(in.dt) {
  names <- in.dt[, get(id.col)]
  dst.mtrx <- as.matrix(dist(in.dt[, .(get(east.col), get(north.col))],
                             method = "euclidean",
                             diag = TRUE,
                             upper = TRUE))
  list(meanDistance = colMeans(dst.mtrx),
       id = names)
  return(dst.mtrx)
}

edge_nn()

# Apply spDists [sp] by time step
MeanDistsSp <- function(in.dt) {
  # Pull out the names from each subbed dt
  names <- in.dt[, get(id.col)]
  # Create the distance matrix with spDists
  dst.mtrx <- spDists(as.matrix(in.dt[ , .(get(east.col), get(north.col))]), 
                      longlat = FALSE)
  # Output the column means (average pairwise dist) + names
  list(meanDistance = colMeans(dst.mtrx),
       id = names)
}

# Apply distm with distGeo [geosphere] by time step
MeanDistsGeo <- function(in.dt) {
  names <- in.dt[, get(id.col)]
  # Swap out distGeo, distVincentyEllipsoid, or others in "fun = "
  dst.mtrx <- distm(as.matrix(in.dt[ , .(get(x.col), get(y.col))]), 
                    fun = distGeo)
  list(meanDistance = colMeans(dst.mtrx),
       id = names)
}

# Pairwise distance matrix with [rdist]'s pdist
MeanRdist <- function(in.dt) {
  names <- in.dt[, get(id.col)]
  # Swap out distGeo, distVincentyEllipsoid, or others in "fun = "
  dst.mtrx <- pdist(as.matrix(in.dt[ , .(get(east.col), get(north.col))]), 
                    metric = "euclidean")
  list(meanDistance = colMeans(dst.mtrx),
       id = names)
}

### Processing ----
#TODO(alec): rm the extra dist ids
# m <- microbenchmark(

# Fastest.
# locs[, c("meanDistance", "distID") :=
#        MeanPairwiseDists(.SD), by = timeGroup,
#      .SDcols = c(id.col, east.col, north.col)]

Mrlocs[, c("meanDistance", "distID") :=
       MeanPairwiseDists(.SD), by = timeGroup,
     .SDcols = c(id.col, east.col, north.col)]

mat<-Mrlocs[,
         MeanPairwiseDists(.SD), by = timeGroup,
       .SDcols = c(id.col, east.col, north.col)]



# Slower.
# locs[, c("meanDistanceSp", "distIDSp") := 
#        MeanDistsSp(.SD), by = timeGroup,
#      .SDcols = c(id.col, east.col, north.col)]

# Mrlocs[, c("meanDistanceSp", "distIDSp") := 
#        MeanDistsSp(.SD), by = timeGroup,
#      .SDcols = c(id.col, east.col, north.col)]
# 
# # Slowest. distVincentyEllipsoid is slower than distHaversine. 
# locs[, c("meanDistanceGeo", "distIDGeo") := 
#        MeanDistsGeo(.SD), by = timeGroup,
#      .SDcols = c(id.col, x.col, y.col)]
# 
# Mrlocs[, c("meanDistanceGeo", "distIDGeo") := 
#        MeanDistsGeo(.SD), by = timeGroup,
#      .SDcols = c(id.col, x.col, y.col)]

# Faster?
# locs[, c("meanDistanceRdist", "distIDRdist") := 
#        MeanRdist(.SD), by = timeGroup,
#      .SDcols = c(id.col, east.col, north.col)]

## getting the daily average and log daily average mean distance
library(plyr)
avglocs <- ddply(Mrlocs, .(distID, idate), summarize, val = mean(meanDistance))
avglocs$distID <- as.factor(avglocs$distID)
avglocs$logval <- log(avglocs$val)
avglocs$Year <- substr(avglocs$idate, 1,4)

#Remove indidivuals that are outliers or missing values
avglocs <- avglocs[!(avglocs$distID == "mr2009a27" & avglocs$Year == "2011"),]
avglocs <- avglocs[!(avglocs$distID == "mr2009a14" & avglocs$Year == "2011"),]
avglocs <- avglocs[!(avglocs$distID == "mr2013a15" & avglocs$Year == "2013"),]

avglocs$val2 <- avglocs$val/1000

## Yearly average
yravglocs <- ddply(Mrlocs, .(distID, Year), summarize, val = mean(meanDistance))
yravglocs2009 <- yravglocs[yravglocs$Year == "2009",]
yravglocs2010 <- yravglocs[yravglocs$Year == "2010",]
yravglocs2011 <- yravglocs[yravglocs$Year == "2011",]
yravglocs2011 <- yravglocs2011[!yravglocs2011$distID == "mr2009a27",]
yravglocs2011 <- yravglocs2011[!yravglocs2011$distID == "mr2009a14",]
yravglocs2012 <- yravglocs[yravglocs$Year == "2012",]
yravglocs2013 <- yravglocs[yravglocs$Year == "2013",]
yravglocs2013 <- yravglocs2013[!yravglocs2013$distID == "mr2013a15",]

yravglocs2009 <- yravglocs2009[order(yravglocs2009$val),]
yravglocs2009$idrank <- seq(1:nrow(yravglocs2009))
yravglocs2009$valkm <- yravglocs2009$val/1000

yravglocs2010 <- yravglocs2010[order(yravglocs2010$val),]
yravglocs2010$idrank <- seq(1:nrow(yravglocs2010))
yravglocs2010$valkm <- yravglocs2010$val/1000

yravglocs2011 <- yravglocs2011[order(yravglocs2011$val),]
yravglocs2011$idrank <- seq(1:nrow(yravglocs2011))
yravglocs2011$valkm <- yravglocs2011$val/1000

yravglocs2012 <- yravglocs2012[order(yravglocs2012$val),]
yravglocs2012$idrank <- seq(1:nrow(yravglocs2012))
yravglocs2012$valkm <- yravglocs2012$val/1000

yravglocs2013 <- yravglocs2013[order(yravglocs2013$val),]
yravglocs2013$idrank <- seq(1:nrow(yravglocs2013))
yravglocs2013$valkm <- yravglocs2013$val/1000

yravglocs_ranked <- rbind(yravglocs2009, yravglocs2010, yravglocs2011, yravglocs2012, yravglocs2013)
yravglocs_ranked2 <- rbind(yravglocs2009, yravglocs2010, yravglocs2011, yravglocs2012, yravglocs2013)
yravglocs_ranked2$val2 <- yravglocs_ranked2$val/1000
yravglocs_ranked2$fit2 <- yravglocs_ranked2$fit/1000

write.csv(yravglocs_ranked2, '~/Dropbox/Vander Wal Lab/CH2data/yravglocs_ranked.csv')
# plot in ascending order
ggplot(yravglocs_ranked) + geom_point(aes(x=ID,y=NNdist, colour=factor(ID)), size = 1) + 
  facet_grid(~Year, scales = "free_x") + theme(legend.position = "none")


### Plotting ----
# autoplot(m)
# plot(m)

ggplot(locs) + geom_histogram(aes(meanDistance)) + 
  labs(title = "Mean Pairwise Distance Using Dist")

## Plotted distances across time coloured by ID
# ggplot(locs) + geom_point(aes(x=EASTING,y=NORTHING, col=distID), size = 1)
# 
# ggplot(locs) + geom_point(aes(x=idate,y=meanDistance, col=distID), size = 1) + 
#   labs(title = "Mean Pairwise Distance Using Dist")
# 
avglocs$idate2 <- as.Date(avglocs$idate)
pdf("/Users/maegwinbonar/Desktop/Vander Wal Lab/CH2data/NNdist1.pdf",width = 11.02, height = 4.33)
ggplot(avglocs) + geom_line(aes(x=idate2,y=val, col=distID), size = 1) +
  labs(title = "") + ylab("Daily avg NN distance (km)")+
  xlab("Date of calving season")+
  #geom_point(aes(x=CalvingDate, y=NNpart), data = yravglocs_ranked)+
  facet_grid(~Year, scales = "free_x") + 
  theme(legend.position = "none",
        axis.text.x=element_text(size=20, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 20, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 24),
        axis.title.y = element_text(size = 22),
        strip.text.x = element_text(size = 24),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_x_date(date_breaks = "1 month", date_labels = "%B")
dev.off()

pdf("/Users/maegwinbonar/Desktop/Vander Wal Lab/CH2data/NNdist2.pdf",width = 11.02, height = 4.33)
ggplot(yravglocs_ranked) + geom_point(aes(x=idrank,y=val, colour=factor(distID)), size = 5) + 
  ylab("Season avg NN distance (km)")+
  xlab("Individual")+
  geom_line(aes(x = idrank, y = fit, group = idrank < breakpoint), color = 'black', alpha = 0.75, size = 1.2)+
  facet_grid(~Year, scales = "free_x") + theme(legend.position = "none")+
  theme(legend.position = "none",
        axis.text.x=element_text(size=20, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 20, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 24),
        axis.title.y = element_text(size = 22),
        strip.text.x = element_text(size = 24),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_x_continuous(breaks = c(5,10,15,20))+
  scale_y_continuous(breaks = c(20, 40, 60))
  
dev.off()

str(avglocs)
for (var in unique(avglocs$Year)) {
  dev.new()
  print(ggplot(avglocs[avglocs$Year==var,]) + 
    geom_line(aes(x=idate,y=val, col=distID), size = 1) +
    labs(title = paste(var, "Mean Pairwise Distance Using Dist", sep = " ")) +
    geom_point(aes(x=CalvingDate, y=NNpart), data = yravglocs_ranked[yravglocs_ranked$Year==var,])+
    geom_point(aes(x=LossDate, y=NNpart, col=distID), data = yravglocs_ranked[yravglocs_ranked$Year==var,], colour = "red")+
    theme(legend.position = "none"))
}

ggplot(avglocs) + geom_line(aes(x=idate,y=logval, col=distID), size = 1) +
  labs(title = "Mean Pairwise Distance Using Dist")

## back to plotting the mean distances
ggplot(locs) + geom_histogram(aes(meanDistanceSp)) + 
  labs(title = "Mean Pairwise Distance Using SpDist")

ggplot(locs) + geom_histogram(aes(meanDistanceGeo)) + 
  labs(title = "Mean Pairwise Distance Using Geosphere")

# Difference in mean by pairwise comparison
(mDS <- ggplot(locs) +
   geom_histogram(aes(meanDistance - meanDistanceSp)))

(mDG <- ggplot(locs) +
    geom_histogram(aes(meanDistance - meanDistanceGeo)))

(mGS <- ggplot(locs) +
   geom_histogram(aes(meanDistanceSp - meanDistanceGeo)))

(mDR <- ggplot(locs) +
    geom_histogram(aes(meanDistance - meanDistanceRdist)))

(mGR <- ggplot(locs) +
    geom_histogram(aes(meanDistanceGeo - meanDistanceRdist)))

(mSR <- ggplot(locs) +
    geom_histogram(aes(meanDistanceSp - meanDistanceRdist)))

grid.arrange(grobs = list(mDS, mDG, mGS,
                          mDR, mGR, mSR))

## Broken stick regression to find on and off the calving ground
## Mean yearly values first
# breaks <- yravglocs$idrank[which(yravglocs$idrank >= 10 & yravglocs$idrank <= 17)] ## Choose the potenial range where the break might occur
# 
# ### now we iteratively search these breakpoints for the model that has the
# ### lowest residual mean squared error (MSE), using that as our criteria for the best model.
# mse <- numeric(length(breaks))
# for(i in 1:length(breaks)){
#   piecewise1 <- lm(yravglocs$val ~ yravglocs$idrank*(yravglocs$idrank < breaks[i]) 
#                    + yravglocs$idrank*(yravglocs$idrank>=breaks[i]))
#   mse[i] <- summary(piecewise1)[6]
# }
# mse <- as.numeric(mse)
# 
# ## plot breaks by mse and the lowest value should be where the break occurs
# plot(breaks, mse)
# #just to be sure we will choose the break point with the lowest error
# breaks[which(mse==min(mse))]
# 
# piecewise2 <- lm(yravglocs$val ~ yravglocs$idrank*(yravglocs$idrank < 16) + 
#                    yravglocs$idrank*(yravglocs$idrank > 16))
# summary(piecewise2)
# 
# x<- yravglocs$idrank
# y <- yravglocs$val
# 
# plot(x,y)
# curve((-45047.6 + 60024.5) + (4435.7 - 3909.6)*x, add=T, from=1, to=16)
# curve((-45047.6 - 3909.6) + 4435.7*x, add=T, from=16, to=max(x))
# abline(v=16, lty=3)

## another method using package segmented
library(segmented)

### 2009
x <- yravglocs2009$idrank
y <- yravglocs2009$valkm

lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi = 14)
summary(segmented.mod)

plot(x,y, pch=16)
plot(segmented.mod, add=T)

fit <- numeric(length(x)) * NA
fit[complete.cases(rowSums(cbind(y, x)))] <- broken.line(segmented.mod)$fit

yravglocs2009$fit <- fit
yravglocs2009$breakpoint <- 15.903
f <- 15.903
predict(segmented.mod, data.frame(x=f))

bstick2009 <-ggplot(yravglocs2009, aes(x = idrank, y = valkm)) + 
  geom_point(size = 0.75) +
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
  scale_x_continuous(breaks = c(5,10,15,20))+
  scale_y_continuous(limits = c(15,60))

### Designate on and off values for calving ground based on broken stick Merge
yravglocs2009$CalvingGround <- ifelse(yravglocs2009$idrank >= 16, "Off","On")

### 2010
x <- yravglocs2010$idrank
y <- yravglocs2010$valkm

lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=c(14))
summary(segmented.mod)

plot(x,y, pch=16)
plot(segmented.mod, add=T)

fit <- numeric(length(x)) * NA
fit[complete.cases(rowSums(cbind(y, x)))] <- broken.line(segmented.mod, link = F)$fit

yravglocs2010$fit <- fit
yravglocs2010$breakpoint <- 19.553
f <- 19.553
predict(segmented.mod, data.frame(x=f))

bstick2010 <-ggplot(yravglocs2010, aes(x = idrank, y = valkm)) + 
  geom_point(size = 0.75) +
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
  scale_x_continuous(breaks = c(5,10,15,20))

### Designate on and off values for calving ground based on broken stick Merge
yravglocs2010$CalvingGround <- ifelse(yravglocs2010$idrank >= 20, "Off","On")

### 2011
x <- yravglocs2011$idrank
y <- yravglocs2011$valkm

lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=c(14))
summary(segmented.mod)

plot(x,y, pch=16)
plot(segmented.mod, add=T)

fit <- numeric(length(x)) * NA
fit[complete.cases(rowSums(cbind(y, x)))] <- broken.line(segmented.mod, link = F)$fit

yravglocs2011$fit <- fit
yravglocs2011$breakpoint <- 21.633
f <- 21.633
predict(segmented.mod, data.frame(x=f))

bstick2011 <-ggplot(yravglocs2011, aes(x = idrank, y = valkm)) + 
  geom_point(size = 0.75) +
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
  scale_x_continuous(breaks = c(5,10,15,20))

### Designate on and off values for calving ground based on broken stick Merge
yravglocs2011$CalvingGround <- ifelse(yravglocs2011$idrank >= 22, "Off","On")

### 2012
x <- yravglocs2012$idrank
y <- yravglocs2012$valkm

lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=c(14))
summary(segmented.mod)

plot(x,y, pch=16)
plot(segmented.mod, add=T)

fit <- numeric(length(x)) * NA
fit[complete.cases(rowSums(cbind(y, x)))] <- broken.line(segmented.mod, link = F)$fit

yravglocs2012$fit <- fit
yravglocs2012$breakpoint <- 16.503
f <- 16.503
predict(segmented.mod, data.frame(x=f))

bstick2012 <-ggplot(yravglocs2012, aes(x = idrank, y = valkm)) + 
  geom_point(size = 0.75) +
  geom_line(aes(x = idrank, y = fit, group = idrank < breakpoint), color = 'black', size = 0.5)+
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
  scale_x_continuous(breaks = c(5,10,15,20))+
  scale_y_continuous(limits = c(15, 60))

### Designate on and off values for calving ground based on broken stick Merge
yravglocs2012$CalvingGround <- ifelse(yravglocs2012$idrank >= 17, "Off","On")

### 2013
x <- yravglocs2013$idrank
y <- yravglocs2013$valkm

lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=c(10))
summary(segmented.mod)

plot(x,y, pch=16)
plot(segmented.mod, add=T)

fit <- numeric(length(x)) * NA
fit[complete.cases(rowSums(cbind(y, x)))] <- broken.line(segmented.mod, link = F)$fit

yravglocs2013$fit <- fit
yravglocs2013$breakpoint <- 10.328
f <- 10.328
predict(segmented.mod, data.frame(x=f))

bstick2013 <-ggplot(yravglocs2013, aes(x = idrank, y = valkm)) + 
  geom_point(size =0.75) +
  geom_line(aes(x = idrank, y = fit, group = idrank < breakpoint), color = 'black', size = 0.5)+
  ylab("NN dist. (km)")+
  xlab(NULL)+
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
  scale_x_continuous(limits = c(0, 15),breaks = c(5,10,15))

library(grid)
library(gridExtra)
plot_grid(bstick2009, bstick2010, bstick2011, bstick2012, bstick2013 ,
          labels=c("a", "b", "c", "d", "e"), ncol = 3, nrow = 2, label_size = 14)

pdf("/Users/maegwinbonar 1/Dropbox/Vander Wal Lab/CH2data/Fig2_bknstick.pdf", 
    width = 6.8, height = 4, pointsize = 2)
grid.arrange(bstick2009, bstick2010, bstick2011, bstick2012, bstick2013, nrow = 2)
dev.off()

pdf("/Users/maegwinbonar 1/Dropbox/Vander Wal Lab/CH2data/Fig2_bknstick2.pdf", 
    width = 6.8, height = 4, pointsize = 2)
plot_grid(bstick2009, bstick2010, bstick2011, bstick2012, bstick2013 ,
          labels=c("a", "b", "c", "d", "e"), ncol = 3, nrow = 2, label_size = 14)
dev.off()

qplot(idrank, fit, group = idrank < 10.328, geom = c('point', 'smooth'), 
      method = 'lm', se = F, data = yravglocs2013)

### Designate on and off values for calving ground based on broken stick Merge
yravglocs2013$CalvingGround <- ifelse(yravglocs2013$idrank >= 11, "Off","On")

### Change mr2009a31 in year 2013 to "On" instead of "Off"
yravglocs_ranked$IDyear <- paste(yravglocs_ranked$ID, yravglocs_ranked$Year, sep = "_")
yravglocs_ranked$CalvingGround[yravglocs_ranked$IDyear=="mr2009a31_2013"] <- "On"

yravglocs_ranked_new <- rbind(yravglocs2009, yravglocs2010, yravglocs2011, yravglocs2012, yravglocs2013)

### yearly averae IDs with other IDs so we can have collar and year info Bring
survival <- fread("~/Desktop/Vander Wal Lab/MRCaribouFemalesGPS/Survival_ComboIBMPBM_MR.csv")

yravglocs_ranked$IDYear <- paste(yravglocs_ranked$Year,yravglocs_ranked$ID, sep = "")
avglocs$IDYear <- paste(avglocs$Year,avglocs$distID, sep = "")
Mrlocs$IDYear <- paste(Mrlocs$Year,Mrlocs$distID, sep = "")

### in the survival estimates from IBM and PBM methods Merge with yearly values
yravglocs_ranked <- merge(yravglocs_ranked, survival, by = "IDYear")
avglocs<- merge(avglocs, yravglocs_ranked, by = "IDYear", all.x = T, all = F)
Mrlocs<- merge(Mrlocs, yravglocs_ranked, by = "IDYear", all.x = T, all = F)

Mrlocs <- setDT(Mrlocs)[,. (ANIMAL_ID, FIX_DATE, FIX_TIME, EASTING, NORTHING, 
                            X_COORD, Y_COORD, Year, meanDistance, CalvingGround,Calved.y,
                            CalvingDate, Lost, LossDate)]
setnames(Mrlocs, c("meanDistance", "Calved.y"),c("NNdist", "Calved"))

avglocs <- setDT(avglocs)[,. (ID, idate, val, logval, Year.x, 
                            CalvingGround,Calved.y,CalvingDate, Lost, LossDate)]
setnames(avglocs, c("idate", "val", "logval", "Year.x", "Calved.y"),
         c("Date", "NNdist", "logNNdist", "Year", "Calved"))

yravglocs_ranked <- setDT(yravglocs_ranked)[,. (ID, Year, NNdist, 
                              CalvingGround,Calved,CalvingDate, Lost, LossDate)]

yravglocs_ranked$NNdistkm <- yravglocs_ranked$NNdist/1000
avglocs$NNdistkm <- avglocs$NNdist/1000
yravglocs_ranked$NNdistkm <- yravglocs_ranked$NNdist/1000
### by ID Run a glmm or something to find the relationships between survival and
### NN or on and off calving ground
yravglocs_ranked$Calved2 <- ifelse(yravglocs_ranked$Calved == "TRUE", 1,0)
yravglocs_ranked$Lost2 <- ifelse(yravglocs_ranked$Lost1 == "TRUE", 1,0)
yravglocs_ranked$CalvingGround <- as.factor(yravglocs_ranked$CalvingGround)
yravglocs_ranked$Year.x <- as.factor(yravglocs_ranked$Year.x)
yravglocs_ranked$CalvingDate2 <- as.IDate(yravglocs_ranked$CalvingDate2)
yravglocs_ranked$LossDate2 <- as.IDate(yravglocs_ranked$LossDate2)
yravglocs_ranked$ID <- as.factor(yravglocs_ranked$ID)

### add nearest neighbour distance at date of parturition
avglocs$IDdate <- paste(avglocs$distID, avglocs$idate, sep = "")

yravglocs_ranked$IDdate <- paste(yravglocs_ranked$distID, yravglocs_ranked$CalvingDate2, sep = "")

yravglocs_ranked <- merge(yravglocs_ranked, avglocs, by = "IDdate", all.x = T, all = F)

yravglocs_ranked <- setDT(yravglocs_ranked)[, .(distID.x, Year.x, val.x, CalvingGround, 
                                                Calved2, CalvingDate2, Lost2, LossDate2, val.y)]
colnames(yravglocs_ranked) <- c("ID", "Year", "NNdist", "CalvingGround", "Calved", "CalvingDate", 
                                "Lost", "LossDate", "NNpart")
yravglocs_ranked$LossDate[yravglocs_ranked$Lost==0] <- NA

par(mfrow=c(1,1))
plot(yravglocs_ranked$Lost ~yravglocs_ranked$NNdist)

plot(yravglocs_ranked$NNdistkm, yravglocs_ranked$Lost)
curve(predict(model1, data.frame(NNdistkm=x), type="response"), add=TRUE, lwd=2) 

yrvOn$predict.mod1 = predict(model2a, type = "response", newdata = yrvOn)

#pdf("/Users/maegwinbonar/Desktop/Vander Wal Lab/CH2data/CalvinggroundOn_curve2.pdf",width = 7, height = 4)
curve1<-ggplot(yrvOn, aes(x=NNdistkm_cent, y=Lost))+ 
  geom_point(size = 2, alpha=0.5, colour = 'black')+
  #geom_text(aes(density2, propON,label=Year),hjust=0.5, vjust=-1, size = 7)+
  #geom_smooth(method = "glm", fullrange=T, col="black", se=T)+
  #stat_smooth( aes(y = Lost),  method="glm", family="binomial", se=F)+
  geom_line(data = yrvOn, aes(x = NNdistkm_cent, y = predict.mod1), size=0.5) +
  #geom_vline(xintercept = 17.5512, colour = "red", linetype="dashed")+
  ylab("Prob. calf mortality")+ xlab("Average NN dist (mean-centered)")+
  #xlim(-1.5, 1.5)+ylim(0, 0.4)+
  guides(colour = F)+
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        axis.text.x=element_text(size=10, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 10, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_colour_gradient(low = "#2c7bb6", high = "#fdae61")
#dev.off()




summary(model2a)

### add survival and calving ground status to daily average NN distance dataframe
cg_all<- setDT(yravglocs_ranked)[, .(ID, Year,CalvingGround, 
                                                Lost)]
cg_all$IDYear <- paste(cg_all$Year, cg_all$ID, sep = "")
avglocs$IDYear <- paste(avglocs$Year, avglocs$distID, sep = "")

avglocs_cg <- merge(avglocs,cg_all, by = "IDYear", all.x = T, all = F)
avglocs_cg$NNdistkm <- avglocs_cg$val/1000

plot(avglocs_cg$Lost ~avglocs_cg$val)

library(lme4)
require(MuMIn)
#mean values

str(yravglocs_ranked)
yravglocs_ranked$ID <- as.factor(yravglocs_ranked$ID)

model1 <- glmer(Lost ~ NNdistkm+(1|Year)+(1|ID), data = yravglocs_ranked, family = "binomial")
summary(model1)

yravglocs_ranked$predict.mod1 = predict(model2, type = "response", newdata = yravglocs_ranked)

ggplot(yravglocs_ranked, aes(x=NNdistkm, y=Lost))+ 
  geom_point(size = 4, alpha=0.5, aes(colour = CalvingGround))+
  #geom_text(aes(density2, propON,label=Year),hjust=0.5, vjust=-1, size = 7)+
  #geom_smooth(method = "glm",fullrange=T, col="black", se=T)+
  #stat_smooth(aes(y = Lost), colour="black", method="glm", family="binomial", se=T)+
  #geom_line(data = yravglocs_ranked, aes(x = NNdistkm, y = predict.mod1), size=1) +
  #geom_vline(xintercept = 17.5512, colour = "red", linetype="dashed")+
  ylab("Probability of calf mortality")+ xlab("Average NN dist (km)")+
  #xlim(-1.5, 1.5)+ylim(0, 0.4)+
  guides(colour = F)+
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        axis.text.x=element_text(size=14, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 14, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 14),
        axis.title.y = element_text(size = 14),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))
  #scale_colour_gradient(low = "#2c7bb6", high = "#fdae61")


model1daily <- glmer(Lost ~ NNdistkm+(1|Year.x)+(1|distID), data = avglocs_cg, family = "binomial")
summary(model1daily)



r.squaredGLMM(model2a)

boxplot(yravglocs_ranked$NNdist ~yravglocs_ranked$Lost, main = "All individuals (n=101)",
        ylab = "Yearly average nearest neighbour distance", xlab = "Calf loss")

box1 <-ggplot(aes(x=as.factor(Lost), y=NNdistkm, fill = as.factor(Lost)), data = yravglocs_ranked)+
  geom_boxplot(notch = T)+ ylab("Avg. NN dist. (km)")+ xlab("Calf survival")+
  scale_x_discrete(labels = c("Survived", "Lost"))+ 
  theme(legend.position = "none",
        axis.text.x=element_text(size=10, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 10, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_fill_manual(values = c("gray","dark gray"))

pdf("/Users/maegwinbonar/Desktop/Vander Wal Lab/CH2data/NNboxplot_fig1.pdf",width = 3.5, height = 3.5)
ggplot(aes(x=as.factor(Lost), y=NNdistkm, fill = as.factor(Lost)), data = yravglocs_ranked)+
  geom_boxplot(notch = T)+ ylab("Season avg NN distance (km)")+ xlab("Calf survival")+
  scale_x_discrete(labels = c("Survived", "Lost"))+ 
  theme(legend.position = "none",
        axis.text.x=element_text(size=14, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 14, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 14),
        axis.title.y = element_text(size = 14),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_fill_manual(values = c("gray","dark gray"))

dev.off()


ggplot(aes(x=as.factor(Lost), y=as.integer(CalvingGround2), fill = as.factor(Lost)), data = yravglocs_ranked)+
  geom_point()+ geom_line(aes(fit))+ylab("Yearly average NN distance (m)")+ xlab("Calf survival")+
  #scale_x_discrete(labels = c("Survived", "Lost"))+ 
  theme(legend.position = "none",
        axis.text.x=element_text(size=20, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 20, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_fill_manual(values = c("#00bfc4","#f8766d"))+
  scale_y_continuous(breaks = c(20000,40000,60000))

model2 <- glmer(Lost ~ CalvingGround+(1|Year)+(1|ID), data = yravglocs_ranked, family = "binomial")
summary(model2)

library(simr)
powerSim(model2)
pc<-powerCurve(model2)
print(pc)
plot(pc)

model2daily <- glmer(Lost ~ CalvingGround+(1|Year.x)+(1|distID), data = avglocs_cg, family = "binomial")
summary(model2daily)

avgOn <- avglocs_cg[avglocs_cg$CalvingGround == "On",]
avgOff <- avglocs_cg[avglocs_cg$CalvingGround == "Off",]

model2adaily <- glmer(Lost ~ NNdistkm+(1|Year.x)+(1|ID), data = avgOn, family = "binomial")
summary(model2adaily)

model2bdaily <- glmer(Lost ~ NNdistkm+(1|Year.x)+(1|ID), data = avgOff, family = "binomial")
summary(model2bdaily)

model3daily <- glmer(Lost ~ NNdistkm*CalvingGround+(1|Year.x)+(1|ID), data = avglocs_cg, family = "binomial")
summary(model3daily)

plot(model2)

r.squaredGLMM(model2)

yrvOn <- yravglocs_ranked[yravglocs_ranked$CalvingGround == "On",]
yrvOn$NNdistkm <- yrvOn$NNdist/1000
yrvOn$NNdistkm_cent <- (yrvOn$NNdistkm - mean(yrvOn$NNdistkm))/sd(yrvOn$NNdistkm)
boxplot(yrvOn$NNdist ~ yrvOn$Lost, main = "Individuals on calving ground (n=81)",
        ylab = "Season average nearest neighbour distance", xlab = "Calf loss")

box2 <-ggplot(aes(x=as.factor(Lost), y=NNdistkm, fill = as.factor(Lost)), data = yrvOn)+
  geom_boxplot(notch = T)+ ylab("Avg. NN dist. (km)")+ xlab("Calf survival")+
  scale_x_discrete(labels = c("Survived", "Lost"))+ 
  theme(legend.position = "none",
        axis.text.x=element_text(size=10, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 10, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_fill_manual(values = c("gray","dark gray"))


yravglocs_ranked$CalvingGround2[yravglocs_ranked$CalvingGround == "On"] <- "1On calving ground"
yravglocs_ranked$CalvingGround2[yravglocs_ranked$CalvingGround == "Off"] <- "2Off calving ground"
levels(yravglocs_ranked$CalvingGround2) <- c("On calving ground", "Off calving ground")
yravglocs_ranked$NNdist2 <- yravglocs_ranked$NNdist/1000

pdf("/Users/maegwinbonar/Desktop/Vander Wal Lab/CH2data/NNboxplot2.pdf",width = 11.02, height = 4.45)
ggplot(aes(x=as.factor(Lost), y=NNdistkm, fill = as.factor(Lost)), data = yravglocs_ranked)+
  geom_boxplot(notch = T)+ ylab("Season average NN distance (km)")+ xlab("Calf survival")+
  scale_x_discrete(labels = c("Survived", "Lost"))+ 
  facet_grid(~CalvingGround2)+
  theme(legend.position = "none",
        axis.text.x=element_text(size=20, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 20, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 24),
        axis.title.y = element_text(size = 20),
        strip.text.x = element_text(size = 24),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_fill_manual(values = c("#00bfc4","#f8766d"))+
  scale_y_continuous(breaks = c(20,40,60))
dev.off()

yrvOff <- yravglocs_ranked[yravglocs_ranked$CalvingGround == "Off",]
yrvOff$NNdistkm <- yrvOff$NNdist/1000
boxplot(yrvOff$NNdist ~ yrvOff$Lost, main = "Individuals off calving ground (n=20)",
        ylab = "Season average nearest neighbour distance", xlab = "Calf loss")

box3 <-ggplot(aes(x=as.factor(Lost), y=NNdistkm, fill = as.factor(Lost)), data = yrvOff)+
  geom_boxplot(notch = T)+ ylab("Avg. NN dist. (km)")+ xlab("Calf survival")+
  scale_x_discrete(labels = c("Survived", "Lost"))+ 
  theme(legend.position = "none",
        axis.text.x=element_text(size=10, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 10, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_fill_manual(values = c("gray","dark gray"))

install.packages("cowplot")
library(cowplot)
pdf("/Users/maegwinbonar 1/Dropbox/Vander Wal Lab/Ch2data/NNboxplot_curve.pdf",width = 6.8, height = 5, pointsize = 12)
#jpeg("/Users/maegwinbonar/Desktop/Vander Wal Lab/Bonar_et_al_fig2.jpeg",width = 7.0, height = 7.0, pointsize = 12, res = 300, units = "in")
plot_grid(box1, curve1, box2, box3, labels=c("a", "b", "c", "d"), ncol = 2, nrow = 2, label_size = 14)
dev.off()

yrvOn$NNdistkm <- yrvOn$NNdist/1000

model2a <- glmer(Lost ~ NNdist+(1|Year)+(1|ID), data = yrvOn, family = "binomial")
summary(model2a)

summary(yrvOn)

tmp <- as.data.frame(confint(glht(model2a))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()
r.squaredGLMM(model2a)
r.squaredLR(model2a)



model2b <- glmer(Lost ~ NNdist+(1|Year)+(1|ID), data = yrvOff, family = "binomial")
summary(model2b)

yrvOff$predict.mod1 = predict(model2b, type = "response", newdata = yrvOff)
ggplot(yrvOff, aes(x=NNdistkm, y=Lost))+ 
  geom_point(size = 4, alpha=0.5, aes(colour = Lost))+
  #geom_text(aes(density2, propON,label=Year),hjust=0.5, vjust=-1, size = 7)+
  #geom_smooth(method = "glm", fullrange=T, col="black", se=F)+
  #stat_smooth(aes(y = Lost), colour="black", method="glm", family="binomial", se=T)+
  #geom_line(data =yrvOff, aes(x = NNdistkm, y = predict.mod1), size=1) +
  #geom_vline(xintercept = 17.5512, colour = "red", linetype="dashed")+
  ylab("Probability of calf mortality")+ xlab("Average NN dist (km)")+
  #xlim(-1.5, 1.5)+ylim(0, 0.4)+
  guides(colour = F)+
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        axis.text.x=element_text(size=14, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 14, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 14),
        axis.title.y = element_text(size = 14),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_colour_gradient(low = "#2c7bb6", high = "#fdae61")


model3 <- glmer(Lost ~ NNdist*CalvingGround+(1|Year)+(1|ID), data = yravglocs_ranked, family = "binomial")
summary(model3)

## Are the values repeatable?
model4 <- lmer(NNdist ~ Year+(1|ID), data = yravglocs_ranked)
summary(model4)
72643870/(72643870+15078233)


library(rptR)
grname <- unique(yravglocs_ranked$Year)
yravglocs_ranked$CalvingGround2 <- ifelse(yravglocs_ranked$CalvingGround == "On", 1,0)
yravglocs_ranked$CalvingGround2 <- as.integer(yravglocs_ranked$CalvingGround2)
model5 <-rpt(CalvingGround2 ~ Year+(1|ID), grname = c("ID"),  
             data = yravglocs_ranked, datatype = "Binary" ,link = "probit", nboot = 100, npermut = 0)
summary(model5)

### Proportion on and off the calving ground
propON <- ddply(yravglocs_ranked, .(Year), summarize, val = mean(CalvingGround2))
### Yearly density values
density <- c(8799,8814,9358,9902,10445)

MRdens <- cbind(density, propON)
MRdens$propOFF <- 1-MRdens$val
MRdens$density2 <- (MRdens$density - mean(MRdens$density))/sd(MRdens$density)
colnames(MRdens) <- c("density", "Year", "propON", "propOFF", "density2")
plot(MRdens$density2, MRdens$propOFF, ylab = "Proportion off calving ground", 
     xlab = "Population estimate", ylim=c(0,0.4))
curve(predict(m2, data.frame(MRdens$density2), type="response"), add=TRUE, lwd=2) 

range(MRdens$propOFF)

m <- lm(MRdens$propOFF ~ MRdens$density2)
summary(m)

m2 <- lm(MRdens$propOFF ~ MRdens$val)
summary(m2)

MRdens2 <- MRdens[!MRdens$Year == "2009",]
m2 <- lm(MRdens2$propOFF ~ MRdens2$density2)
summary(m2)

lmodel <- lm(propOFF~density2, data = MRdens)
MRdens$fit2 <- NA

MRdens$fit2[MRdens$Year=="2010"] <- 0.1083038
MRdens$fit2[MRdens$Year=="2011"] <- 0.1606303
MRdens$fit2[MRdens$Year=="2012"] <- 0.2129568
MRdens$fit2[MRdens$Year=="2013"] <- 0.2651871

pdf("/Users/maegwinbonar 1/Dropbox/Vander Wal Lab/CH2data/NNdensity1_new2.pdf",width = 6.8, height = 4)
ggplot(MRdens, aes(x=density2, y=propOFF, shape=Year))+ geom_point(size = 5)+
  #geom_text(aes(density2, propON,label=Year),hjust=0.5, vjust=-1, size = 7)+
  #geom_smooth(method = "lm", fullrange=T, col="black", se=F)+
  #geom_abline(slope = 1, intercept = 0)+
  geom_smooth(aes(x=density2, y=fit2), method = "lm")+
  ylab("Prop. disaggregating")+ xlab("Population size (mean-centered)")+
  xlim(-1.5, 1.5)+ylim(0, 0.4)+
  guides(shape = guide_legend(override.aes = list(size=4)))+
  theme(legend.justification=c(1,0), legend.position=c(0.98, 0.02),
        legend.text=element_text(size=14),
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.text.x=element_text(size=14, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 14, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 14),
        axis.title.y = element_text(size = 14),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_color_brewer(palette = "RdYlBu")
dev.off()

pdf("/Users/maegwinbonar 1/Dropbox/Vander Wal Lab/CH2data/NNdensity1_new2_no2009.pdf",width = 6.8, height = 4)
ggplot(data = MRdens2)+
  geom_smooth(aes(density2, propOFF), method = "lm", fullrange=T, col="dark gray", se=F, linetype = 2)+
  xlim(-1.5, 1.5)+ylim(0, 0.4)+
  guides(shape = guide_legend(override.aes = list(size=4)))+
  theme(legend.justification=c(1,0), legend.position=c(0.98, 0.02),
        legend.text=element_text(size=14),
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.text.x=element_text(size=14, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 14, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 14),
        axis.title.y = element_text(size = 14),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))
dev.off()
  
plot(MRdens$propOFF, MRdens$propON)
## need to jitter the years 2009 and 2013 so that they are visible
MRdens$propON[5] = 0.7142857+0.005
plot(MRdens$propON, MRdens$propOFF, ylim=c(0, 0.4), xlim=c(0.65, 0.9))
text(MRdens$propON, MRdens$propOFF, labels = MRdens$Year, cex= 0.7, pos=3)

pdf("/Users/maegwinbonar/Desktop/Vander Wal Lab/CH2data/NNdensity2.pdf",width = 6.00, height = 4.57)
ggplot(MRdens)+ geom_point(aes(propON, propOFF, col = Year), size = 5)+
  #geom_text(aes(propON, propOFF,label=Year),hjust=0.5, vjust=-1, size = 7)+
  ylab("Proportion off calving ground")+ xlab("Proportion on calving ground")+
  xlim(0.65, 0.95)+ylim(0.05, 0.35)+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  theme(legend.justification=c(1,0), legend.position=c(0.98, 0.62),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.text.x=element_text(size=20, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
        axis.text.y=element_text(size = 20, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
        axis.title.x=element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_color_brewer(palette = "RdYlBu")
dev.off()


lost <- c(0.308642, 0.35, 0.691358, 0.65)
state <- c("Lost", "Lost", "Survived", "Survived")
calvingground <- c("On", "Off", "On","Off")

CG <- data.frame(lost, state, calvingground)

pdf("/Users/maegwinbonar/Desktop/Vander Wal Lab/CH2data/CalvingGroundbarplot1.pdf",width = 5.43, height = 4.33)
bar1 <-ggplot(data=CG, aes(x=calvingground, y=lost, fill=state)) +
  ylab("Proportion")+ xlab("Calving ground")+
  geom_bar(stat="identity")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  guides(fill=guide_legend(title="Calf survival"))+
  theme(legend.text=element_text(size=12),
        legend.key = element_blank(),
        legend.title = element_text(size = 12),
              axis.text.x=element_text(size=12, colour = "black",margin=margin(c(0.1,0.1,5,0.1))),
              axis.text.y=element_text(size = 12, colour = "black",margin=margin(c(0.1,0.1,0.1,5))),
              axis.title.x=element_text(size = 12),
              axis.title.y = element_text(size = 12),
              strip.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(size = 1, colour = "black", fill=NA))+
  scale_fill_manual(name=NULL,
                     labels = c("Lost", "Survived"),
                    values = c("#fdae61","#2c7bb6"))
  #scale_fill_brewer(palette="PuBuGn")
  #scale_x_discrete(labels=c("1Calved" = "Calved", "2Calf Died" = "Calf Died",
                            #"3Calf Survived" = "Calf Survived"))
dev.off()


### find the switching threshold for individuals on the calving ground
ggplot(yrvOn, aes(x = NNdist, y = Lost)) + 
  geom_point()+
  geom_line(aes(y=fit), size=0.8)

library(effects)

library(sjPlot)
library(devtools)
devtools::install_github("strengejacke/sjPlot")

model2a <- glmer(Lost ~ NNdist+(1|Year)+(1|ID), data = yrvOn, family = "binomial")
summary(model2a)

model2b <- glmer(Lost ~ NNdist+(1|Year)+(1|ID), data = yrvOff, family = "binomial")
summary(model2b)

model2 <- glmer(Lost ~ CalvingGround+(1|Year)+(1|ID), data = yravglocs_ranked, family = "binomial")
summary(model2)
powerSim(model2)

model1 <- glmer(Lost ~ NNdist2+(1|Year)+(1|ID), data = yravglocs_ranked, family = "binomial")
summary(model1)

sjp.glmer(model2a, type = "fe.pc")
sjp.glmer(model2b, type = "fe.pc")
sjp.glmer(model2, type = "fe.pc")
sjp.glmer(model1, type = "fe.pc")

plot(effect("NNdist", model2a))

write.csv(avglocs, "/Users/maegwinbonar/Desktop/Vander Wal Lab/CH2data/avg_daily_NNdist.csv")
write.csv(Mrlocs, "/Users/maegwinbonar/Desktop/Vander Wal Lab/CH2data/raw_all_NNdist.csv")
write.csv(yravglocs_ranked, "/Users/maegwinbonar/Desktop/Vander Wal Lab/CH2data/avg_season_NNdist.csv")

str(avg_daily_NNdist_vor)
