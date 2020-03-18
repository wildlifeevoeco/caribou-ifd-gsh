

### Packages ----
libs <- c('data.table', 'ggplot2', 'spatsoc')
lapply(libs, require, character.only = TRUE)


## load data
DT <- readRDS("output/1-caribou-all.RDS")

## calculate group_times
DT <- group_times(DT, datetime = 'datetime', threshold = '5 minutes')

## calculate nearest neighbour distance
distGlob <- edge_dist(DT = DT, id = 'IDYr', coords = c('EASTING', 'NORTHING'),
                   timegroup = 'timegroup', threshold = 500000, returnDist = TRUE, 
                   fillNA = FALSE,
                   splitBy = c("Year"))
distGlob <- distGlob[!is.na(distGlob$distance)]

## calculate average global distance
distGlobSum <- distGlob[, mean(distance), by = c("ID1")]
colnames(distGlobSum) <- c("IDYr", "NNglob")

## calculate NEAREST NEIGHBOUR
distNN <- edge_nn(DT = DT, id = 'IDYr', coords = c('EASTING', 'NORTHING'),
                  timegroup = 'timegroup', threshold = 500000, returnDist = TRUE, 
                  splitBy = c("Year"))
distNN <- distNN[!is.na(distNN$distance)]

## calculate average NN distance
distNNSum <- distNN[, mean(distance), by = c("ID")]
colnames(distNNSum) <- c("IDYr","NN")


distALL <- merge(distGlobSum, distNNSum, by = "ID")

ggplot(distALL, aes(NNglob, NN)) +
  geom_point() +
  geom_smooth()  +
  xlim(0, 100000) +
  ylim(0, 100000)

DT2 <- merge(DT, distNNSum, by = "IDYr")
DT2[NN >= 17000, agg := "off"]
DT2[NN < 17000, agg := "on"]


ggplot(DT2) +
  geom_point(aes(EASTING, NORTHING, color = agg), alpha = 0.25) +
  facet_wrap(~Year)

