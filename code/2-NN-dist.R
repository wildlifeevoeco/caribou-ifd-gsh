

### Packages ----
libs <- c('data.table', 'ggplot2', 'spatsoc')
lapply(libs, require, character.only = TRUE)


## load data
DT <- readRDS("output/1-caribou-all.RDS")

## calculate group_times
DT <- group_times(DT, datetime = 'datetime', threshold = '5 minutes')

## calculate nearest neighbour distance
dist <- edge_dist(DT = DT, id = 'IDYr', coords = c('EASTING', 'NORTHING'),
                   timegroup = 'timegroup', threshold = 500000, returnDist = TRUE, 
                   fillNA = FALSE,
                   splitBy = c("Year"))


dist[, mean(distance), by = c("Year", "ID1")]
