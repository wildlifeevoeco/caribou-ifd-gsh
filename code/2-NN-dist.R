

### Packages ----
libs <- c('data.table', 'ggplot2', 'spatsoc')
lapply(libs, require, character.only = TRUE)


## load data
DT <- readRDS("output/1-caribou-all.RDS")

## calculate nearest neighbour distance
dist <- edge_dist(DT = DT, id = 'IDYr', coords = c('EASTING', 'NORTHING'),
                   timegroup = 'timegroup', threshold = 50, returnDist = TRUE, 
                   fillNA = FALSE,
                   splitBy = c("Year"))
