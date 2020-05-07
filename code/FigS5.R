

### Packages ----
libs <- c('data.table', 'ggplot2', 'spatsoc', 'gridExtra')
lapply(libs, require, character.only = TRUE)


## load data
DT <- readRDS("data/locs/caribou-all.RDS")

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

png("graphics/FigS5.png", height = 3000, width = 6000, res = 600, units = "px")
aa <-ggplot(distNN) +
  geom_histogram(aes(distance/1000, fill = factor(Year))) +
  ylab("Count") +
  xlab("Nearest neighbour distnace (km)") +
  ggtitle('A)') +
  theme(legend.position = 'none', 
        axis.title = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        plot.title=element_text(size = 12, hjust=0),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        strip.text = element_text(size = 12)) +
  facet_wrap(~Year)

bb <- ggplot(distNN[distance < 500]) +
  geom_histogram(aes(distance, fill = factor(Year))) +
  ylab("Count") +
  xlab("Nearest neighbour distnace (m)") +
  ggtitle('B)') +
  theme(legend.position = c(0.85, 0.25),  
        legend.title = element_blank(),
        axis.title = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        plot.title=element_text(size = 12, hjust=0),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        strip.text = element_text(size = 12)) +
  facet_wrap(~Year)
grid.arrange(aa,bb, ncol = 2)
dev.off()