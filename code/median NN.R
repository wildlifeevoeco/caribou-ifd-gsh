
library(data.table)
library(ggplot2)
library(gridExtra)
library(spatsoc)

raw <- fread("data/raw_all_NNdist.csv")

dist <- edge_dist(DT = raw, id = 'ANIMAL_ID', coords = c('EASTING', 'NORTHING'),
                      timegroup = 'datetime', threshold = 450000, returnDist = TRUE, 
                      splitBy = c("Year"))
dist2 <- dist[dist[,.I[which.min(distance)],by=. (ID1, datetime)][['V1']]]


meanDist <- dist2[ID1 == "mr2009a08"][, mean(distance), by = "Year"]

ggplot(dist2[ID1 == "mr2009a08"]) +
  geom_histogram(aes(distance)) +
  geom_vline(data = meanDist, aes(xintercept = V1), color = "red") +
  facet_wrap(~Year)
  
ggplot(dist2, aes(distance, fill = ID1)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Year)


raw2 <- raw[, median(NNdist), by = c("Year", "ANIMAL_ID")]
raw2$mean <- raw[, mean(NNdist), by = c("Year", "ANIMAL_ID")]$V1
colnames(raw2)[3] <- "median"

cor.test(raw2$mean, raw2$median)


aa <- ggplot(raw2) +
  geom_density(aes(mean/1000), alpha = 0.75, fill = "grey") +
  geom_density(aes(median/1000), alpha = 0.75, fill = "pink") +
  ylab("Frequency") +
  xlab("Avg. NN dist. (km)") +
  theme(#legend.position = 'none', 
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        plot.title=element_text(size = 14, hjust=0),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        strip.text = element_text(size = 14)) 

bb <- ggplot(raw2) +
  geom_point(aes(mean/1000, median/1000), alpha = 0.75, fill = "grey") +
  ylab("Median NN dist. (km)") +
  xlab("Avg. NN dist. (km)") +
  theme(legend.position = 'none', 
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        plot.title=element_text(size = 14, hjust=0),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        strip.text = element_text(size = 14)) 
grid.arrange(aa,bb, nrow = 1)

a1 <- lm(c(mean/1000)~ c(median/1000), data = raw2)
summary(a1)
