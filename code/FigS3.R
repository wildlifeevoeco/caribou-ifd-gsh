
### Packages ----
libs <- c('data.table', 'ggplot2', 'lubridate')
lapply(libs, require, character.only = TRUE)

## Load data
NN <- fread("data/avg_season_NNdist.csv")

NN[, CalvingDate := as.IDate(CalvingDate)]
NN[, JDate := yday(CalvingDate)]

NN[, .N, by = "Year"]

avg <- NN[, median(NNdistkm), by = "Year"]

NN$Lost[NN$Lost == 0] <- "Survived"
NN$Lost[NN$Lost == 1] <- "Died"

png("graphics/FigS3.png", width = 6000, height = 4000,
    units = "px", res = 600)
ggplot(NN) +
  geom_jitter(aes(JDate, NNdistkm, color = factor(Lost))) +
  ylab("Avg. NN dist. (km)") +
  xlab("Parturition date (days since January 1)") +
  geom_hline(data = avg, aes(yintercept = V1), lty = 2) +
  theme(legend.position = c(0.8,0.3),
        legend.key = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.background = element_rect(
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        plot.title=element_text(size = 14, hjust=0),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        strip.text = element_text(size = 14)) +
  facet_wrap(~Year)
dev.off()


