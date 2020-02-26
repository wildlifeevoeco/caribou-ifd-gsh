### Packages ----
libs <- c("data.table", "ggplot2", 
          "sp","gridExtra", "cowplot")
lapply(libs, require, character.only = TRUE)

avgOn <- mean(yrvOn$predict.mod1, na.rm = TRUE)
avg <- mean(yrvOff$predict.mod1, na.rm = TRUE)

## summary statistics:

all <- rbind(yrvOff, yrvOn, fill = T)

aa <- all[Calved == 1][, uniqueN(Year), by = "ID"]

range(aa$V1)
mean(aa$V1)
sd(aa$V1)

a1 <- glm(Lost~NNdistkm_cent, data = yrvOn, family = "binomial")
preddata <- with(a1, data.frame(NNdistkm_cent = seq(min(yrvOn$NNdistkm_cent), max(yrvOn$NNdistkm_cent), length = 100)))
preds <- predict(a1, newdata = preddata, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
fit2 <- a1$family$linkinv(fit)
preddata$upr <- a1$family$linkinv(upr)
preddata$lwr <- a1$family$linkinv(lwr)

### Calf mortality curve (panel b) ----
curve1<-ggplot(yrvOn, aes(x=NNdistkm_cent, y=Lost))+ 
  geom_jitter(size = 2, alpha = 0.4, colour = 'black', height = 0.03)+
  geom_line(data=preddata, mapping=aes(x=NNdistkm_cent, y=upr), lty = 2) + 
  geom_line(data=preddata, mapping=aes(x=NNdistkm_cent, y=lwr), lty = 2) +
  geom_line(data = yrvOn, aes(x = NNdistkm_cent, y = predict.mod1), size = 0.75) +
  ylab("Probability of calf mortality")+ xlab("Average NN dist (mean-centered)")+
  geom_hline(yintercept = avgOn, lty = 3) +
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
curve1
### Calf survival NN distance (panel a) ----
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

### Calf survival NN distance for ON individuals (panel c) ----
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

### Calf survival NN distance OFF individuals (panel d) ----
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

### Putting the whole thing together ----
pdf("graphics/Fig3_boxplot.pdf",width = 5, height = 5, pointsize = 10)
#jpeg("/Users/maegwinbonar/Desktop/Vander Wal Lab/Bonar_et_al_fig2.jpeg",width = 7.0, height = 7.0, pointsize = 12, res = 300, units = "in")
#png("graphics/Fig3_boxplot.png", height = 3000, width = 3000, res = 500, units = "px")
plot_grid(box1, curve1, box2, box3, labels=c("a", "b", "c", "d"), ncol = 2, nrow = 2, label_size = 14)
dev.off()
