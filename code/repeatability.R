

### Packages ----
libs <- c('data.table', 'ggplot2', 'rptR')
lapply(libs, require, character.only = TRUE)


## Load data
avg_season <- fread("data/avg_season_NNdist.csv")
avg_season$Year <- as.factor(avg_season$Year) 
avg_season$ID <- as.factor(avg_season$ID) 

avg_season$CalvingGround2[avg_season$CalvingGround == "On"] <- 0
avg_season$CalvingGround2[avg_season$CalvingGround == "Off"] <- 1
avg_season$CalvingGround2 <- as.integer(avg_season$CalvingGround2)


## repeatability in NN distance (inter-annual) -- Results in Table S3
mod1<-rpt(NNdistkm ~ Year + (1|ID), data = avg_season, grname = c("ID"))
summary(mod1)


## transform data to generate rank
datanew<-transform(avg_season, 
                   year.rank = ave(NNdistkm, Year, 
                                   FUN = function(x) rank(-x, ties.method = "first")))

##repeatability of NN rank (inter-annual) -- Results in Table S3
modrank<-rpt(year.rank~Year + (1|ID), data = datanew, grname = c("ID"))
summary(modrank)


## repeatability in calving ground (on/off)
## don't include year as fixed effect
modon.off<-rpt(CalvingGround2 ~ (1|ID), data = avg_season, datatype = "Binary", 
               grname = c("ID"))
summary(modon.off)

##repeatability in core/periphery 
avg_season$CoreThresh<- with(avg_season, NNdistkm - 17.5512)
avg_season$Core <- as.numeric(ifelse(avg_season$CoreThresh > 0, 0, 1))
avg_season$Periph<- as.integer(ifelse(avg_season$CoreThresh < 0, 0, 1))
modcore <- rpt(factor(Core) ~ 1 + (1|ID), data = avg_season[CalvingGround == "On"],
             datatype = "Binary",
             grname = c("ID"))
summary(modcore)
plot(modcore)

a1 <- lme4::glmer(Core ~ 1 + (1|ID), 
      family = "binomial", 
      data = avg_season[CalvingGround == "On"])
summary(a1)
