

### Packages ----
libs <- c('data.table', 'ggplot2', 'rptR')
lapply(libs, require, character.only = TRUE)


## Load data
avg_season <- fread("data/analysis/avg_season_NNdist.csv")
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


### repeatability in NN distance (intra-annual) -- Results in Table S3
rep <- fread("data/analysis/monthly-rep.csv")
rep$month <- as.factor(rep$month)
rep$avgNN <- rep$V1/1000

month_rep <-  rpt(avgNN ~ Year*month + (1|ANIMAL_ID), 
                  data = rep, grname = c("ANIMAL_ID"))
summary(month_rep)
