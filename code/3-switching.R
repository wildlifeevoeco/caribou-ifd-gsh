
library(data.table)


avg_season <- fread("data/avg_season_NNdist.csv")


avg_season$Year<-factor(avg_season$Year, labels=c("2009","2010","2011","2012","2013"))

avg_season$CalvingGround2<- ifelse(avg_season$CalvingGround=="On",0,1)


###determine which individuals switched between on/off in two consuctive years 
# 0 = no switch, 1 = switching event

avg_season$ID <- as.factor(avg_season$ID)
library(data.table)

avg_season$Lag1 <- ave(avg_season$CalvingGround2, avg_season$ID, 
                       FUN= function(x) c(NA, x[-length(x)] )  )
View(avg_season)

avg_season$switch <- abs(avg_season$CalvingGround2 - avg_season$Lag1)
