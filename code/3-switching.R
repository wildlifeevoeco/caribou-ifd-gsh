
library(data.table)

## load data
avg_season <- fread("data/avg_season_NNdist.csv")

avg_season$CalvingGround2<- ifelse(avg_season$CalvingGround=="On",0,1)


###determine which individuals switched between on/off in two consuctive years 
# 0 = no switch, 1 = switching event

avg_season$ID <- as.factor(avg_season$ID)

avg_season$Lag1 <- ave(avg_season$CalvingGround2, avg_season$ID, 
                       FUN= function(x) c(NA, x[-length(x)] )  )

avg_season$switch <- abs(avg_season$CalvingGround2 - avg_season$Lag1)

switch <- avg_season[, c("ID", "switch", "Year")]

switch <- switch[!is.na(switch)]


