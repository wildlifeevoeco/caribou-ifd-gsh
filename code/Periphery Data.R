library(readxl)
library(data.table)
pc <- fread("data/Periphery_Core.csv")

pc$Month[pc$Month == "5"] <- "05"
pc$Month[pc$Month == "6"] <- "06"
pc$Month[pc$Month == "7"] <- "07"


pc$Month <- as.factor(pc$Month)
pc$Year <- as.factor(pc$Year)

pc$ID.yr<- paste(pc$ID, pc$Year, sep = "_")
pc$ID.yr.mn<- paste(pc$ID.yr, pc$Month, sep = "_")
##create new data frame to work on in case I fuck it all up 
df <- pc

##add the perophery data by animal ID
df$Periphery<- pc$Periphery[match(df$ID.yr.mn, pc$ID.yr.mn)]

##change 2 (=off) in df to NA
df$Periphery[df$Periphery == "2"] <- NA

##
## calculate distance from global core-periphery 

df$Off.Thresh<-
  ifelse(df$Year==2009, 23.7233,
  ifelse(df$Year==2010, 25.72665,
  ifelse(df$Year==2011, 23.03188,
  ifelse(df$Year==2012, 29.54081,
  ifelse(df$Year==2013, 32.70388,
                                     NA  )))))

df$Dist.Off <- abs(df$MeanNN - df$Off.Thresh) 

df$CalvingGround2<- ifelse(df$CalvingGround=="On",0,1)


df$Lag1 <- ave(df$CalvingGround2, df$ID.yr.mn, 
                       FUN= function(x) c(NA, x[-length(x)] )  )

df$switch <- abs(df$CalvingGround2 - df$Lag1)

## calculate distance from global core-periphery 

df$CoreThresh<- with(df, MeanNN - 17.5512)

## if ID is in the core = 1 if on periphery = 0 

avg_season$Core<- ifelse(avg_season$CoreThresh > 0, 0, 1)

on <-subset(df, CalvingGround== "On")

peri <- rpt(Periphery ~ Year + Month + (1|ANIMAL_ID), data = df, grname = c("ANIMAL_ID"))

summary(peri)



