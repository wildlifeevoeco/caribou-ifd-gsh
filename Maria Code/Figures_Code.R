

library(readr)
library(dplyr)
library(stats)
library(data.table)
library(ggsignif)

avg_season <- read_csv("~/Downloads/Winter Semester/Honours Readings/Statistics /data/avg_season_NNdist.csv")
View(avg_season)

avg_season$Year<-factor(avg_season$Year, labels=c("2009","2010","2011","2012","2013"))
levels(avg_season$Year)

avg_season$CalvingGround2<- ifelse(avg_season$CalvingGround=="On",0,1)

avg_season$Off.Thresh<-
  ifelse(avg_season$Year==2009, 23.7233,
  ifelse(avg_season$Year==2010, 25.72665,
  ifelse(avg_season$Year==2011, 23.03188,
  ifelse(avg_season$Year==2012, 29.54081,
  ifelse(avg_season$Year==2013, 32.70388,
                                     NA  )))))
str(avg_season)


avg_season$Dist.Off <- abs(avg_season$NNdistkm - avg_season$Off.Thresh)                    

avg_season$ID <- as.factor(avg_season$ID)

avg_season$Lag1 <- ave(avg_season$CalvingGround2, avg_season$ID, 
                       FUN= function(x) c(NA, x[-length(x)] )  )

avg_season$switch <- abs(avg_season$CalvingGround2 - avg_season$Lag1)

avg_season$direction <- (avg_season$CalvingGround2 - avg_season$Lag1)

data1<-avg_season
data1$switch <- as.character(data1$switch)

data1$Switch<-
  ifelse(avg_season$switch==1, "Yes",
  ifelse(avg_season$switch==0, "No",
                NA  ))
data1$Direction<-
  ifelse(avg_season$direction==-1, "Moved On",
  ifelse(avg_season$direction== 1, "Moved Off",
                NA  ))

switchdata<-subset(data1, switch== "1")
View(switchdata)

data1$Switch <- as.factor(data1$Switch)
avg_season$switch <- as.factor(avg_season$switch)

ggplot(data=subset(data1, !is.na(Switch)), aes(x = Switch, y = Dist.Off)) +
  geom_boxplot(outlier.colour = NA, notch = TRUE, notchwidth = 0.5 ) +
  scale_y_continuous(name = "Distance from Threshold (km)") +
  theme(axis.text=element_text(size=32), axis.title=element_text(size=32,face="bold")) +
  coord_cartesian(ylim = c(0, 18))

##Individually assign switchers 
avg_season$switchers<-
  ifelse(avg_season$ID== "mr2009a07", 1,
  ifelse(avg_season$ID== "mr2009a14", 1,
  ifelse(avg_season$ID== "mr2009a26", 1,
  ifelse(avg_season$ID== "mr2009a31", 1,
  ifelse(avg_season$ID== "mr2011a01", 1,
  ifelse(avg_season$ID== "mr2009a18", 1,
  ifelse(avg_season$ID== "mr2011a02", 1,
  ifelse(avg_season$ID== "mr2011a04", 1,
                                  0  ))))))))

Switcher <-subset(avg_season, switchers== "1")

ggplot(Switcher, aes(Year, CalvingGround, group = ID)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~ID) +
  theme(axis.text.x = element_text(angle=60, hjust=1), 
        legend.position = 'none',
        plot.title = element_text(size = 20, color = "black"),
        axis.text=element_text(size=10, color = "black"),
        axis.title=element_text(size=10)) +
  theme_light()+
  ylab(NULL) + 
  xlab(NULL) 