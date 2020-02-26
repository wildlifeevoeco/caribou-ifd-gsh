library(lme4)
library(MCMCglmm)
library(readr)
library(data.table)
library(ggplot2)
library(Matrix)
library(MuMIn)
library(piecewiseSEM)

avg_season <- read_csv("~/Dropbox/Vander Wal Lab/Maria_Honours/Code/avg_season_NNdist.csv")
View(avg_season)


avg_season$Year<-factor(avg_season$Year, labels=c("2009","2010","2011","2012","2013"))
levels(avg_season$Year)


#lapply(seq_along(splitdata1), function(x) {
#  assign(c("Y2009", "Y2010", "Y2011", "Y2012", "Y2013")[x], splitdata1[[x]], envir=.GlobalEnv)
#}
#)

library(MCMCglmm)
library(rptR)

##repeatability in NNdistkm from year to year 
mod1<-rpt(NNdistkm ~ Year + (1|ID), data = avg_season, grname = c("ID"))
summary(mod1)
plot(mod1)

##create ranks of closest NNdistkm within years where the higher the 
#rank the lower the distance(?)
datanew<-transform(avg_season, 
                   year.rank = ave(NNdistkm, Year, 
                                   FUN = function(x) rank(-x, ties.method = "first")))
datanew[datanew$Year==2009,]

##repeatability of rank from year to year 
modrank<-rpt(year.rank~Year + (1|ID), data = datanew, grname = c("ID"))
summary(modrank)
plot(modrank)


##avg_season_NNdist$CalvingGround2[avg_season_NNdist$CalvingGround=="On"]<-1
###avg_season_NNdist$CalvingGround2<-as.integer(avg_season_NNdist$CalvingGround2)
#modswitch<-rpt(CalvingGround2~Year + (1|ID), data = avg_season_NNdist,datatype = "Binary", 
#                    grname = c("ID"),
#                   link = "probit")


##modswitch<-MCMCglmm(CalvingGround2~(1|ID), data = avg_season_NNdist, 
#                    family="zibinomial",
#                    verbose = TRUE,
#                   pr=T,saveX = TRUE,saveZ = TRUE)


## repeatability in calving ground (on/off)

avg_season$CalvingGround2<- ifelse(avg_season$CalvingGround=="On",0,1)

modon.off<-rpt(CalvingGround2~ (1|ID), data = avg_season,datatype = "Binary", 
               grname = c("ID"))

plot(modon.off)
summary(modon.off)

#modswitch1<-rpt(CalvingGround2~ Year + (1|ID), data = avg_season,datatype = "Binary", 
#             grname = c("ID"))

#plot(modswitch1)
#summary(modswitch1)


## calculate distance from global core-periphery 

avg_season$CoreThresh<- with(avg_season, NNdistkm - 17.5512)

## if ID is in the core = 1 if on periphery = 0 

avg_season$Core<- ifelse(avg_season$CoreThresh > 0, 0, 1)
avg_season$Periph<- ifelse(avg_season$CoreThresh < 0, 0, 1)

##repeatability in core/periphery 
modcore<-rpt(Core~ Year +(1|ID), data = avg_season,datatype = "Binary",
            grname = c("ID"))

modcore<-rpt(Core~ (1|ID), data = avg_season,datatype = "Binary",
             grname = c("ID"))

#only those on the calving ground 
ondata<-subset(avg_season, CalvingGround== "On")
View(ondata)

#as a function of year and ID
modcore<- rpt(Core ~ Year + (1|ID), data = ondata, datatype = 'Binary',
              grname = c("ID"))

summary(modcore)
plot(modcore)

#as a function of year and ID
modcore1<- rpt(Core ~ (1|ID), data = ondata, datatype = 'Binary',
               grname = c("ID"))

modcore1<- rpt(Periph ~ (1|ID), data = ondata, datatype = 'Binary',
               grname = c("ID"))

summary(modcore1)
plot(modcore1)



## calculate distance from yearly on/off threshold 

#Create new colum w/ yearly on/off 
library(dplyr)
library(stats)

avg_season$Off.Thresh<-
  ifelse(avg_season$Year==2009, 23.7233,
  ifelse(avg_season$Year==2010, 25.72665,
  ifelse(avg_season$Year==2011, 23.03188,
  ifelse(avg_season$Year==2012, 29.54081,
  ifelse(avg_season$Year==2013, 32.70388,
                                     NA  )))))
str(avg_season)

#Subtract from NNdiskm 
avg_season$Dist.Off <- abs(avg_season$NNdistkm - avg_season$Off.Thresh)                    

### Model Selection 
#example 
mod<-glmer(switch ~ covaritates + (1|ID), data= data,
           family = "binomial")

###determine which individuals switched between on/off in two consuctive years 
# 0 = no switch, 1 = switching event

avg_season$ID <- as.factor(avg_season$ID)
library(data.table)

avg_season$Lag1 <- ave(avg_season$CalvingGround2, avg_season$ID, 
                       FUN= function(x) c(NA, x[-length(x)] )  )
View(avg_season)

avg_season$switch <- abs(avg_season$CalvingGround2 - avg_season$Lag1)

##write the model
avg_season$lag.core <- lag(avg_season$Core,1)
avg_season$lag.off <- lag(avg_season$Dist.Off,1)
avg_season$lag.lost <- lag(avg_season$Lost,1)

# core threshold only
library(AICcmodavg)
library(lme4)
MOD <-glmer(switch ~ lag.core + (1|ID), data = avg_season,
            family = "binomial",
            na.action = na.omit)

AICcmodavg::AICc(MOD, return.K = FALSE, second.ord = TRUE, nobs = NULL)

summary(MOD)
r.squaredGLMM(MOD)

# on/off threshold only
library(lmerTest)
MOD1 <-glmer(switch ~ lag.off + (1|ID) + (1|Year), data = avg_season,
             family = "binomial",
             na.action = na.omit)

AICcmodavg::AICc(MOD1, return.K = FALSE, second.ord = TRUE, nobs = NULL)
summary(MOD1)
rsquared(MOD1)

# calf loss only
MOD2 <-glmer(switch ~ lag.lost + (1|ID), data = avg_season,
             family = "binomial")

AICcmodavg::AICc(MOD2, return.K = FALSE, second.ord = TRUE, nobs = NULL)
summary(MOD2)
rsquared(MOD2)

##  |distance from threshold| and Core/Periphery 
MOD3 <-glmer(switch ~ lag.off + lag.core + (1|ID), data = avg_season,
             family = "binomial")

AICcmodavg::AICc(MOD3, return.K = FALSE, nobs = NULL)
rsquared(MOD3)


## |distance from threshold| and calf loss 
MOD4 <-glmer(switch ~ lag.off + lag.lost + (1|ID), data = avg_season,
             family = "binomial")
AICcmodavg::AICc(MOD4, return.K = FALSE, second.ord = TRUE, nobs = NULL)
rsquared(MOD4)

## core and calf loss 
MOD5 <- glmer(switch ~ lag.core + lag.lost + (1|ID), data = avg_season,
              family = "binomial")
AICcmodavg::AICc(MOD5, return.K = FALSE, second.ord = TRUE, nobs = NULL)
rsquared(MOD5)

## all covariates 
MOD6<- glmer(switch~ lag.core + lag.off + lag.lost + (1|ID), data = avg_season,
             family = "binomial")

AICcmodavg::AICc(MOD6, return.K = FALSE, second.ord = TRUE, nobs = NULL)
rsquared(MOD6)

## Null model 
nullmod<-glmer(switch ~ (1|ID), data= avg_season,
               family = "binomial")

AICcmodavg::AICc(nullmod, return.K = FALSE, second.ord = TRUE, nobs = NULL)
rsquared(nullmod)

##figuring out the switch 

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

#ggplot(avg_season, aes(x = switch, y = Dist.Off)) +
#  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
#  facet_grid(. ~ Year)
library(ggsignif)

ggplot(data=subset(data1, !is.na(Switch)), aes(x = Switch, y = Dist.Off)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, notch = TRUE, notchwidth = 0.5 ) +
  scale_y_continuous(name = "Distance from On/Off Threshold")

ggplot(data=subset(data1, !is.na(Switch)), aes(x = Switch, y = Dist.Off)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, notch = TRUE, notchwidth = 0.5 ) +
  scale_y_continuous(name = "Distance from Threshold (Km)")+
  geom_signif(comparisons = list(c("No", "Yes")), 
              map_signif_level=TRUE)


library(ggpubr)
kruskal.test(Dist.Off ~ Switch, data = data1)

ggplot(data=subset(data1, !is.na(Switch)), aes(x = Switch, y = Dist.Off)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  facet_grid(. ~ Year) +
  scale_y_continuous(name = "Distance from On/Off Threshold")+
  geom_signif(comparisons = list(c("No", "Yes")), 
              map_signif_level=TRUE)

ggplot(switchdata, aes(Year, direction, colour= ID)) +
  geom_point(size = 10) +
  scale_y_continuous(name = "-1 = moved off, 1 = moved on") 

## bar graph 
ggplot(switchdata, aes(Year, direction, fill= ID)) +
  geom_bar(stat="identity", position = "dodge" ) +
  scale_y_continuous(name = "-1 = moved off, 1 = moved on")

##R2 values 

# Install latest version from CRAN
install.packages("piecewiseSEM")
library(piecewiseSEM)
library(devtools)


switch.modlist = list(
  
  glm(switch ~ lag.core + (1|ID), data = avg_season,
      family = binomial(link = logit),
      na.action = na.omit),
  
  glmer(switch ~ lag.off + (1|ID), data = avg_season,
        family = binomial(link = logit),
        na.action = na.omit),
  
  glmer(switch ~ lag.lost + (1|ID), data = avg_season,
        family = binomial(link = logit),
        na.action = na.omit),
  
  glmer(switch ~ lag.off + lag.core + (1|ID), data = avg_season,
        family = binomial(link = logit),
        na.action = na.omit),
  
  glmer(switch ~ lag.off + lag.lost + (1|ID), data = avg_season,
        family = binomial(link = logit),
        na.action = na.omit),
  
  glmer(switch ~ lag.core + lag.lost + (1|ID), data = avg_season,
        family = binomial(link = logit),
        na.action = na.omit),
  
  glmer(switch~ lag.core + lag.off + lag.lost + (1|ID),data = avg_season, 
        family = binomial(link = logit),
        na.action = na.omit)
  
)


sem.model.fits(switch.modlist)

###

ggplot(avg_season, aes(Year, direction, colour= ID)) +
  geom_point(size = 10) +
  scale_y_continuous(name = "-1 = moved off, 1 = moved on") 

ggplot(subset(avg_season,ID %in% c("mr2009a07")), aes(Year, CalvingGround)) + 
  geom_point(size = 10)

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

pdf("/Users/maegwinbonar 1/Dropbox/Vander Wal Lab/CH2data/switch_plot_new.pdf", 
    width = 6.8, height = 4)
ggplot(Switcher, aes(Year, CalvingGround, group = ID)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~ID) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=90, hjust=1), 
        legend.position = 'none',
        plot.title = element_text(size = 20, color = "black"),
        axis.text=element_text(size=10, color = "black"),
        axis.title=element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  ylab(NULL) + 
  xlab(NULL) +
  scale_y_discrete(labels = c('Agg', 'Disagg'))
dev.off()

