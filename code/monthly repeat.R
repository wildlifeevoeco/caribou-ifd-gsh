
library(data.table)
library(rptR)

raw <- fread("data/raw_all_NNdist.csv")


raw[, idate := as.IDate(FIX_DATE)]
raw[, itime := as.ITime(FIX_TIME)]
raw[, datetime := as.POSIXct(paste(idate,itime), format = "%Y-%m-%d %H:%M:%S" )]

raw[, JDate := yday(datetime)]
raw[, month := month(datetime)]
raw[, Year := year(datetime)]

rep <- raw[, mean(NNdist), by = c("Year","month" ,"ANIMAL_ID")]

rep$month <- as.factor(rep$month)
rep$avgNN <- rep$V1/1000


rpt(avgNN ~ Year*month + (1|ANIMAL_ID), data = rep, grname = c("ANIMAL_ID"), nboot = 100)
