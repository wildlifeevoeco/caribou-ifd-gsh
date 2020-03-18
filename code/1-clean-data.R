


### All Locs - DTing ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Collar data
# Outputs: Prepped data

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'lubridate')
lapply(libs, require, character.only = TRUE)

### Set variables ----

# Time zone 
tz <- 'America/St_Johns'

# Max moverate
maxMoveRate <- 30000

### Projection ----
projCols <- c('EASTING', 'NORTHING')
time.col <- 'datetime'

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

### Input raw data ----
DT <- fread('../CaribouSEE/input/AllCaribouDataRaw.csv',
            drop = c("SPECIES", "EPSG_CODE", "Map_Quality",
                     "COLLAR_FILE_ID", "EXCLUDE", "VENDOR_CL", "DOP",
                     "NAV", "VALIDATED", "LOCQUAL", "COLLAR_ID",
                     "AGE", "Fix_Time_Delta", "V1", "FIX_ID"))


### Preprocessing ----
DT[, datetime := ymd_hms(paste(FIX_DATE, FIX_TIME))]
DT[, roundtime := round_date(datetime, unit = "hours")]

DT[, Year := year(datetime)]
DT[, JDate := yday(datetime)]

DT[JDate >= 141 & JDate <= 212, season := "calving"]

DT <- DT[!(is.na(season))]

DT <- DT[SEX == "F" & COLLAR_TYPE_CL == "GPS" & Year >= "2009" &
           HERD == "MIDRIDGE"]

## Loc fields
DT[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]

# Sub by bounding box
DT <- DT[NORTHING > 5250000 & NORTHING < 6000000 &
                 EASTING > 600000 & EASTING < 800000]

# Create lag and dif column names
lag.cols <- paste('lag', projCols, sep = '')
difference.cols <- c('difX', 'difY')

lag.time.col <- paste0('lag', time.col)
dif.time.col <- paste0('dif', time.col)

# Use shift  to create lagged cols
DT[order(get(time.col)), (lag.cols) := data.table::shift(.SD, 1, NA, 'lag'),
      by = .(ANIMAL_ID, Year),
      .SDcols = projCols]

# Find the difference squared between all points in each x,y separately
DT[, (difference.cols) := .((get(projCols[1]) - get(lag.cols[1])) ^2,
                               (get(projCols[2]) - get(lag.cols[2])) ^2)]

# Square root the summed difference for a simple step length
DT[, simpleStep := sqrt(rowSums(.SD)),
      .SDcols = difference.cols]

## Delta Time
DT[order(get(time.col)), (lag.time.col) := data.table::shift(.SD, 1, NA, 'lag'), 
      by = .(ANIMAL_ID, Year),
      .SDcols = time.col]

# difference in time in hours
DT[, (dif.time.col) := as.numeric(get(time.col) - get(lag.time.col), units = 'hours')]

# Simple step length divided by time difference
DT[, moveRate := simpleStep / (get(dif.time.col))]

# Drop more than 30km/hr movements
DT <- DT[moveRate < 50000]

## add IDYr
DT$IDYr <- as.factor(paste(DT$ANIMAL_ID, DT$Year, sep = "_"))

## Export data
saveRDS(DT[,c("ANIMAL_ID", "datetime", "roundtime","Year", 
             "JDate", "EASTING", "NORTHING")], 'output/1-caribou-all.Rds')

message('=== PREP COMPLETE ===')