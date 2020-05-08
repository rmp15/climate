# Finding weekly long-term averages
# and anomalies based on long-term averages

rm(list=ls())

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(sp)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# names for files
year_start <- as.character(args[1])
year_end <- as.character(args[2])
dname <- as.character(args[3])
freq <- as.character(args[4])
num <- as.character(args[5])
space.res <- as.character(args[6])

# for code testing
# dname <- 't2m' ; freq <- 'daily' ; num <- 'four' ; year_start <- '2010' ; year_end<- '2020' ; space.res='lad'

years = c(year_start:year_end)

# directory to load year files
dir.input = "../../output/grid_county_intersection_raster/lads/"

# load files for all years of long-term average calculation
dat.lads = data.frame()
for(year in years){
    print(paste0(year,' loading'))
    weighted.area.national.total = read.csv(paste0(dir.input,'weighted_area_raster_lads_',dname,'_',freq,'_',as.character(year),'.csv'))
    dat.lads = rbind(dat.lads, weighted.area.national.total)
}

# get rid of pointless columns
dat.lads$X = NULL

# make date column into date format
dat.lads$date = as.Date(dat.lads$date, format="%Y-%m-%d")

# attach week and month ID
library(lubridate)
dat.week = read.csv('~/git/climate/countries/UK/data/weeks_table/day_to_week_table.csv')
dat.week$date = as.Date(dat.week$date, format="%Y-%m-%d")
dat.week$month_of_year = month(dat.week$date)
dat.week$year = year(dat.week$date)
dat.unique = unique(dat.week[c("month_of_year", "year")])
dat.unique$month_id = 1:nrow(dat.unique)
dat.week = merge(dat.week, dat.unique, by=c("year","month_of_year"),all.X=TRUE)
dat.week = dat.week[order(dat.week$date),]
rownames(dat.week) = 1:nrow(dat.week)

# merge temperature and week of year dataframes
dat.lads.week = merge(dat.lads,dat.week,by=c('date'),all.X=TRUE)

# do the scaling thing without FALSE as the second option by week to get anomalies measured against weekly averages
dat.lads.anomaly = ddply(dat.lads.week,.(lad,week_of_year), transform, anomaly.t2m=scale(t2m,scale = FALSE),mean.t2m=mean(t2m))
dat.lads.anomaly = dat.lads.anomaly[order(dat.lads.anomaly$lad,dat.lads.anomaly$date),]
rownames(dat.lads.anomaly) = 1:nrow(dat.lads.anomaly)
dat.lads.anomaly.weekly = ddply(dat.lads.anomaly,.(lad,week_id), summarize, anomaly.t2m.weekly=mean(anomaly.t2m))

# do the scaling thing without FALSE as the second option by month to get anomalies measured against monthly averages
dat.lads.anomaly.2 = ddply(dat.lads.week,.(lad,month_of_year), transform, anomaly=scale(t2m,scale = FALSE),mean.t2m=mean(t2m))
dat.lads.anomaly.2 = dat.lads.anomaly.2[order(dat.lads.anomaly.2$lad,dat.lads.anomaly.2$date),]
rownames(dat.lads.anomaly.2) = 1:nrow(dat.lads.anomaly.2)
dat.lads.anomaly.monthly = ddply(dat.lads.anomaly.2,.(lad,month_id), summarize, anomaly.t2m.monthly=mean(anomaly))

# save dat.lads.anomaly.weekly and dat.lads.anomaly.monthly
write.csv(dat.lads.anomaly.weekly,paste0(dir.input,'weekly_anomalies_weighted_area_raster_lads_',dname,'_',freq,'_',as.character(year_start),'_',as.character(year_end),'.csv'), row.names=FALSE)
write.csv(dat.lads.anomaly.monthly,paste0(dir.input,'monthly_anomalies_weighted_area_raster_lads_',dname,'_',freq,'_',as.character(year_start),'_',as.character(year_end),'.csv'), row.names=FALSE)

# also save dat.lads.anomaly and dat.lads.anomaly.2 for full save in case
write.csv(dat.lads.anomaly,paste0(dir.input,'full_weekly_anomalies_weighted_area_raster_lads_',dname,'_',freq,'_',as.character(year_start),'_',as.character(year_end),'.csv'), row.names=FALSE)
write.csv(dat.lads.anomaly.2,paste0(dir.input,'full_monthly_anomalies_weighted_area_raster_lads_',dname,'_',freq,'_',as.character(year_start),'_',as.character(year_end),'.csv'), row.names=FALSE)
