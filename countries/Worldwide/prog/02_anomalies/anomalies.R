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
country.id <- as.character(args[7])

# for code testing
# dname = 't2m' ; freq = 'daily' ; num = 'four' ; year_start = '2010' ; year_end = '2020' ; space.res='1' ; country.id = 'BEL'

years = c(year_start:year_end)

# directory to load year files
# create directory to place output files into
dir.input = paste0("../../output/grid_county_intersection_raster/",country.id,'/adm',space.res,'/')

# load files for all years of long-term average calculation
dat.adm = data.frame()
for(year in years){
    print(paste0(year,' loading'))
    weighted.area.national.total = readRDS(paste0(dir.input,'weighted_area_raster_',country.id,'_',space.res,'_',dname,'_',freq,'_',as.character(year),'.rds'))
    dat.adm = rbind(dat.adm, weighted.area.national.total)
}

# save concatenated files for all years
# also save dat.adm.anomaly and dat.adm.anomaly.2 for full save in case
saveRDS(dat.adm,paste0(dir.input,'weighted_area_raster_',country.id,'_',space.res,'_',dname,'_',freq,'_',as.character(year_start),'_',as.character(year_end),'.rds'))
write.csv(dat.adm,paste0(dir.input,'weighted_area_raster_',country.id,'_',space.res,'_',dname,'_',freq,'_',as.character(year_start),'_',as.character(year_end),'.csv'), row.names=FALSE)

# below to run for anomalies but currently being processed elsewhere
run = 0

if(run==1){
    # make date column into date format
    dat.adm$date = as.Date(dat.adm$date, format="%Y-%m-%d")

    # attach week and month ID
    library(lubridate)
    dat.week = read.csv('~/git/climate/countries/Worldwide/data/weeks_table/day_to_week_table.csv')
    dat.week$date = as.Date(dat.week$date, format="%Y-%m-%d")
    dat.week$month_of_year = month(dat.week$date)
    dat.week$year = year(dat.week$date)
    dat.unique = unique(dat.week[c("month_of_year", "year")])
    dat.unique$month_id = 1:nrow(dat.unique)
    dat.week = merge(dat.week, dat.unique, by=c("year","month_of_year"),all.X=TRUE)
    dat.week = dat.week[order(dat.week$date),]
    rownames(dat.week) = 1:nrow(dat.week)

    # get rid of these as these are legacy variables
    dat.week$week_id = dat.week$month_id = NULL

    # merge temperature and week of year dataframes
    dat.adm.week = merge(dat.adm,dat.week,by=c('date'),all.X=TRUE)

    # column of ID
    names(dat.adm.week)[2] = 'region'
    # names(dat.adm.week)[3] = 'region_name'

    # do the scaling thing without FALSE as the second option by week to get anomalies measured against weekly averages
    dat.adm.anomaly = ddply(dat.adm.week,.(region,week_of_year), transform, anomaly.t2m=round(scale(t2m,scale = FALSE),2),mean.t2m=round(mean(t2m),2))
    dat.adm.anomaly = dat.adm.anomaly[order(dat.adm.anomaly$region,dat.adm.anomaly$date),]
    rownames(dat.adm.anomaly) = 1:nrow(dat.adm.anomaly)
    # dat.adm.anomaly.weekly = ddply(dat.adm.anomaly,.(region,week_id), summarize, anomaly.t2m.weekly=mean(anomaly.t2m))

    # do the scaling thing without FALSE as the second option by month to get anomalies measured against monthly averages
    dat.adm.anomaly.2 = ddply(dat.adm.week,.(region,month_of_year), transform, anomaly.t2m=round(scale(t2m,scale = FALSE),2),mean.t2m=round(mean(t2m),2))
    dat.adm.anomaly.2 = dat.adm.anomaly.2[order(dat.adm.anomaly.2$region,dat.adm.anomaly.2$date),]
    rownames(dat.adm.anomaly.2) = 1:nrow(dat.adm.anomaly.2)
    # dat.adm.anomaly.monthly = ddply(dat.adm.anomaly.2,.(region,month_id), summarize, anomaly.t2m.monthly=mean(anomaly))

    # rename region variable to match the input files for merging with population weights later
    names(dat.adm.anomaly)[2] = names(dat.adm.anomaly.2)[2] = paste0('ID_',space.res)

    # save dat.adm.anomaly.weekly and dat.adm.anomaly.monthly
    # write.csv(dat.adm.anomaly.weekly,paste0(dir.input,'weekly_anomalies_weighted_area_raster_',country.id,'_',space.res,'_',dname,'_',freq,'_',as.character(year_start),'_',as.character(year_end),'.csv'), row.names=FALSE)
    # write.csv(dat.adm.anomaly.monthly,paste0(dir.input,'monthly_anomalies_weighted_area_raster_',country.id,'_',space.res,'_',dname,'_',freq,'_',as.character(year_start),'_',as.character(year_end),'.csv'), row.names=FALSE)

    # also save dat.adm.anomaly and dat.adm.anomaly.2 for full save in case
    write.csv(dat.adm.anomaly,paste0(dir.input,'full_weekly_anomalies_weighted_area_raster_adm_',dname,'_',freq,'_',as.character(year_start),'_',as.character(year_end),'.csv'), row.names=FALSE)
    write.csv(dat.adm.anomaly.2,paste0(dir.input,'full_monthly_anomalies_weighted_area_raster_adm_',dname,'_',freq,'_',as.character(year_start),'_',as.character(year_end),'.csv'), row.names=FALSE)
}

