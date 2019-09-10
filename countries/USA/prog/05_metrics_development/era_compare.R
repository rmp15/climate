rm(list=ls())

#library(ggplot2)
library(foreign)
library(plyr)
#library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])
dname <- as.character(args[2])
year.start <- as.numeric(args[3])
year.end <- as.numeric(args[4])

# year.start = 1980 ; year.end = 2009

ifelse(!dir.exists(paste0("../../output/metrics_development/",dname)), dir.create(paste0("../../output/metrics_development/",dname)), FALSE)

# load county summary by day (for single year) for ERA-Interim and ERA5
dat.county.erai <- readRDS(paste0('../../output/county_weighted_mean_summary/county_daily/',dname,'/county_daily_',dname,'_',year,'.rds'))
dat.county.era5 <- readRDS(paste0('../../output/county_weighted_mean_summary/county_daily_era5/',dname,'/county_daily_',dname,'_',year,'.rds'))

# fix temperature if loaded
if(dname=='t2m'){
dat.county.erai$t2m <- dat.county.erai$t2m - 273.15
dat.county.era5$t2m <- dat.county.era5$t2m - 273.15

}

# create some monthly averages
dat.county.erai.monthly = ddply(dat.county.erai,.(year,month,state.county.fips),summarize,t2m=mean(t2m))
dat.county.era5.monthly = ddply(dat.county.era5,.(year,month,state.county.fips),summarize,t2m=mean(t2m))

# merge
dat.compare = merge(dat.county.erai.monthly,dat.county.era5.monthly,by=c('year','month','state.county.fips'))
names(dat.compare)[c(4,5)] = c('ERAInterim','ERA5')
dat.compare$state = substr(dat.compare$state.county.fips,1,2)


library(ggplot2)

pdf('~/Desktop/plots_era_compare_1980.pdf')
ggplot(data=subset(dat.compare,!(state%in%c('02','15')))) +
    geom_point(aes(x=ERAInterim,y=ERA5)) +
    coord_equal() +
    geom_abline()

ggplot(data=subset(dat.compare,!(state%in%c('02','15')))) +
    geom_point(aes(x=ERAInterim,y=ERA5)) +
    facet_wrap(~state) +
    coord_equal() +
    geom_abline()
dev.off()