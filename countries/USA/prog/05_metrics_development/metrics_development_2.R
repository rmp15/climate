rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])
dname <- as.character(args[2])
year.start <- as.numeric(args[3])
year.end <- as.numeric(args[4])

ifelse(!dir.exists(paste0("../../output/metrics_development/",dname)), dir.create(paste0("../../output/metrics_development/",dname)), FALSE)

# load county summary by day (for single year)
dat.county <- readRDS(paste0('../../output/county_weighted_mean_summary/county_daily/',dname,'/county_daily_',dname,'_',year,'.rds'))

# fix temperature if loaded
if(dname=='t2m'){
dat.county$t2m <- dat.county$t2m - 273.15
}

# fix precipitation if loaded
if(dname=='tp'){
    dat.county$tp <- dat.county$tp * 1000
}

# load weightings by county for state summary based on population
state.weighting <- readRDS('~/data/climate/population_weightings/state_population_weightings.rds')

# filter for a year of interest
year.selected <- year
state.weighting.filter <- subset(state.weighting,year %in% year.selected)

# leap year test
is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

dat.county$leap <- as.integer(is.leapyear(dat.county$year))

####################################################
# 1d. 90TH PERCENTILE VALUE CENTRED BY LONGTERM NORMAL (chosen period of study)
####################################################

var <- paste0('90percc_',dname)

# process for finding average temperature
dat.at <- dat.county
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.at <- ddply(dat.at,.(year,month,state.county.fips),function(x) round(quantile(x$variable,c(0.9)),1))

# rename
names(dat.at)[grep('90',names(dat.at))] <- 'var.weighted'

# merge and create weighted mean for state
dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
temp.state <- na.omit(temp.state)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.90perc')

# load 90th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/state_longterm_nonnormals_mean_t2m_',year.start,'_',year.end,'.rds'))

# establish number of years of study
num.years <- length(year.start:year.end)

# merge state-month 90th percentile values just calculated and subtract multiyear 90th percentile
temp.state <- merge(temp.state,dat.perc,by=c('month','state.fips','sex','age'))
names(temp.state)[grep(paste0(dname,'.90perc') ,names(temp.state))] <- 'var.adj'
names(temp.state)[grep(paste0(dname,'.',num.years,'yr.ul') ,names(temp.state))] <- paste0('ul')
temp.state$var.adj <- with(temp.state,var.adj-ul)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.90percc')

# ONLY SAVE THE FIRST 6 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
temp.state <- temp.state[,c(1:6)]

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))


