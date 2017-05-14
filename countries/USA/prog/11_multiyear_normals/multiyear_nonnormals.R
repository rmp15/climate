rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric <- as.character(args[4])

cat('processing for years',year.start,year.end)

ifelse(!dir.exists(paste0("../../output/multiyear_normals/",dname,"/",metric)), dir.create(paste0("../../output/multiyear_normals/",dname,"/",metric), recursive=TRUE), FALSE)

# normal years
years <- year.start:year.end

# dummy dataframe to load into
dat <- data.frame()

# loop to load years which are involved
for(i in years) {

    # load county summary by day (for single year)
    dat.county <- readRDS(paste0('../../output/county_weighted_mean_summary/county_daily/',dname,'/county_daily_',dname,'_',i,'.rds'))

    # fix temperature if loaded
    if(dname=='t2m'){
        dat.county$t2m <- dat.county$t2m - 273.15
    }

    # fix precipitation if loaded
    if(dname=='tp'){
        dat.county$tp <- dat.county$tp * 1000
    }
    
    dat <- rbind(dat,dat.county)
    
}

# load weightings by county for state summary based on population
state.weighting <- readRDS('~/data/climate/population_weightings/state_population_weightings.rds')

# filter for a year of interest
state.weighting.filter <- subset(state.weighting,year %in% years)

# replace name of var
var <- paste0('mean_',dname)

# 90 PERCENTILE (10% ABOVE AND 10% BELOW)

# COUNTY

# process for finding average and 90th percentile upper and lower limits
dat.at <- dat
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.mean <- ddply(dat.at,.(month,state.county.fips),summarize,var.weighted=round(mean(variable),1))
dat.perc <- ddply(dat.at, .(month,state.county.fips),function(x) round(quantile(x$variable,c(0.1,0.9)),1))
dat.at <- merge(dat.mean,dat.perc,by=c('month','state.county.fips'))

# rename
names(dat.at)[grep('var.weighted',names(dat.at))] <- paste0(dname,'.',length(years),'yr.mean')
names(dat.at)[grep('90',names(dat.at))] <- paste0(dname,'.',length(years),'yr.ul')
names(dat.at)[grep('10',names(dat.at))] <- paste0(dname,'.',length(years),'yr.ll')

# save output
saveRDS(dat.at,paste0("../../output/multiyear_normals/",dname,"/",metric,'/county_longterm_nonnormals_',var,'_',year.start,'_',year.end,'.rds'))

# STATE

# process for finding average
dat.at <- dat
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.mean <- ddply(dat.at,.(month,state.county.fips),summarize,var.weighted=round(mean(variable),1))
dat.perc <- ddply(dat.at, .(month,state.county.fips),function(x) round(quantile(x$variable,c(0.1,0.9)),1))
dat.at <- merge(dat.mean,dat.perc,by=c('month','state.county.fips'))

# take weighted mean for state by population of middle year
dat.at$year <- years[round(length(years)/2)]

# merge and create weighted mean and 90th percentile upper and lower limits for state
dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted),var.10=sum(pop.weighted*`10%`),var.90=sum(pop.weighted*`90%`))
temp.state <- na.omit(temp.state)

# rename
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.',length(years),'yr.mean')
names(temp.state)[grep('90',names(temp.state))] <- paste0(dname,'.',length(years),'yr.ul')
names(temp.state)[grep('10',names(temp.state))] <- paste0(dname,'.',length(years),'yr.ll')

# save output
saveRDS(temp.state,paste0("../../output/multiyear_normals/",dname,"/",metric,'/state_longterm_nonnormals_',var,'_',year.start,'_',year.end,'.rds'))

# 95 PERCENTILE (5% ABOVE AND 5% BELOW)

# COUNTY

# process for finding average and 90th percentile upper and lower limits
dat.at <- dat
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.mean <- ddply(dat.at,.(month,state.county.fips),summarize,var.weighted=round(mean(variable),1))
dat.perc <- ddply(dat.at, .(month,state.county.fips),function(x) round(quantile(x$variable,c(0.05,0.95)),1))
dat.at <- merge(dat.mean,dat.perc,by=c('month','state.county.fips'))

# rename
names(dat.at)[grep('var.weighted',names(dat.at))] <- paste0(dname,'.',length(years),'yr.mean')
names(dat.at)[grep('95',names(dat.at))] <- paste0(dname,'.',length(years),'yr.ul')
names(dat.at)[grep('5',names(dat.at))] <- paste0(dname,'.',length(years),'yr.ll')

# save output
saveRDS(dat.at,paste0("../../output/multiyear_normals/",dname,"/",metric,'/county_longterm_95_nonnormals_',var,'_',year.start,'_',year.end,'.rds'))

# STATE

# process for finding average
dat.at <- dat
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.mean <- ddply(dat.at,.(month,state.county.fips),summarize,var.weighted=round(mean(variable),1))
dat.perc <- ddply(dat.at, .(month,state.county.fips),function(x) round(quantile(x$variable,c(0.05,0.95)),1))
dat.at <- merge(dat.mean,dat.perc,by=c('month','state.county.fips'))

# take weighted mean for state by population of middle year
dat.at$year <- years[round(length(years)/2)]

# merge and create weighted mean and 90th percentile upper and lower limits for state
dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted),var.10=sum(pop.weighted*`5%`),var.90=sum(pop.weighted*`95%`))
temp.state <- na.omit(temp.state)

# rename
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.',length(years),'yr.mean')
names(temp.state)[grep('90',names(temp.state))] <- paste0(dname,'.',length(years),'yr.ul')
names(temp.state)[grep('10',names(temp.state))] <- paste0(dname,'.',length(years),'yr.ll')

# save output
saveRDS(temp.state,paste0("../../output/multiyear_normals/",dname,"/",metric,'/state_longterm_95_nonnormals_',var,'_',year.start,'_',year.end,'.rds'))


