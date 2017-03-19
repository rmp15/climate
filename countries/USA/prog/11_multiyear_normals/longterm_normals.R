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

ifelse(!dir.exists(paste0("../../output/longterm_normals/",dname,"/",metric)), dir.create(paste0("../../output/longterm_normals/",dname,"/",metric), recursive=TRUE), FALSE)

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

# process for finding average
dat.at <- dat.county
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.at <- ddply(dat.at,.(month,state.county.fips),summarize,var.weighted=round(mean(variable),1),var.sd=round(sd(variable),1))

# take weighted mean for state by population of middle year
dat.at$year <- years[round(length(years)/2)]

# merge and create weighted mean for state
dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted),var.sd=sum(pop.weighted*var.sd))
temp.state <- na.omit(temp.state)

# get 99 percentiles from assuming normal dist
temp.state$ul <- temp.state$var.adj + 2.326 * temp.state$var.sd
temp.state$ll <- temp.state$var.adj - 2.326 * temp.state$var.sd

# rename
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.',length(years),'yr.mean')
names(temp.state)[grep('var.sd',names(temp.state))] <- paste0(dname,'.',length(years),'yr.sd')
names(temp.state)[grep('ul',names(temp.state))] <- paste0(dname,'.',length(years),'yr.ul')
names(temp.state)[grep('ll',names(temp.state))] <- paste0(dname,'.',length(years),'yr.ll')

# save output
saveRDS(temp.state,paste0("../../output/longterm_normals/",dname,"/",metric,'/state_longterm_normals_',var,'_',year.start,'_',year.end,'.rds'))

