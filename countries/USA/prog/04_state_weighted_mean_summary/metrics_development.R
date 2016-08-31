rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])

ifelse(!dir.exists("../../output/state_weighted_mean_summary"), dir.create("../../output/state_weighted_mean_summary"), FALSE)

# for each county, load summary by day and then by month (for single year)
dat.county <- readRDS(paste0('../../output/state_weighted_mean_summary/county_daily_',year,'.rds'))

# load weightings by county for state summary based on population
state.weighting <- readRDS('~/data/climate/population_weightings/state_population_weightings.rds')

# filter for a year of interest
year.selected <- year
state.weighting.filter <- subset(state.weighting,year %in% year.selected)

# 1. NUMBER OF DAYS ABOVE A THRESHOLD
var <- 'no_above_threshold'

# CREATE PROCESS FOR dat.th

# merge and create weighted mean for state
dat.temp <-merge(dat.th,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,temp.adj=sum(pop.weighted*temp.weighted))
temp.state <- na.omit(temp.state)

# convert kelvin to degrees
temp.state$temp.cel <- temp.state$temp.adj- 273.15

# save output
saveRDS(dat.wm,paste0('../../output/state_weighted_mean_summary/county_summary_',var,'_',year.selected,'.rds'))
saveRDS(temp.state,paste0('../../output/state_weighted_mean_summary/state_weighted_summary_',var,'_',year.selected,'.rds'))
