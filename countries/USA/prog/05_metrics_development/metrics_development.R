rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])
dname <- as.character(args[2])

ifelse(!dir.exists(paste0("../../output/metrics_development/",dname)), dir.create(paste0("../../output/metrics_development/",dname)), FALSE)

# load county summary by day (for single year)
dat.county <- readRDS(paste0('../../output/county_weighted_mean_summary/county_daily/',dname,'/county_daily_',dname,'_',year,'.rds'))

# fix temperature if loaded
if(dname=='t2m'){
dat.county$t2m <- dat.county$t2m - 273.15
}

# load weightings by county for state summary based on population
state.weighting <- readRDS('~/data/climate/population_weightings/state_population_weightings.rds')

# filter for a year of interest
year.selected <- year
state.weighting.filter <- subset(state.weighting,year %in% year.selected)

####################################################
# 1. AVERAGE VALUE
####################################################
var <- paste0('mean_',dname)

# process for finding average temperature
dat.at <- dat.county
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.at <- ddply(dat.at,.(year,month,state.county.fips),summarize,var.weighted=round(mean(variable),1))

# merge and create weighted mean for state
dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
temp.state <- na.omit(temp.state)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.mean')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 2. NUMBER OF DAYS ABOVE A THRESHOLD
####################################################
threshold.upper <- 30
var <- paste0('days_above_',threshold.upper,'_',dname)

# process for counting days above threshold
# do i need to make days above threshold as a proportion of number of days in month?
dat.th.up <- dat.county
names(dat.th.up)[grep(dname,names(dat.th.up))] <- 'variable'
dat.th.up$count <- ifelse(dat.th.up$variable> threshold.upper,1,0)
dat.th.up <- ddply(dat.th.up,.(year,month,state.county.fips),summarize,days.above.threshold=sum(count))

# merge and create weighted mean for state
dat.temp <-merge(dat.th.up,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,days.above.threshold=sum(pop.weighted*days.above.threshold))
temp.state <- na.omit(temp.state)

# round (is this right?)
temp.state$days.above.threshold <- round(temp.state$days.above.threshold)
names(temp.state)[grep('days.above.threshold',names(temp.state))] <- paste0(dname,'.da.',threshold.upper)

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 3. NUMBER OF DAYS BELOW A THRESHOLD
####################################################
threshold.lower <- 10
var <- paste0('days_below_',threshold.lower,'_',dname)

# process for counting days below threshold
# do i need to make days above threshold as a proportion of number of days in month?
dat.th.do <- dat.county
names(dat.th.do)[grep(dname,names(dat.th.do))] <- 'variable'
dat.th.do$count <- ifelse(dat.th.do$variable<threshold.lower,1,0)
dat.th.do <- ddply(dat.th.do,.(year,month,state.county.fips),summarize,days.below.threshold=sum(count))

# merge and create weighted mean for state
dat.temp <-merge(dat.th.do,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,days.below.threshold=sum(pop.weighted*days.below.threshold))
temp.state <- na.omit(temp.state)

# round (is this right?)
temp.state$days.below.threshold <- round(temp.state$days.below.threshold)
names(temp.state)[grep('days.below.threshold',names(temp.state))] <- paste0(dname,'.db.',threshold.upper)

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 4. STANDARD DEVIATION
####################################################
var <- paste0('sd_',dname)

# process for finding sd of temperature
dat.sd <- dat.county
names(dat.sd)[grep(dname,names(dat.sd))] <- 'variable'
dat.sd <- ddply(dat.sd,.(year,month,state.county.fips),summarize,var.weighted=round(sd(variable),1))

# merge and create weighted mean for state
dat.temp <-merge(dat.sd,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
temp.state <- na.omit(temp.state)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.sd')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 5. NUMBER OF DAYS CHANGED BY A VALUE FROM DAY BEFORE
####################################################
threshold <- 5
var <- paste0('days_changing_by_',threshold,'_',dname)

# process for counting days changing by threshold
# do i need to make days above threshold as a proportion of number of days in month?
dat.ch <- dat.county
names(dat.ch)[grep(dname,names(dat.ch))] <- 'variable'
dat.ch <- ddply(dat.ch, .(month,year,state.county.fips), transform, diff=c(0,diff(variable)))
#dat.count <- ddply(dat.ch,.(month,year,state.county.fips),summarize,over=sum(diff>threshold),under=sum(diff<(-1*threshold)),from=sum(abs(diff)>threshold))
dat.ch <- ddply(dat.ch,.(month,year,state.county.fips),summarize,day.changed.by.threshold=sum(abs(diff)>threshold))

# merge and create weighted mean for state
dat.temp <-merge(dat.ch,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*day.changed.by.threshold))
temp.state <- na.omit(temp.state)

# round (is this right?)
temp.state$var.adj <- round(temp.state$var.adj)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.dcb.',threshold)

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 6. NUMBER OF UPWAVES
####################################################
threshold <- 5
num.days <- 3
var <- paste0('number_of_',num.days,'_day_',threshold,'_delta_',dname,'.waves')

# process for counting number of upwaves
# do i need to make number of upwaves as a proportion of number of days in month?
dat.uw <- dat.county
names(dat.uw)[grep(dname,names(dat.uw))] <- 'variable'



####################################################
# 7. NUMBER OF DOWNWAVES
####################################################



