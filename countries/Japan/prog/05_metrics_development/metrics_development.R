rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])
dname <- as.character(args[2])

ifelse(!dir.exists(paste0("../../output/metrics_development/",dname)), dir.create(paste0("../../output/metrics_development/",dname), recursive=TRUE), FALSE)

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
#state.weighting <- readRDS('~/data/climate/population_weightings/state_population_weightings.rds')

# filter for a year of interest
year.selected <- year
#state.weighting.filter <- subset(state.weighting,year %in% year.selected)

####################################################
# 1. AVERAGE VALUE
####################################################
var <- paste0('mean_',dname)

# process for finding average temperature
dat.at <- dat.county
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.at <- ddply(dat.at,.(year,month,state_id),summarize,var.weighted=round(mean(variable),1))

# merge and create weighted mean for state
temp.state <- dat.at
#temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
#temp.state <- na.omit(temp.state)
names(temp.state)[grep('var.weighted',names(temp.state))] <- paste0(dname,'.mean')

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
dat.th.up <- ddply(dat.th.up,.(year,month,state_id),summarize,days.above.threshold=sum(count))

# merge and create weighted mean for state
temp.state <- dat.th.up
#temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,days.above.threshold=sum(pop.weighted*days.above.threshold))
#temp.state <- na.omit(temp.state)

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
dat.th.do <- ddply(dat.th.do,.(year,month,state_id),summarize,days.below.threshold=sum(count))

# merge and create weighted mean for state
temp.state <- dat.th.do
#temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,days.below.threshold=sum(pop.weighted*days.below.threshold))
#temp.state <- na.omit(temp.state)

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
dat.sd <- ddply(dat.sd,.(year,month,state_id),summarize,var.weighted=round(sd(variable),1))

# merge and create weighted mean for state
temp.state <- dat.sd
#temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
#temp.state <- na.omit(temp.state)
names(temp.state)[grep('var.weighted',names(temp.state))] <- paste0(dname,'.sd')

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
dat.ch <- ddply(dat.ch, .(month,year,state_id), transform, diff=c(0,diff(variable)))
dat.ch <- ddply(dat.ch,.(month,year,state_id),summarize,day.changed.by.threshold=sum(abs(diff)>threshold))

# merge and create weighted mean for state
temp.state <- dat.ch
#temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*day.changed.by.threshold))
#temp.state <- na.omit(temp.state)

# round (is this right?)
#temp.state$var.adj <- round(temp.state$var.adj)
names(temp.state)[grep('day.changed.by.threshold',names(temp.state))] <- paste0(dname,'.dcb.',threshold)

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 6. NUMBER OF DAYS INCREASING BY A VALUE FROM DAY BEFORE
####################################################
threshold <- 5
var <- paste0('days_increasing_by_',threshold,'_',dname)

# process for counting days changing by threshold
# do i need to make days above threshold as a proportion of number of days in month?
dat.ch <- dat.county
names(dat.ch)[grep(dname,names(dat.ch))] <- 'variable'
dat.ch <- ddply(dat.ch, .(month,year,state_id), transform, diff=c(0,diff(variable)))
#dat.count <- ddply(dat.ch,.(month,year,state.county.fips),summarize,over=sum(diff>threshold),under=sum(diff<(-1*threshold)),from=sum(abs(diff)>threshold))
dat.ch <- ddply(dat.ch,.(month,year,state_id),summarize,day.increased.by.threshold=sum(diff>threshold))

# merge and create weighted mean for state
temp.state <- dat.ch
#temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*day.increased.by.threshold))
#temp.state <- na.omit(temp.state)

# round (is this right?)
#temp.state$var.adj <- round(temp.state$var.adj)
names(temp.state)[grep('day.increased.by.threshold',names(temp.state))] <- paste0(dname,'.dib.',threshold)

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 7. NUMBER OF DAYS DECREASING BY A VALUE FROM DAY BEFORE
####################################################
threshold <- 5
var <- paste0('days_decreasing_by_',threshold,'_',dname)

# process for counting days changing by threshold
# do i need to make days above threshold as a proportion of number of days in month?
dat.ch <- dat.county
names(dat.ch)[grep(dname,names(dat.ch))] <- 'variable'
dat.ch <- ddply(dat.ch, .(month,year,state_id), transform, diff=c(0,diff(variable)))
#dat.count <- ddply(dat.ch,.(month,year,state.county.fips),summarize,over=sum(diff>threshold),under=sum(diff<(-1*threshold)),from=sum(abs(diff)>threshold))
dat.ch <- ddply(dat.ch,.(month,year,state_id),summarize,day.decreased.by.threshold=sum(diff<(-1*threshold)))

# merge and create weighted mean for state
temp.state <-dat.ch
#temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*day.decreased.by.threshold))
#temp.state <- na.omit(temp.state)

# round (is this right?)
#temp.state$var.adj <- round(temp.state$var.adj)
names(temp.state)[grep('day.decreased.by.threshold',names(temp.state))] <- paste0(dname,'.ddb.',threshold)

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 8. NUMBER OF UPWAVES 1(ABSOLUTE THRESHOLD) FIX
####################################################
threshold <- 25
num.days <- 3
var <- paste0('number_of_min_',num.days,'_day_above_',threshold,'_upwaves_',dname)

# process for counting number of upwaves
# do i need to make number of upwaves as a proportion of number of days in month?
dat.uw <- dat.county
names(dat.uw)[grep(dname,names(dat.uw))] <- 'variable'
dat.uw$above.threshold <- ifelse(dat.uw$variable>threshold,1,0)
dat.uw <- ddply(dat.uw, .(month,year,state_id), summarize, up.waves=length(rle(above.threshold)$lengths[rle(above.threshold)$values==1 & rle(above.threshold)$lengths>=num.days]))

# merge and create weighted mean for state
temp.state <- dat.uw
#temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*up.waves))
#temp.state <- na.omit(temp.state)

# round (is this right?)
temp.state$up.waves <- round(temp.state$up.waves)
names(temp.state)[grep('up.waves',names(temp.state))] <- paste0(dname,'.uwo.',threshold,'.',num.days,'d')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 9. NUMBER OF DOWNWAVES 1 (ABSOLUTE THRESHOLD) FIX
####################################################
threshold <- 5
num.days <- 3
var <- paste0('number_of_min_',num.days,'_day_below_',threshold,'_downwaves_',dname)

# process for counting number of downwaves
# do i need to make number of upwaves as a proportion of number of days in month?
dat.dw <- dat.county
names(dat.dw)[grep(dname,names(dat.dw))] <- 'variable'
dat.dw$below.threshold <- ifelse(dat.dw$variable<threshold,1,0)
dat.dw <- ddply(dat.dw, .(month,year,state_id), summarize, down.waves=length(rle(below.threshold)$lengths[rle(below.threshold)$values==1 & rle(below.threshold)$lengths>=num.days]))

# merge and create weighted mean for state
temp.state <- dat.dw
#temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*down.waves))
#temp.state <- na.omit(temp.state)

# round (is this right?)
temp.state$down.waves <- round(temp.state$down.waves)
names(temp.state)[grep('down.waves',names(temp.state))] <- paste0(dname,'.dwu.',threshold,'.',num.days,'d')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 10. NUMBER OF UPWAVES 2
####################################################

# USE LANCET COUNTDOWN DATA

# THIS IS UPWAVES BY USING A JUMP (SAY UP BY 5 FROM DAY BEFORE)
# AND THEN HOW LONG IT SUSTAINS IT

####################################################
# 11. NUMBER OF DOWNWAVES 2
####################################################

# THIS IS DOWNWAVES BY USING A JUMP (SAY DOWN BY 5 FROM DAY BEFORE)
# AND THEN HOW LONG IT SUSTAINS IT


