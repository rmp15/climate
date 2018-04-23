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

year.start = 1980 ; year.end = 2009

print(year)

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

print('data loaded')

####################################################
# 1c. AVERAGE VALUE CENTRED BY LONGTERM NORMAL (chosen period of study)
####################################################

print('processing meanc3')

var <- paste0('meanc3_',dname)

# process for finding average temperature
dat.at <- dat.county
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.at <- ddply(dat.at,.(year,month,state.county.fips),summarize,var.weighted=round(mean(variable),1))

# merge and create weighted mean for state
dat.temp <- merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
temp.state <- na.omit(temp.state)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.mean')

# load multiyear normal for period of study
dat.multi <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/state_longterm_normals_mean_',dname,'_',year.start,'_',year.end,'.rds'))

# establish number of years of study
num.years <- 30

# merge state-month mean values just calculated and subtract multiyear normal
temp.state <- merge(temp.state,dat.multi,by=c('month','state.fips','sex','age'))
names(temp.state)[grep(paste0(dname,'.mean') ,names(temp.state))] <- 'var.adj'
names(temp.state)[grep(paste0(dname,'.',num.years,'yr.mean') ,names(temp.state))] <- paste0(num.years,'yr.mean')
temp.state$var.adj <- with(temp.state,var.adj-`30yr.mean`)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.meanc3')

# ONLY SAVE THE FIRST 6 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
temp.state <- temp.state[,c(1:6)]

# for temporary plotting below
temp.state.2 = temp.state

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

########################################################################################################
# 1d. AVERAGE VALUE CENTRED BY LONGTERM NORMAL BY COUNTY THEN BUILT TO STATE
########################################################################################################

print('processing meanc4')

var <- paste0('meanc4_',dname)

# process for finding average temperature
dat.at <- dat.county
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.at <- ddply(dat.at,.(year,month,state.county.fips),summarize,var.weighted=round(mean(variable),1))

# load multiyear normal for period of study
dat.multi <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_normals_mean_',dname,'_',year.start,'_',year.end,'.rds'))

# establish number of years of study
num.years <- 30

# merge county-month mean values and subtract multiyear normal
temp.county <- merge(dat.at,dat.multi,by=c('month','state.county.fips'))
names(temp.county)[grep(paste0(dname,'.',num.years,'yr.mean') ,names(temp.county))] <- paste0(num.years,'yr.mean')
names(temp.county)[grep(paste0('var.weighted') ,names(temp.county))] <- 'var.adj'
temp.county$var.adj <- with(temp.county,var.adj-`30yr.mean`)
names(temp.county)[grep('var.adj',names(temp.county))] <- paste0(dname,'.meanc4')

# ONLY SAVE THE FIRST 6 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
temp.county <- temp.county[,c(1:4)]

# merge and create weighted mean for state
dat.temp <- merge(temp.county,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,t2m.meanc4=(sum(pop.county*t2m.meanc4)/sum(pop.county)))

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

temp.state.plot = merge(temp.state.2,temp.state)

# TEMPORARY TO EXAMINE VALUES

# combine state average deviation from county-based mean with county deviations from county-based means
dat.compare = merge(dat.temp,temp.state,by=c('year','month','sex','age','state.fips'),all.x=TRUE)
dat.compare$diff = with(dat.compare,t2m.meanc4.x-t2m.meanc4.y)

ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var,'/diff/')), dir.create(paste0("../../output/metrics_development/",dname,'/',var,'/diff/')), FALSE)
saveRDS(dat.compare,paste0("../../output/metrics_development/",dname,'/',var,'/diff/',year))

ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var,'/plots/')), dir.create(paste0("../../output/metrics_development/",dname,'/',var,'/plots/')), FALSE)

library(ggplot2)
pdf(paste0("../../output/metrics_development/",dname,'/',var,'/plots/',year,'.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(dat.compare,age==65&sex==2&!(state.fips%in%c('02','15')))) +
    geom_point(aes(x=state.fips,y=t2m.meanc4.x,size=(pop.weighted))) +
    geom_point(aes(x=state.fips,y=t2m.meanc4.y),color='red',size=2) +
    geom_hline(yintercept=0,linetype='dotted') +
    facet_wrap(~month)

ggplot(data=subset(dat.compare,age==65&sex==2&!(state.fips%in%c('02','15')))) +
    geom_point(aes(x=state.fips,y=diff,size=(pop.weighted))) +
    geom_hline(yintercept=0,linetype='dotted') +
    facet_wrap(~month)

ggplot(data=subset(temp.state.plot,!(state.fips%in%c('02','15'))),aes(x=t2m.meanc3,y=t2m.meanc4)) + facet_wrap(~month) + geom_point() + geom_abline(slope=1)
dev.off()

print(paste0(year,' done'))