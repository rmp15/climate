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
type <- as.character(args[5])

year.start = 1980 ; year.end = 2009

ifelse(!dir.exists(paste0("../../output/metrics_development/",dname)), dir.create(paste0("../../output/metrics_development/",dname)), FALSE)

# load county summary by day (for single year)
if(type=='mean'){
    dat.county <- readRDS(paste0('../../output/county_weighted_mean_summary/county_daily/',dname,'/county_daily_',dname,'_',year,'.rds'))
}
if(type%in%c('min','max')){
    dat.county <- readRDS(paste0('../../output/county_weighted_mean_summary/county_daily/',dname,'/county_daily_',dname,'_',year,'_',type,'.rds'))
}

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
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'_',type,'.rds'))

####################################################
# 1a. AVERAGE VALUE CENTRED BY LONGTERM NORMAL (1986-2005)
####################################################
# var <- paste0('meanc_',dname)
#
# # process for finding average temperature
# dat.at <- dat.county
# names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
# dat.at <- ddply(dat.at,.(year,month,state.county.fips),summarize,var.weighted=round(mean(variable),1))
#
# # merge and create weighted mean for state
# dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
# temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
# temp.state <- na.omit(temp.state)
# names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.mean')
#
# # load multiyear normal for 1986-2005
# dat.multi <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/state_longterm_normals_mean_',dname,'_1986_2005.rds'))
#
# # merge state-month mean values just calculated and subtract multiyear normal
# temp.state <- merge(temp.state,dat.multi,by=c('month','state.fips','sex','age'))
# names(temp.state)[grep(paste0(dname,'.mean') ,names(temp.state))] <- 'var.adj'
# names(temp.state)[grep(paste0(dname,'.20yr.mean') ,names(temp.state))] <- '20yr.mean'
# temp.state$var.adj <- with(temp.state,var.adj-`20yr.mean`)
# names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.meanc')
#
# # ONLY SAVE THE FIRST 6 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
# temp.state <- temp.state[,c(1:6)]
#
# # save output
# ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
# saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################
# 1c. AVERAGE VALUE CENTRED BY LONGTERM NORMAL (chosen period of study)
####################################################

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
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'_',type,'.rds'))

