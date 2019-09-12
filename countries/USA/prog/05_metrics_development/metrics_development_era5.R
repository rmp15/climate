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

year.start = 1980 ; year.end = 2017

ifelse(!dir.exists(paste0("../../output/metrics_development_era5/",dname)), dir.create(paste0("../../output/metrics_development_era5/",dname),recursive=TRUE), FALSE)

# load county summary by day (for single year)
dat.county <- readRDS(paste0('../../output/county_weighted_mean_summary/county_daily_era5/',dname,'/county_daily_',dname,'_',year,'.rds'))

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
# var <- paste0('mean_',dname)
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
# # save output
# ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
# saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

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


####################################################
# 1b. AVERAGE VALUE CENTRED BY LONGTERM NORMAL (entire period of study)
####################################################

# var <- paste0('meanc2_',dname)
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
# # load multiyear normal for period of study
# dat.multi <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/state_longterm_normals_mean_',dname,'_1980_2013.rds'))
#
# # establish number of years of study
# num.years <- 34
#
# # merge state-month mean values just calculated and subtract multiyear normal
# temp.state <- merge(temp.state,dat.multi,by=c('month','state.fips','sex','age'))
# names(temp.state)[grep(paste0(dname,'.mean') ,names(temp.state))] <- 'var.adj'
# names(temp.state)[grep(paste0(dname,'.',num.years,'yr.mean') ,names(temp.state))] <- paste0(num.years,'yr.mean')
# temp.state$var.adj <- with(temp.state,var.adj-`32yr.mean`)
# names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.meanc2')
#
# # ONLY SAVE THE FIRST 6 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
# temp.state <- temp.state[,c(1:6)]
#
# # save output
# ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
# saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 1c. AVERAGE VALUE CENTRED BY LONGTERM NORMAL (chosen period of study)
####################################################
#
# var <- paste0('meanc3_',dname)
#
# # process for finding average temperature
# dat.at <- dat.county
# names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
# dat.at <- ddply(dat.at,.(year,month,state.county.fips),summarize,var.weighted=round(mean(variable),1))
#
# # merge and create weighted mean for state
# dat.temp <- merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
# temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
# temp.state <- na.omit(temp.state)
# names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.mean')
#
# # load multiyear normal for period of study
# dat.multi <- readRDS(paste0('../../output/multiyear_normals_era5/',dname,'/mean/state_longterm_nonnormals_mean_',dname,'_',year.start,'_',year.end,'.rds'))
# names(dat.multi) = c('month','state.fips','sex','age','t2m.10yr.mean','t2m.10yr.ll','t2m.10yr.ul')
#
# # establish number of years of study
# num.years <- 30
#
# # merge state-month mean values just calculated and subtract multiyear normal
# temp.state <- merge(temp.state,dat.multi,by=c('month','state.fips','sex','age'))
# names(temp.state)[grep(paste0(dname,'.mean') ,names(temp.state))] <- 'var.adj'
# # names(temp.state)[grep(paste0(dname,'.',num.years,'yr.mean') ,names(temp.state))] <- paste0(num.years,'yr.mean')
# temp.state$var.adj <- with(temp.state,var.adj-t2m.10yr.mean)
# names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.meanc3')
#
# # ONLY SAVE THE FIRST 6 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
# temp.state <- temp.state[,c(1:6)]
#
# # for temporary plotting below
# temp.state.2 = temp.state
#
# # save output
# ifelse(!dir.exists(paste0("../../output/metrics_development_era5/",dname,'/',var)), dir.create(paste0("../../output/metrics_development_era5/",dname,'/',var)), FALSE)
# saveRDS(temp.state,paste0('../../output/metrics_development_era5/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

########################################################################################################
# 1d. AVERAGE VALUE CENTRED BY LONGTERM NORMAL BY COUNTY THEN BUILT TO STATE
########################################################################################################

var <- paste0('meanc4_',dname)

# process for finding average temperature
dat.at <- dat.county
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.at <- ddply(dat.at,.(year,month,state.county.fips),summarize,var.weighted=round(mean(variable),1))

# load multiyear normal for period of study
dat.multi <- readRDS(paste0('../../output/multiyear_normals_era5/',dname,'/mean/county_longterm_nonnormals_mean_',dname,'_',year.start,'_',year.end,'.rds'))
names(dat.multi) = c('month','state.county.fips','t2m.10yr.mean','t2m.10yr.ll','t2m.10yr.ul')
# establish number of years of study
num.years <- 10

# merge county-month mean values and subtract multiyear normal
temp.county <- merge(dat.at,dat.multi,by=c('month','state.county.fips'))
names(temp.county)[grep(paste0(dname,'.',num.years,'yr.mean') ,names(temp.county))] <- paste0(num.years,'yr.mean')
names(temp.county)[grep(paste0('var.weighted') ,names(temp.county))] <- 'var.adj'
temp.county$var.adj <- with(temp.county,var.adj-`10yr.mean`)
names(temp.county)[grep('var.adj',names(temp.county))] <- paste0(dname,'.meanc4')

# ONLY SAVE THE FIRST 4 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
temp.county <- temp.county[,c(1:4)]

# merge and create weighted mean for state
dat.temp <- merge(temp.county,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,t2m.meanc4=(sum(pop.county*t2m.meanc4)/sum(pop.county)))

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development_era5/",dname,'/',var)), dir.create(paste0("../../output/metrics_development_era5/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development_era5/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

# temp.state.plot = merge(temp.state.2,temp.state)
#
# library(ggplot2)
# pdf(paste0("../../output/metrics_development_era5/",dname,'/',var,'/plots/',year,'.pdf'),height=0,width=0,paper='a4r')
# ggplot(data=subset(temp.state.plot,age==0&sex==2&!(state.fips%in%c('02','15','32')))) +
#     geom_point(aes(x=t2m.meanc3,y=t2m.meanc4)) +
#     geom_abline() +
#     facet_wrap(~month)
#
# ggplot(data=subset(temp.state.plot,age==65&sex==2&!(state.fips%in%c('02','15','32')))) +
#     geom_point(aes(x=t2m.meanc3,y=t2m.meanc4)) +
#     geom_abline() +
#     facet_wrap(~state.fips)
#
# ggplot(data=subset(temp.state.plot,age==65&sex==2&!(state.fips%in%c('02','15','32'))),aes(x=t2m.meanc3,y=t2m.meanc4)) +
#     facet_wrap(~month) + geom_point() +
#     geom_abline(slope=1)
# dev.off()

####################################################
# 2a. 10TH PERCENTILE VALUE CENTRED BY LONGTERM NORMAL (1986-2005)
####################################################
#
# var <- paste0('10percc_',dname)
#
# # process for finding average temperature
# dat.at <- dat.county
# names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
# dat.at <- ddply(dat.at,.(year,month,state.county.fips),function(x) round(quantile(x$variable,c(0.1)),1))
#
# # rename
# names(dat.at)[grep('10',names(dat.at))] <- 'var.weighted'
#
# # merge and create weighted mean for state
# dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
# temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
# temp.state <- na.omit(temp.state)
# names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.10perc')
#
# # load 10th percentile data for state
# dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/state_longterm_nonnormals_mean_t2m_1986_2005.rds'))
#
# # establish number of years of study
# num.years <- 20
#
# # merge state-month 10th percentile values just calculated and subtract multiyear 90th percentile
# temp.state <- merge(temp.state,dat.perc,by=c('month','state.fips','sex','age'))
# names(temp.state)[grep(paste0(dname,'.10perc') ,names(temp.state))] <- 'var.adj'
# names(temp.state)[grep(paste0(dname,'.',num.years,'yr.ll') ,names(temp.state))] <- paste0('ll')
# temp.state$var.adj <- with(temp.state,var.adj-ll)
# names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.10percc')
#
# # ONLY SAVE THE FIRST 6 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
# temp.state <- temp.state[,c(1:6)]
#
# # save output
# ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
# saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))
#
# ####################################################
# # 2b. 10TH PERCENTILE VALUE CENTRED BY LONGTERM NORMAL (entire period of study)
# ####################################################
#
# var <- paste0('10percc2_',dname)
#
# # process for finding average temperature
# dat.at <- dat.county
# names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
# dat.at <- ddply(dat.at,.(year,month,state.county.fips),function(x) round(quantile(x$variable,c(0.1)),1))
#
# # rename
# names(dat.at)[grep('10',names(dat.at))] <- 'var.weighted'
#
# # merge and create weighted mean for state
# dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
# temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
# temp.state <- na.omit(temp.state)
# names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.10perc')
#
# # load 10th percentile data for state
# dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/state_longterm_nonnormals_mean_t2m_1980_2013.rds'))
#
# # establish number of years of study
# num.years <- 34
#
# # merge state-month 10th percentile values just calculated and subtract multiyear 90th percentile
# temp.state <- merge(temp.state,dat.perc,by=c('month','state.fips','sex','age'))
# names(temp.state)[grep(paste0(dname,'.10perc') ,names(temp.state))] <- 'var.adj'
# names(temp.state)[grep(paste0(dname,'.',num.years,'yr.ll') ,names(temp.state))] <- paste0('ll')
# temp.state$var.adj <- with(temp.state,var.adj-ll)
# names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.10percc2')
#
# # ONLY SAVE THE FIRST 6 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
# temp.state <- temp.state[,c(1:6)]
#
# # save output
# ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
# saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

# ####################################################
# # 2c. 10TH PERCENTILE VALUE CENTRED BY LONGTERM NORMAL (chosen period of study)
# ####################################################
#
# var <- paste0('10percc3_',dname)
#
# # process for finding average temperature
# dat.at <- dat.county
# names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
# dat.at <- ddply(dat.at,.(year,month,state.county.fips),function(x) round(quantile(x$variable,c(0.1)),1))
#
# # rename
# names(dat.at)[grep('10',names(dat.at))] <- 'var.weighted'
#
# # merge and create weighted mean for state
# dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
# temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
# temp.state <- na.omit(temp.state)
# names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.10percc3')
#
# # load 10th percentile data for state
# dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/state_longterm_nonnormals_mean_t2m_',year.start,'_',year.end,'.rds'))
#
# # establish number of years of study
# num.years <- 30
#
# # merge state-month 10th percentile values just calculated and subtract multiyear 90th percentile
# temp.state <- merge(temp.state,dat.perc,by=c('month','state.fips','sex','age'))
# names(temp.state)[grep(paste0(dname,'.10perc') ,names(temp.state))] <- 'var.adj'
# names(temp.state)[grep(paste0(dname,'.',num.years,'yr.ll') ,names(temp.state))] <- paste0('ll')
# temp.state$var.adj <- with(temp.state,var.adj-ll)
# names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.10percc3')
#
# # ONLY SAVE THE FIRST 6 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
# temp.state <- temp.state[,c(1:6)]
#
# # save output
# ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
# saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))
