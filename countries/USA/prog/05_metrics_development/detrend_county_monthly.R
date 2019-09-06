rm(list=ls())

#library(ggplot2)
library(foreign)
library(plyr)
library(pracma)
library(SpecsVerification)
library(tidyr)

#library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric <- as.character(args[4])
type <- as.character(args[5])

# load county summary by day (for single year)
dat.county <- readRDS(paste0('../../output/metrics_development_county/',dname,'/',metric,'_',dname,'/county_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))

# add state names
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
# dat.county = merge(dat.county,state.lookup,by.x='state.fips',by.y='fips')

# find mean of each county over time period
dat.mean = ddply(dat.county,.(state.county.fips,month),summarise,mean=mean(mmean))
dat.mean$state.fips = substr(dat.mean$state.county.fips,1,2)
dat.mean$state.fips = as.numeric(dat.mean$state.fips)
dat.mean = merge(dat.mean,state.lookup,by.x='state.fips',by.y='fips')

# plot boxplot of means
ggplot(data=subset(dat.mean)) +
    geom_boxplot(aes(x=as.factor(month),y=mean)) +
    geom_hline(yintercept=0,alpha=0.5,linetype='dotted') +
    facet_wrap(~full_name)

# find the trend of each county over time period
dat.trend <- ddply(dat.county,.(state.county.fips,month), function(z)coef(summary(lm(mmean ~ year, data=z))))
dat.trend <- dat.trend[!c(TRUE,FALSE),]
dat.trend$state.fips = substr(dat.trend$state.county.fips,1,2)
dat.trend$state.fips = as.numeric(dat.trend$state.fips)

names(dat.trend)[7] = 'p_value'

# plot boxplot of trends
ggplot(data=subset(dat.trend)) +
    geom_boxplot(aes(x=as.factor(month),y=Estimate)) +
    geom_hline(yintercept=0,alpha=0.5,linetype='dotted') +
    facet_wrap(~full_name)

# and without non-significant values
ggplot(data=subset(dat.trend,p_value<0.1)) +
    geom_boxplot(aes(x=as.factor(month),y=Estimate)) +
    geom_hline(yintercept=0,alpha=0.5,linetype='dotted') +
    facet_wrap(~full_name)

# detrend each count-month across time
dat.detrend = ddply(dat.county,.(state.county.fips,month),function(z)Detrend(z$mmean,demean=FALSE))
names(dat.detrend)[c(3:40)] = c(year.start:year.end)
dat.long.detrend = gather(dat.detrend, year, variable, V1:V38, factor_key=TRUE)


# find mean of detrended
dat.detrend.mean = ddply(dat.long.detrend,.(state.county.fips,month),summarise,mean.detrend=mean(variable))

dat.slope = ddply(dat.county,.(state.county.fips,month), )