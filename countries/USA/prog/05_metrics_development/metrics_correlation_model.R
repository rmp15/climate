# this script
# loads particular metrics of interest
# establishes r^2 values for each state-month
# find average for country for each metric based on state-month values
# plots on a matrix/heat map
# go to correct directory
setwd('~/git/climate/countries/USA/prog/00_bash')

rm(list=ls())

library(ggplot2)
library(RColorBrewer)
library(plyr)
library(INLA)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname.1 <- as.character(args[3])
dname.2 <- as.character(args[4])

# declare variables
year.start = 1979 ; year.end = 2015 ;
dname.1 = 't2m' ; dname.2 = 't2m'

# create output directory
dir = paste0("../../output/metrics_correlation_matrix/",dname.1,"/")
ifelse(!dir.exists(dir), dir.create(dir,recursive=TRUE), FALSE)

# load climate metrics
source('../../data/objects/objects.R')

# load all metrics of interest
i=1 # label counter
dat.complete = data.frame()
for (metric in metrics.matrix) {
    dat.metric = readRDS(paste0('../../output/metrics_development/',dname.1,'/',
                            metric,'_',dname.1,'/state_weighted_summary_',
                            metric,'_',dname.1,'_',year.start,'_',year.end,'.rds'))
    dat.metric = subset(dat.metric,age==85&sex==2)
    dat.metric= with(dat.metric,dat.metric[order(sex,age,state.fips,month),])
    assign(paste0('dat.',i),dat.metric)
    if(i==1){
        dat.complete = dat.metric
    }
    if(i>1){
        dat.complete = merge(dat.complete,dat.metric)
    }
    i=i+1
}

dat.complete$month = as.integer(dat.complete$month)

# eliminate unnecessary columns
dat.complete$sex = NULL ; dat.complete$age = NULL ;
dat.complete$year = NULL ; dat.complete$leap = NULL

# extract unique table of state and months to generate state.month
dat.state.month = unique(dat.complete[,c('month', 'state.fips')])
dat.state.month$month = as.integer(dat.state.month$month)
dat.state.month$state.month = seq(nrow(dat.state.month))

# merge year.month table with population table to create year.month id
dat.complete = merge(dat.complete,dat.state.month, by=c('month','state.fips'),all.x=1)

# define a least-squares regression model in INLA
fml = t2m.meanc3 ~ f(state.month,t2m.10percc3,model='iid')
mod = inla(fml, family="gaussian", data=dat.complete)
