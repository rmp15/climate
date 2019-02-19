rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric <- as.character(args[4])

# load mean max and min
dat.mean = readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat.min = readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'_min.rds'))
dat.max = readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'_max.rds'))

# restrict to one age and sex
dat.mean = subset(dat.mean,sex==2&age==65)
dat.min = subset(dat.min,sex==2&age==65)
dat.max = subset(dat.max,sex==2&age==65)

# merge datasets
dat.merged = merge(dat.mean,dat.min, by=c('year','month','state.fips','sex','age'),all.x=TRUE)
dat.merged = merge(dat.merged,dat.max, by=c('year','month','state.fips','sex','age'))

# TEMPORARILY restrict to just 1979-2015 (weird problem with 2016)
dat.merged = subset(dat.merged,year%in%c(seq(1979,2015)))

# get rid of age and sex
dat.merged$age = dat.merged$sex = NULL



# plots
ggplot(data=dat.merged) +
geom_point(aes(x=t2m.mean,y=t2m.min))

ggplot(data=dat.merged) +
geom_point(aes(x=t2m.mean,y=t2m.max))
