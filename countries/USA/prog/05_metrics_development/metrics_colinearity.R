# this script
# loads 2 particular metrics
# plots them against each other
# establishes r^2 values

rm(list=ls())

library(ggplot2)
library(plyr)
library(car)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric.1 <- as.character(args[4])
metric.2 <- as.character(args[5])
metric.3 <- as.character(args[6])

# create output directory
dir = paste0("../../output/metrics_colinearity/",dname,"/",metric.1,"/")
ifelse(!dir.exists(dir), dir.create(dir,recursive=TRUE), FALSE)

# load climate metrics
source('../../data/objects/objects.R')

# load metric of interest
dat.1 <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric.1,'_',dname,'/state_weighted_summary_',metric.1,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat.2 <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric.2,'_',dname,'/state_weighted_summary_',metric.2,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat.3 <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric.3,'_',dname,'/state_weighted_summary_',metric.3,'_',dname,'_',year.start,'_',year.end,'.rds'))

# subset for only one age-sex group
dat.1 <- subset(dat.1,age==85&sex==2)
dat.2 <- subset(dat.2,age==85&sex==2)
dat.3 <- subset(dat.3,age==85&sex==2)

# establish linear fit for R-squared value
dat.plot = data.frame(month=dat.1$month,state.fips=dat.1$state.fips,metric.1=dat.1[,ncol(dat.1)],metric.2=dat.2[,ncol(dat.2)],metric.3=dat.3[,ncol(dat.3)])
dat.plot$dummy <- seq(1:nrow(dat.plot))
lm = lm(dummy~metric.1+metric.2+metric.3,data=dat.plot)

# establish co-linearity of variables
vif = vif(lm)
