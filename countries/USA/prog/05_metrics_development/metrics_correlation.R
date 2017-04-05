# this script
# loads a particular metric
# plots against all the others
# establishes r^2 values

rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
dname <- as.character(args[1])
metric <- as.character(args[2])
year.start <- as.numeric(args[3])
year.end <- as.numeric(args[4])

# create output directory
ifelse(!dir.exists(paste0("../../output/metrics_correlation/",dname)), dir.create(paste0("../../output/metrics_correlation/",dname),recursive=TRUE), FALSE)

# load climate metrics
source('../../data/objects/objects.R')

# load metric of interest
dat <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))

# load all other climate variables
dat.metrics <- dat[,c(1:5)]
for(i in metrics) {
    dat.temp <- readRDS(paste0('../../output/metrics_development/',dname,'/',i,'_',dname,'/state_weighted_summary_',i,'_',dname,'_',year.start,'_',year.end,'.rds'))
    dat.metrics <- merge(dat.metrics,dat.temp)
}

# remove first 5 columns
dat.metrics[,c(1:6)] <- NULL
