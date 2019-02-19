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
dat.mean = readRDS(dat,paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'_',type,'.rds')
dat.min = readRDS(dat,paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'_min.rds')
dat.max = readRDS(dat,paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'_max.rds')

# merge datasets
dat.merged = merge(dat.mean,dat.min, by=c()))
dat.merged = merge(dat.merged,dat.max, by=c()))