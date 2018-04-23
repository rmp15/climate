rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
dname <- as.character(args[1])
year.start <- as.numeric(args[2])
year.end <- as.numeric(args[3])
metric.1 <- as.character(args[4])
metric.2 <- as.character(args[4])

# create directory
dir = paste0("../../output/metrics_compare_county_state/",dname,'/',metric.1,'/',metric.2)
ifelse(!dir.exists(dir), dir.create(dir, recursive=TRUE), FALSE)

# load file with climate data
dat.1 <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric.1,'_',dname,'/state_weighted_summary_',metric.1,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat.1$state.fips <- as.numeric(as.character(dat.1$state.fips))
dat.2 <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric.2,'_',dname,'/state_weighted_summary_',metric.2,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat.2$state.fips <- as.numeric(as.character(dat.2$state.fips))

dat.both = merge(dat.1,dat.2)
dat.both$diff = with(dat.both,t2m.meanc3-t2m.meanc4)
dat.both = subset(dat.both,!(state.fips%in%c(2,15)))

pdf(paste0(dir,'/comparison_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')

ggplot(data=subset(dat.both,!(state.fips%in%c(2,15))),aes(x=t2m.meanc3,y=t2m.meanc4)) + facet_wrap(~month) + geom_point() + geom_abline(slope=1)

ggplot(data=subset(dat.both,!(state.fips%in%c(2,15,32))),aes(x=t2m.meanc3,y=t2m.meanc4)) + facet_wrap(~month) + geom_point() + geom_abline(slope=1)
dev.off()
