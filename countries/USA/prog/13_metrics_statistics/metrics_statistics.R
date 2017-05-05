rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric <- as.character(args[4])

# create directory
ifelse(!dir.exists(paste0("../../output/metrics_statistics/",dname,"/",metric)), dir.create(paste0("../../output/metrics_statistics/",dname,"/",metric), recursive=TRUE), FALSE)

# load file with climate data
dat <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat$state.fips <- as.numeric(as.character(dat$state.fips))

# load state names
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')

# merge state names
dat <- merge(dat,state.lookup,by.x='state.fips',by.y='fips')

names(dat)[grep(dname,names(dat))] <- 'variable'

dat$variable.rounded <- round(dat$variable)

# summarise how many
dat.summary <- count(subset(dat,age==85 & sex==2),'variable.rounded')

dat.summary.state <- count(subset(dat,age==85 & sex==2),c('full_name','variable.rounded'))

# export data

saveRDS(dat.summary,paste0("../../output/metrics_statistics/",dname,"/",metric,"_",dname,"/national_summary_",dname,"_",metric,"_",year.start,"_",year.end))

saveRDS(dat.summary.state,paste0("../../output/metrics_statistics/",dname,"/",metric,"_",dname,"/subnational_summary_",dname,"_",metric,"_",year.start,"_",year.end))
