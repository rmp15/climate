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

cat('processing for years',year.start,year.end)

ifelse(!dir.exists(paste0("../../output/multiyear_normals_plots/",dname,"/",metric)), dir.create(paste0("../../output/multiyear_normals_plots/",dname,"/",metric), recursive=TRUE), FALSE)

# replace name of var
var <- paste0('mean_',dname)

# load data
dat = readRDS(paste0("../../output/multiyear_normals/",dname,"/",metric,'/state_longterm_95_nonnormals_',var,'_',year.start,'_',year.end,'.rds'))

# isolate one age-sex group and remove alaska and hawaii
dat = subset(dat,age==65&sex==1)
dat = subset(dat, !(state.fips%in%c('02','15')))

library(ggplot2)

# plot by average values by month
