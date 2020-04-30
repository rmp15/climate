# Extracting daily NetCDF files for ERA-5

rm(list=ls())

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(sp)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# names for files
year_start <- as.character(args[1])
year_end <- as.character(args[2])
dname <- as.character(args[3])
freq <- as.character(args[4])
num <- as.character(args[5])
space.res <- as.character(args[6])

# for code testing
# dname <- 't2m' ; freq <- 'daily' ; num <- 'four' ; year_start <- '2010' ; year_start <- '2019' ; space.res='lad'

# create directory to place output files into
dir.input = "../../output/grid_county_intersection_raster/"

# load files for all years of long-term average calculation
dat.lads = data.frame()
for(year in years){
    weighted.area.national.total = read.csv(paste0(dir.input,'weighted_area_raster_lads_',dname,'_',freq,'_',as.character(year),'.csv'))
    dat.lads = rbind(dat.lads, weighted.area.national.total)
}

# do the scaling thing without FALSE as the second option
# 1. by week
# 2. by month