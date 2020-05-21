# Finding weekly long-term averages
# and anomalies based on long-term averages

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
country.id <- as.character(args[7])
year.pop = as.character(args[8])

# for code testing
# dname = 't2m' ; freq = 'daily' ; num = 'four' ; year_start = '2011' ; year_end = '2020' ; space.res='1' ; country.id = 'BEL' ; year.pop = '2010'

years = c(year_start:year_end)

# load processed daily anomalies
dir.input = paste0("../../output/grid_county_intersection_raster/",country.id,'/adm',space.res,'/')
dat.adm.anomaly = read.csv(paste0(dir.input,'full_weekly_anomalies_weighted_area_raster_adm_',dname,'_',freq,'_',as.character(year_start),'_',as.character(year_end),'.csv'))

# load populalation weightings
dir.output = paste0("../../output/population_grid_summary/",country.id,'/adm',space.res,'/')
weighted.area.national = read.csv(paste0(dir.output,'population_',country.id,'_',space.res,'_',as.character(year.pop),'.csv'))
names(weighted.area.national)[3] = 'population'

# merge population values with temperature records
dat.merged = merge(dat.adm.anomaly, weighted.area.national, by='ID_1',all.x=TRUE)

# create national means of absolute values as well as anomaly values, weighted by population of subnational regions
dat.weighted.mean = ddply(dat.merged, .(date), summarize, t2m.weighted=weighted.mean(t2m,population))
