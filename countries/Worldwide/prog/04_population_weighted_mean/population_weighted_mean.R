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

# for code testing
# dname = 't2m' ; freq = 'daily' ; num = 'four' ; year_start = '2010' ; year_end = '2020' ; space.res='0' ; country.id = 'BEL'

years = c(year_start:year_end)

# attach