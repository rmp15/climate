rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.arg <- as.numeric(args[1])

# create output file locations
file.loc <- paste0("../../output/test_missing_counties/",year.arg,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load weighted mean lookup table (this comes from counties in the map)
wm.lookup <- readRDS('../../output/grid_county_intersection/weighted_area_unproj_national.rds')
wm.lookup$state.county.fips <- as.character(wm.lookup$state.county.fips)

# load county population file (this comes from CDC)
pop.county.wm.month <- readRDS('~/data/climate/population_weightings/state_population_weightings.rds')

# filter for selected year
pop.county.wm.month <- subset(pop.county.wm.month,year==year.arg)

# merge counties to see which ones are missing
counties.map <- data.frame(counties.map=(sort(unique(wm.lookup$state.county.fips))))
counties.pop <- data.frame(counties.pop=(sort(unique(pop.county.wm.month$state.county.fips))))
counties.merged <- merge(counties.map,counties.pop,by.x=c('counties.map'),by.y=c('counties.pop'))
library(dplyr)
counties.missing.map <- anti_join(counties.map,counties.pop,by=c('counties.map'='counties.pop'))
counties.missing.pop <- anti_join(counties.pop,counties.map,by=c('counties.pop'='counties.map'))

# save output
saveRDS(counties.missing.map,paste0(file.loc,'unmatched_counties_map_',year.arg,'.rds'))
write.csv(counties.missing.map,paste0(file.loc,'unmatched_counties_map_',year.arg,'.csv'),row.names=FALSE)
saveRDS(counties.missing.pop,paste0(file.loc,'unmatched_counties_pop_',year.arg,'.rds'))
write.csv(counties.missing.pop,paste0(file.loc,'unmatched_counties_pop_',year.arg,'.csv'),row.names=FALSE)
