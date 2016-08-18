rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

ifelse(!dir.exists("../../output/state_weighted_mean_summary"), dir.create("../../output/state_weighted_mean_summary"), FALSE)

# load csv with ERA-Interim grid values
grid <- read.csv('../../data/lon_lat/global_lon_lat.csv')
grid <- grid[,c(2:3)]

# adjust grid lon values in table to go from -180 to 180
grid$lon <- grid$lon - 180

# objects with lon and lat values
lon <- sort(unique(grid$lon))
lat <- sort(unique(grid$lat))

# limits of lat and lon values
lon.lim <- c(min(lon),max(lon))
lat.lim <- c(min(lat),max(lat))

# establish how many rows and columns there are in grid file
grid.rows <- length(unique(grid$lon))
grid.cols <- length(unique(grid$lat))

# FINISH REPLACING WITH REAL DATA INSTEAD OF MADE UP ONE BELOW
# MIGHT HAVE TO FIX ORIINAL LON LAT COORDS BECAUSE THEY ARE ARRAYS

# load real data
#year <- as.numeric(args[1])
#file.name <- paste0('worldwide_t2m_daily_twice_',year,'.rds')
#grid.temp <- readRDS(paste0('../../output/extracting_netcdf_files/',file.name))

# create dummy temperature data for testing the weighted mean for a year
grid.temp <- grid
set.seed(12232)
dates <- seq(as.Date(paste0(year,"/1/1")), as.Date(paste0(year,"/12/31")), "days")
dates <- as.character(dates)

for (i in dates) {
    dummy <- rnorm(dim(grid.temp)[1],300,10)
    grid.temp <- cbind(grid.temp,dummy)
    names(grid.temp)[match(i,dates)+2] <- i
}

# load lookup tables for polygons grid points and lon lat
poly.lookup <- readRDS('../../output/grid_county_intersection/point_poly_lookup.rds')

# merge dummy temperature data with polygon ids
grid.temp <- merge(grid.temp,poly.lookup,by=c('lon','lat'),all.x=1)
grid.temp <- na.omit(grid.temp)

# load weighted mean lookup table
wm.lookup <- readRDS('../../output/grid_county_intersection/weighted_area_unproj_national.rds')

# for each county, summarise by day and then by month (for single year)
dat.wm <- data.frame()
for(i in unique(wm.lookup$state.county.fips)){
    fips.match <- subset(wm.lookup,state.county.fips==i)
    fips.match <- merge(fips.match,grid.temp,by=c('point.id','poly.id'),all.x=1)
    weightings <- fips.match[ , grepl('weighted.area', names(fips.match) ) ]
    dat.days <- fips.match[ , grepl( year , names(fips.match) ) ]
    dat.days <- dat.days * weightings
    dat.days <- as.data.frame(colSums(dat.days))
    dat.days <- cbind(dat.days, as.data.frame(matrix(unlist(strsplit(rownames(dat.days),'-')), ncol=3, byrow=TRUE)))
    names(dat.days) <- c('temperature','year','month','day')
    summary <- ddply(dat.days,.(year,month),summarize,temp.weighted=mean(temperature))
    summary$state.county.fips <- i
    dat.wm <- rbind(dat.wm,summary)
}
dat.wm$year <- as.numeric(as.character(dat.wm$year))
dat.wm$month <- as.numeric(as.character(dat.wm$month))

# load weightings by county for state summary based on population
state.weighting <- readRDS('../../output/population_weighted_mean/state_population_weightings.rds')

# filter for a year of interest
year.selected <- year
state.weighting.filter <- subset(state.weighting,year %in% year.selected)

dat.temp <-merge(dat.wm,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,temp.adj=sum(pop.weighted*temp.weighted))

saveRDS(temp.state,paste0('../../output/state_weighted_mean_summary/state_weighted_summary_',year.selected,'.rds'))