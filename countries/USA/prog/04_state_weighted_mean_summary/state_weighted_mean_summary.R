rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)

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

# create dummy temperature data for testing the weighted mean for a month
grid.temp <- grid
set.seed(12232)
days.in.month <- 30
for (i in c(1:days.in.month)) {
    dummy <- rnorm(dim(grid.temp)[1],300,1)
    grid.temp <- cbind(grid.temp,dummy)
    names(grid.temp)[i+2] <- paste0('day',i)
}

# load lookup tables for polygons grid points and lon lat
poly.lookup <- readRDS('../../output/grid_county_intersection/point_poly_lookup.rds')

# merge dummy temperature data with polygon ids
grid.temp <- merge(grid.temp,poly.lookup,by=c('lon','lat'),all.x=1)
grid.temp <- na.omit(grid.temp)

# load weighted mean lookup table
wm.lookup <- readRDS('../../output/grid_county_intersection/weighted_area_unproj_national.rds')

# for each county, summarise by day and then by month (for single month)
# fix this
dat.wm <- data.frame()
for(i in unique(wm.lookup$state.county.fips)){
    fips.match <- subset(wm.lookup,state.county.fips==i)
    fips.match <- merge(fips.match,grid.temp,by=c('point.id','poly.id'),all.x=1)
    dat.days <- fips.match[,grepl('day',names(fips.match))]
    running.total <- 0
    for(j in c(1:days.in.month)){
        weighted.mean <- sum(fips.match$weighted.area*dat.days[,j])
        running.total <- running.total + weighted.mean
    }
    month.average <- running.total / days.in.month
    dat.wm <- rbind(dat.wm,data.frame(substr(i,1,2),substr(i,3,5),as.character(i),month.average))
}
names(dat.wm) <- c('state.fips','county.fips','state.county.fips','temp.wm')

# load weightings by county for state summary based on population
state.weighting <- readRDS('../../output/population_weighted_mean/state_population_weightings.rds')

# filter for a few years and month if desired
# change to make more general
years.selected <- c(2005)
state.weighting.filter <- subset(state.weighting,year %in% years.selected)
month.selected <- 1
state.weighting.filter <- subset(state.weighting.filter,month %in% month.selected)

# filter age and sex
# change to make more general
#age.selected <- 75
#sex.selected <- 1
#state.weighting.filter <- subset(state.weighting.filter,age %in% age.selected)
#state.weighting.filter <- subset(state.weighting.filter,sex %in% sex.selected)

dat.temp <-merge(dat.wm,state.weighting.filter,by=c('state.fips','county.fips'))
temp.state <- ddply(dat.temp,.(state.fips,sex,age),summarize,temp.adj=sum(pop.weighted*temp.wm))



















# merge dummy temperature data with weighted area to calculate values
#weighted.area.temp <- merge(weighted.area,grid.temp,by=c('point.id','poly.id'),all.x=TRUE)

# create weighted mean for each day
#weighted.mean.total <- data.frame(day=numeric(0),weighted.temp=numeric(0))
#for(i in c(1:days.in.month)){
#    dummy <- sum(weighted.area.temp$weighted.area*weighted.area.temp[,i+5])
#    weighted.mean.total <- rbind(weighted.mean.total,c(i,dummy))
#}
#names(weighted.mean.total) <- c('day','weighted.temp')
#write.csv(weighted.mean.total,'../../output/grid_county_intersection/weighted_mean_total_unproj.csv',row.names=FALSE)

# load weighted grid to county lookup to created weighted mean of climate variables
# from gridded temperature data
