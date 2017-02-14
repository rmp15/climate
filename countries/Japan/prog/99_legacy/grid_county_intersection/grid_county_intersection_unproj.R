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
library(graticule)
library(spatialEco)

# create directory to place output files into
ifelse(!dir.exists("../../output/grid_county_intersection"), dir.create("../../output/grid_county_intersection"), FALSE)

# load shapefile
us.national <- readOGR(dsn="../../data/shapefiles/cb_2015_us_county_500k",layer="cb_2015_us_county_500k")

original.proj <- proj4string(us.national)

# remove non-mainland territories
us.main <- us.national[!us.national$STATEFP %in% c("60","66","69","71","72","78"),]

state.fips  <- "32"
county      <- "023"
county.fips <- paste0(state.fips,county)

# isolate state for test
us.state <- us.main[us.main$STATEFP %in% state.fips,]

# isolate county to highlight
us.county <- us.state[us.state$GEOID %in% county.fips,]

# load csv with ERA-Interim grid values
grid <- read.csv('../../data/lon_lat/global_lon_lat.csv')
grid <- grid[,c(2:3)]

# adjust grid lon values in table to go from -180 to 180
grid$lon <- grid$lon - 180

# limit grid to extent of USA
grid <- subset(grid, lon>-179 & lon< -50 & lat>15 & lat<85)

# objects with lon and lat values
lon <- sort(unique(grid$lon))
lat <- sort(unique(grid$lat))

# limits of lat and lon values
lon.lim <- c(min(lon),max(lon))
lat.lim <- c(min(lat),max(lat))

# establish how many rows and columns there are in grid file
grid.rows <- length(unique(grid$lon))
grid.cols <- length(unique(grid$lat))

# prepare grid points to plot on map as points
# adjust grid lon and lat values to be in centre of grid square
# is this the correct box to move it into?
interval <- 0.75
grid$lon <- grid$lon - interval/2
grid$lat <- grid$lat - interval/2
#points.proj <- SpatialPoints(grid)
points.proj <- SpatialPointsDataFrame(coords=grid,data=grid)
proj4string(points.proj) <- original.proj
points.proj$id <- 1:nrow(points.proj)

# create a graticule from grid points
grat <- graticule(lons=lon,lats=lat,xlim=lon.lim,ylim=lat.lim,proj=projection(us.state))

# create a graticule as polygons
grat.poly <- graticule(lons=lon,lats=lat,proj=projection(us.state),tiles=TRUE)
grat.poly <- spTransform(grat.poly,proj4string(us.county))

# perform intersection test to establish which grid point intersect with county
county.polys <- which(gIntersects(us.county,grat.poly,byid=TRUE)==TRUE,arr.ind=FALSE)

# create lookup table between points and polygons
point.poly.lookup <- point.in.poly(points.proj,grat.poly)
names(point.poly.lookup) <- c('lon','lat','point.id','poly.id')
point.poly.lookup@data$lon <- point.poly.lookup@data$lon + interval/2
point.poly.lookup@data$lat <- point.poly.lookup@data$lat + interval/2
write.csv(point.poly.lookup@data,'../../output/grid_county_intersection/point_poly_lookup.csv')

# create weighted mean for how much of each grid crosses the county of interest
weighted.area <- data.frame(poly.id=numeric(0),weighted.area=numeric(0))
for(i in seq(1:length(county.polys))) {
poly.id <- county.polys[i]
point.id <- point.poly.lookup@data[which(point.poly.lookup@data$poly.id ==poly.id),3]
w.a <- gArea(gIntersection(us.county,grat.poly[grat.poly@data$layer==county.polys[i],]))/gArea(us.county)
weighted.area <- rbind(weighted.area,c(point.id,poly.id,w.a))
}
names(weighted.area) <- c('point.id','poly.id','weighted.area')
write.csv(weighted.area,'../../output/grid_county_intersection/weighted_area_unproj.csv')

# create dummy temperature data for testing the weighted mean for a few days
grid.temp <- grid
grid.temp$lon <- grid.temp$lon + interval/2
grid.temp$lat <- grid.temp$lat + interval/2
set.seed(12232)
days.in.month <- 30
for (i in c(1:days.in.month)) {
    dummy <- rnorm(dim(grid.temp)[1],300,1)
    grid.temp <- cbind(grid.temp,dummy)
    names(grid.temp)[i+2] <- paste0('day',i)
}
grid.temp <- merge(grid.temp,point.poly.lookup@data,by=c('lon','lat'))

# merge dummy temperature data with weighted area to calculate values
weighted.area.temp <- merge(weighted.area,grid.temp,by=c('point.id','poly.id'),all.x=TRUE)

# create weighted mean for each day
weighted.mean.total <- data.frame(day=numeric(0),weighted.temp=numeric(0))
for(i in c(1:days.in.month)){
    dummy <- sum(weighted.area.temp$weighted.area*weighted.area.temp[,i+5])
    weighted.mean.total <- rbind(weighted.mean.total,c(i,dummy))
}
names(weighted.mean.total) <- c('day','weighted.temp')
write.csv(weighted.mean.total,'../../output/grid_county_intersection/weighted_mean_total_unproj.csv')

# PLOTS

# plot state with graticule overlaid
#pdf('../../output/grid_county_intersection/state_graticule.pdf')
#plot(us.state);plot(grat,add=1)
#dev.off()

# plot state with highlighted county and grids that overlap
pdf('../../output/grid_county_intersection/county_graticule_highlighted_unproj.pdf')
plot(us.state);plot(grat,add=1);plot(us.county,col='green',add=1)
for(i in county.polys) {plot(grat.poly[grat.poly@data$layer==i,],border='blue',lwd=2,add=TRUE)}
for(i in weighted.area$point.id) {plot(points.proj[points.proj$id==i,],col='red',add=TRUE)}
dev.off()

# plot national with graticule overlaid
#pdf('../../output/grid_county_intersection/map_graticule.pdf')
#plot(us.main);plot(grat,add=1)
#dev.off()
