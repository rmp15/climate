rm(list=ls())

print('running grid_county_intersection_unproj_function.R')

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
grat <- graticule(lons=lon,lats=lat,xlim=lon.lim,ylim=lat.lim,proj=projection(us.main))

# create a graticule as polygons
grat.poly <- graticule(lons=lon,lats=lat,proj=projection(us.main),tiles=TRUE)
grat.poly <- spTransform(grat.poly,proj4string(us.main))

# create lookup table between points and polygons
point.poly.lookup <- point.in.poly(points.proj,grat.poly)
names(point.poly.lookup) <- c('lon','lat','point.id','poly.id')
point.poly.lookup@data$lon <- point.poly.lookup@data$lon + interval/2
point.poly.lookup@data$lat <- point.poly.lookup@data$lat + interval/2
#write.csv(point.poly.lookup@data,'../../output/grid_county_intersection/point_poly_lookup.csv',row.names=FALSE)
saveRDS(point.poly.lookup@data,'../../output/grid_county_intersection/point_poly_lookup.rds')


################################################################
# function to perform analysis for entire state
state.analysis <- function(state.arg='01',output=0) {
    
weighted.area <- data.frame()

# isolate state for test
state.fips  <- as.character(state.arg)
us.state <- us.main[us.main$STATEFP %in% state.fips,]

# plot state with highlighted county and grids that overlap
if(output==1){
    pdf(paste0('../../output/grid_county_intersection/county_graticule_highlighted_unproj_',state.fips,'.pdf'))}

counties <- unique(as.character(us.state$GEOID))
#counties <- c('011','013')
print(counties)

for(i in counties) {

county      <- as.character(i)
print(paste('current status:',county))
county.fips <- county
#county.fips <- paste0(state.fips,county)

# isolate county to highlight
us.county <- us.state[us.state$GEOID %in% county.fips,]

# perform intersection test to establish which grid point intersect with county
county.polys <- which(gIntersects(us.county,grat.poly,byid=TRUE)==TRUE,arr.ind=FALSE)

if(output==1){
plot(us.state);plot(grat,add=1);plot(us.county,col='green',add=1)
for(i in county.polys) {plot(grat.poly[grat.poly@data$layer==i,],border='blue',lwd=2,add=TRUE)}
for(i in weighted.area$point.id) {plot(points.proj[points.proj$id==i,],col='red',add=TRUE)}
}

# create weighted mean for how much of each grid crosses the county of interest
#weighted.area <- data.frame(point.id=numeric(0),poly.id=numeric(0),weighted.area=numeric(0))
weighted.area.temp <- data.frame()
for(i in seq(1:length(county.polys))) {
poly.id <- county.polys[i]
point.id <- point.poly.lookup@data[which(point.poly.lookup@data$poly.id ==poly.id),3]
w.a <- gArea(gIntersection(us.county,grat.poly[grat.poly@data$layer==county.polys[i],]))/gArea(us.county)
#weighted.area.temp <- rbind(weighted.area.temp,c(as.numeric(state.fips),as.numeric(county.fips),point.id,poly.id,w.a))
weighted.area.temp <- rbind(weighted.area.temp,data.frame(state.fips,county.fips,point.id,poly.id,w.a))
#print(weighted.area.temp)
}

names(weighted.area.temp) <- c('','','','','')
weighted.area <- rbind(weighted.area,weighted.area.temp)

}

names(weighted.area) <- c('state.fips','county.fips','point.id','poly.id','weighted.area')

if(output==1){
    write.csv(weighted.area,paste0('../../output/grid_county_intersection/weighted_area_unproj_',state.fips,'.csv'),row.names=FALSE)
    dev.off()}

return(weighted.area)

}

################################################################

# perform for every state in the USA
#states <- unique(as.character(us.main$STATEFP))
# perform for just a few to test
states <- c('10')
weighted.area.national <- data.frame()
for(i in states){
analysis.dummy <- state.analysis(i)
weighted.area.national <- rbind(weighted.area.national,analysis.dummy)
}

names(weighted.area.national)[2] <- 'state.county.fips'
weighted.area.national$county.fips <- substr(weighted.area.national$state.county.fips,3,5)
weighted.area.national <- weighted.area.national[,c(1,6,2:5)]
saveRDS(weighted.area.national,paste0('../../output/grid_county_intersection/weighted_area_unproj_national.rds'))

# PLOTS

# plot state with graticule overlaid
#pdf('../../output/grid_county_intersection/state_graticule.pdf')
#plot(us.state);plot(grat,add=1)
#dev.off()

# plot national with graticule overlaid
#pdf('../../output/grid_county_intersection/map_graticule.pdf')
#plot(us.main);plot(grat,add=1)
#dev.off()




