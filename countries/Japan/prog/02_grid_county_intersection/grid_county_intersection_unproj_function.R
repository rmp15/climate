rm(list=ls())

print('running_grid_county_intersection_unproj_function.R')

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
jp_national <- readOGR(dsn="../../data/shapefiles/",layer="JPN_adm1")

original.proj <- proj4string(jp_national)

# load csv with ERA-Interim grid values
grid <- read.csv('../../data/lon_lat/global_lon_lat.csv')
grid <- grid[,c(2:3)]

# adjust grid lon values in table to go from -180 to 180
grid <- transform(grid,lon=ifelse(lon>180,lon-360,lon))

# limit grid to extent of Japan
grid <- subset(grid, lon>120 & lon< 160 & lat>20 & lat<50)

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
points.proj <- SpatialPointsDataFrame(coords=grid,data=grid)
proj4string(points.proj) <- original.proj
points.proj$id <- 1:nrow(points.proj)

# create a graticule from grid points
grat <- graticule(lons=lon,lats=lat,xlim=lon.lim,ylim=lat.lim,proj=projection(jp_national))

# create a graticule as polygons
grat.poly <- graticule(lons=lon,lats=lat,proj=projection(jp_national),tiles=TRUE)
grat.poly <- spTransform(grat.poly,proj4string(jp_national))

# create lookup table between points and polygons
point.poly.lookup <- point.in.poly(points.proj,grat.poly)
names(point.poly.lookup) <- c('lon','lat','point.id','poly.id')
point.poly.lookup@data$lon <- point.poly.lookup@data$lon + interval/2
point.poly.lookup@data$lat <- point.poly.lookup@data$lat + interval/2
saveRDS(point.poly.lookup@data,'../../output/grid_county_intersection/point_poly_lookup.rds')

################################################################
# function to perform analysis for entire state
state.analysis <- function(state.arg=1457,output=0) {
    
weighted.area <- data.frame()

# isolate state
state.fips  <- state.arg
jp_state <- jp_national[jp_national$ID_1 %in% state.fips,]

# plot state with highlighted county and grids that overlap
if(output==1){
    pdf(paste0('../../output/grid_county_intersection/county_graticule_highlighted_unproj_',state.fips,'.pdf'))}

# perform intersection test to establish which grid points intersect with state
state.polys <- which(gIntersects(jp_state,grat.poly,byid=TRUE)==TRUE,arr.ind=FALSE)

if(output==1){
plot(jp_state);plot(grat,add=1)
for(i in state.polys) {plot(grat.poly[grat.poly@data$layer==i,],border='blue',lwd=2,add=TRUE)}
for(i in weighted.area$point.id) {plot(points.proj[points.proj$id==i,],col='red',add=TRUE)}
}

# create weighted mean for how much of each grid crosses the state of interest
weighted.area.temp <- data.frame()
for(i in seq(1:length(state.polys))) {
poly.id <- state.polys[i]
point.id <- point.poly.lookup@data[which(point.poly.lookup@data$poly.id ==poly.id),3]
w.a <- gArea(gIntersection(jp_state,grat.poly[grat.poly@data$layer==state.polys[i],]))/gArea(jp_state)
weighted.area.temp <- rbind(weighted.area.temp,data.frame(state.fips,point.id,poly.id,w.a))
}

names(weighted.area.temp) <- c('','','','')
weighted.area <- rbind(weighted.area,weighted.area.temp)

names(weighted.area) <- c('state_id','point_id','poly_id','weighted_area')

if(output==1){
    write.csv(weighted.area,paste0('../../output/grid_county_intersection/weighted_area_unproj_',state.fips,'.csv'),row.names=FALSE)
    dev.off()}

return(weighted.area)

}

################################################################

# perform for every state in the Japan
states <- unique(as.character(jp_national$ID_1))
# OR perform for just a few to test
weighted.area.national <- data.frame()
for(i in states){
analysis.dummy <- state.analysis(i)
weighted.area.national <- rbind(weighted.area.national,analysis.dummy)
}

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




