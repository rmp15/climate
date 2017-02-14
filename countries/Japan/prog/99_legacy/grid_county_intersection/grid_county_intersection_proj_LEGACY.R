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

# create directory to place output files into
ifelse(!dir.exists("../../output/grid_county_intersection"), dir.create("../../output/grid_county_intersection"), FALSE)

# load shapefile
us.national <- readOGR(dsn="../../data/shapefiles/cb_2015_us_county_500k",layer="cb_2015_us_county_500k")

original.proj <- proj4string(us.national)

# convert shapefile to Albers equal area
us.national <- spTransform(us.national, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

# remove non-mainland territories
us.main <- us.national[!us.national$STATEFP %in% c("02","15","60","66","69","71","72","78"),]

state.fips  <- "06"
county      <- "071"
county.fips <- paste0(state.fips,county)

# isolate states for test
us.state <- us.main[us.main$STATEFP %in% c("06"),]

# isolate county to highlight
us.county <- us.state[us.state$GEOID %in% c("06071"),]

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
# adjust grid lon and lat values to be in centre of grid square (not sure)
grid$lon <- grid$lon - 0.75/2
grid$lat <- grid$lat - 0.75/2
points <- SpatialPoints(grid)
proj4string(points) <- original.proj
points.proj <- spTransform(points, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

# create a graticule from grid points
grat <- graticule(lons=lon,lats=lat,xlim=lon.lim,ylim=lat.lim,proj=projection(us.state))

# create a graticule as polygons
grat.poly <- graticule(lons=lon,lats=lat,proj=projection(us.state),tiles=TRUE)

# perform intersection test to establish which grid point intersect with county
county.polys <- which(gIntersects(us.county,grat.poly,byid=TRUE)==TRUE,arr.ind=FALSE)

# create weighted mean for how much of each grid crosses the county of interest
weighted.area <- data.frame(id=numeric(0),weighted.area=numeric(0))
for(i in seq(1:length(county.polys))) {
id <- county.polys[i]
w.a <- gArea(gIntersection(us.county,grat.poly[grat.poly@data$layer==county.polys[i],]))/gArea(us.county)
weighted.area <- rbind(weighted.area,c(id,w.a)) 
}
names(weighted.area) <- c('id','weighted.area')
#if(sum(weighted.area$weighted.area)==1){
write.csv(weighted.area,'../../output/grid_county_intersection/weighted_area.csv')
#}

# plot state with graticule overlaid
#pdf('../../output/grid_county_intersection/state_graticule.pdf')
#plot(us.state);plot(grat,add=1)
#dev.off()

# plot state with highlighted county and grids that overlap
pdf('../../output/grid_county_intersection/county_graticule_highlighted.pdf')
plot(us.state);plot(grat,add=1);plot(us.county,col='green',add=1)
for(i in county.polys) {plot(grat.poly[grat.poly@data$layer==i,],border='blue',lwd=2,add=TRUE)}
dev.off()

# plot national with graticule overlaid
#pdf('../../output/grid_county_intersection/map_graticule.pdf')
#plot(us.main);plot(grat,add=1)
#dev.off()
