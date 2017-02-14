rm(list=ls())
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(plyr)

# create directory to place output files into
ifelse(!dir.exists("../../output/grid_county_intersection"), dir.create("../../output/grid_county_intersection"), FALSE)

# MAP

# for theme_map
devtools::source_gist("33baa3a79c5cfef0f6df")

# load shapefile
us_county <- readOGR(dsn="../../data/shapefiles/cb_2015_us_county_500k",layer="cb_2015_us_county_500k")

# remove non-mainland territories
us_main <- us_county[!us_county$STATEFP %in% c("02","15","60","66","69","71","72","78"),]

# fortify to prepare for ggplot
map_usa_main <- fortify(us_main)

# add graticule and bounding box (longlat)
grat <- readOGR("../../data/shapefiles/ne_110m_graticules_1", layer="ne_110m_graticules_1")
grat_df <- fortify(grat)

# limit the graticule to cover the extent of mainland USA
grat_df <- subset(grat_df, long> -125 & long< -66 & lat>23 & lat<50)

bbox <- readOGR("../../data/shapefiles/ne_110m_wgs84_bounding_box", layer="ne_110m_wgs84_bounding_box")
bbox_df<- fortify(bbox)

# map of mainland USA by county in case needed
pdf('../../output/grid_county_intersection/usa_map_counties_flat.pdf',height=0,width=0,paper='a4r')
ggplot(data=map_usa_main,aes(x=long,y=lat,group=group)) +
geom_polygon(aes(),color='black',size=0.1) +
geom_path(data=grat_df, aes(long, lat, group=group, fill=NULL), color="grey50") +
#scale_fill_manual(values=map.region.colour,guide = guide_legend(title = '')) +
theme_map() +
theme(text = element_text(size = 15),legend.justification=c(1,0), legend.position='bottom')
dev.off()

# isolate states for test
us_kansas <- us_main[us_main$STATEFP %in% c("20"),]

# limit the graticule to cover the extent of Kansas
grat_kansas_df <- subset(grat_df, long> -103 & long< -93 & lat> 36 & lat< 40)

# fortify to prepare for ggplot
map_kansas <- fortify(us_kansas)

# map of Kansas by county in case needed
pdf('../../output/grid_county_intersection/usa_map_kansas_flat.pdf',height=0,width=0,paper='a4r')
ggplot(data=map_kansas,aes(x=long,y=lat,group=group)) +
geom_polygon(aes(),color='black',size=0.1) +
geom_path(data=grat_kansas_df, aes(long, lat, group=group, fill=NULL), color="grey50") +
#scale_fill_manual(values=map.region.colour,guide = guide_legend(title = '')) +
theme_map() +
theme(text = element_text(size = 15),legend.justification=c(1,0), legend.position='bottom')
dev.off()

# isolate a single county
us_county@data[us_county@data$GEOID==20015,]
us_county <- us_county[us_county$GEOID %in% c("36001"),]

# fortify to prepare to ggplot
map_county <- fortify(us_county)

# map of single county in case needed
pdf('../../output/grid_county_intersection/usa_map_county_flat.pdf',height=0,width=0,paper='a4r')
ggplot(data=map_county,aes(x=long,y=lat,group=group)) +
geom_polygon(aes(),color='black',size=0.1) +
geom_path(data=grat_kansas_df, aes(long, lat, group=group, fill=NULL), color="grey50") +
#scale_fill_manual(values=map.region.colour,guide = guide_legend(title = '')) +
theme_map() +
theme(text = element_text(size = 15),legend.justification=c(1,0), legend.position='bottom')
dev.off()

# create a polygon which will overlap county county
p1 <- data.frame(PID=rep(1, 4), POS=1:4, X=c(max(map_county$long)+0.25,max(map_county$long)+0.25,min(map_county$long)-0.25,min(map_county$long)-0.25), Y=c(max(map_county$lat)+0.25,min(map_county$lat)-0.25,min(map_county$lat)-0.25,max(map_county$lat)+0.25))

# plot county county, grid point which overlaps, and highlighted overlapping point
plot(1,1,ylim=c(max(map_county$lat)+0.5,min(map_county$lat)-0.5),xlim=c(max(map_county$long)+0.5,min(map_county$long)-0.5), t="n", xlab="", ylab="")
polygon(map_county)
polygon(p1$X, p1$Y, border=2)

# copy example http://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r
us_county <- as(us_county, 'SpatialPolygons')

# figure out coordinates of grid
x1 <- min(map_county$long)-0.25
x2 <- (min(map_county$long)+max(map_county$long))/2
x3 <- max(map_county$long)+0.25
y1 <- min(map_county$lat)-0.25
y2 <- (min(map_county$lat) + max(map_county$lat))/2
y3 <- max(map_county$lat)+0.25

p2 <-   union(as(extent(x1,x2,y1,y2), 'SpatialPolygons'),
        as(extent(x2,x3,y1,y2), 'SpatialPolygons'))

#p3 <- union(as(extent(min(map_county$long)-0.25,max(map_county$long)+0.25, min(map_county$lat)-0.25,max(map_county$lat)+0.25), 'SpatialPolygons'))
field <- SpatialPolygonsDataFrame(p2, data.frame(field=c('x','y')), match.ID=F)
projection(field) <- projection(us_county)

# perform intersect to see what proportion covers which area
intrsct <- intersect(us_county,field)

# plot
plot(us_county, axes=T); plot(field, add=T); plot(intrsct, add=T, col='red')

# Extract areas from polygon objects then attach as attribute
areas <- data.frame(area=sapply(intrsct@polygons, FUN=function(x) {slot(x, 'area')}))
row.names(areas) <- sapply(intrsct@polygons, FUN=function(x) {slot(x, 'ID')})
# Combine attributes info and areas
attArea <- spCbind(intrsct, areas)

# For each field, get area per area
mean <- aggregate(area~field, data=attArea, FUN=sum)
mean$w.mean <- mean$area/sum(mean$area)

# perform weighted mean of population for temperature

# copy example http://gis.stackexchange.com/questions/40517/using-r-to-calculate-the-area-of-multiple-polygons-on-a-map-that-intersect-with





























