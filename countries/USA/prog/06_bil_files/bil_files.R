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

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
month <- as.character(args[1])
if(nchar(month)==1){month <- as.character(paste0('0',month))}
year <- as.character(args[2])

# create output directory
ifelse(!dir.exists("../../output/bil_files"), dir.create("../../output/bil_files"), FALSE)

# EPSG strings
#latlong = "+init=epsg:4326"

# load PRISM bil dataset
dat.prism <- raster(paste0('~/data/climate/prism/bil/PRISM_tmean_stable_4kmM2_1982_all_bil/PRISM_tmean_stable_4kmM2_198207_bil.bil'))
# transform the projection of raster
#dat.prism <- projectRaster(dat.prism, crs = latlong)

# load csv with ERA-Interim grid values
grid <- read.csv('../../data/lon_lat/global_lon_lat.csv')
grid <- grid[,c(2:3)]

# adjust grid lon values in table to go from -180 to 180
grid <- transform(grid,lon=ifelse(lon>180,lon-360,lon))

# limit grid to extent of USA
grid <- subset(grid, lon>-179 & lon< -50 & lat>15 & lat<85)

# objects with lon and lat values
lon <- sort(unique(grid$lon))
lat <- sort(unique(grid$lat))

# create a graticule as polygons
grat.poly <- graticule(lons=lon,lats=lat,proj=projection(dat.prism),tiles=TRUE)

dat.output <- data.frame()

for(i in unique(grat.poly@data$layer)){

# isolate grid value
poly <- grat.poly[grat.poly@data$layer==i,]

# perform intersection test to establish which grid points intersect with county
cropped <-  try(crop(dat.prism,extent(poly),snap='out'),silent=1)
value <- try(mean(na.omit(cropped@data@values)),silent=1)
#print(c(i,value))
dummy <- try(data.frame(month,year,i,value),silent=1)
if(substr(dummy,1,5)!='Error'){
    dat.output <- try(rbind(dat.output,dummy),silent=1)}
}

names(dat.output) <- c('month','year','poly.id','temperature')
dat.output <- na.omit(dat.output)
dat.output <- dat.output[complete.cases(dat.output),]

# load ERA-Interim values
dname <- 't2m'
freq <- 'daily'
num <- 'twice'
file.name <- paste0('~/data/climate/net_cdf/',dname,'/processed/','worldwide_',dname,'_',freq,'_',num,'_',year,'.rds')
dat.era <- readRDS(file.name)

# adjust names of ERA-Interim values to be for month to prepare for summary
names.era <- names(dat.era)
names(dat.era) <- substr(names.era,6,7)
names(dat.era)[c(ncol(dat.era)-1,ncol(dat.era))] <- c('lat','lon')

# find average of temperatures which occur in same month
dat.average <- as.data.frame(lapply(split(as.list(dat.era),f = colnames(dat.era)),function(x) Reduce(`+`,x) / length(x)))
names(dat.average) <- c(1:12,'lat','lon')

# load lookup tables for polygons grid points and lon lat
poly.lookup <- readRDS('../../output/grid_county_intersection/point_poly_lookup.rds')





# overlay grid on raster map if required
#plot(grat.poly);plot(dat.prism,add=1)
# and test to add highlight grid square in blue
#plot(grat.poly[grat.poly@data$layer==7938,],border='blue',lwd=2,add=TRUE)