# Extracting NetCDF files

# From:
# https://github.com/rmp15/climate/blob/master/extract_netcdf_data.R

rm(list=ls())
# http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm

library(RColorBrewer)
library(lattice)
library(ncdf4)

# year of interest
year <- 1982

# names for files
# monthly data
#ncname <- 'worldwide_monthly.nc'
# daily data
dname <- 't2m'
ncname <- paste0('worldwide_',dname,'_',year,'.nc')

year <- unlist(strsplit(ncname, "_"))
year <- year[3]
year <- unlist(strsplit(year, ".nc"))

# open NetCDF file
ncin <- nc_open(paste0('../../data/netcdf/',dname,'/',ncname))

# get long and lat data
lon <- ncvar_get(ncin, 'longitude')
nlon <- dim(lon)

lat <- ncvar_get(ncin, "latitude", verbose = F)
nlat <- dim(lat)

# get time variable and convert to days
t <- ncvar_get(ncin, "time")
t.hours <- t / 24
t.sec <- t * 3600
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)

# extract 2-metre temperature
tmp.array <- ncvar_get(ncin, dname)
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")

# global attributes
title <- ncatt_get(ncin, 0, "title")
institution <- ncatt_get(ncin, 0, "institution")
datasource <- ncatt_get(ncin, 0, "source")
references <- ncatt_get(ncin, 0, "references")
history <- ncatt_get(ncin, 0, "history")
Conventions <- ncatt_get(ncin, 0, "Conventions")

# close NetCDF file
nc_close(ncin)

# split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
t.names <- as.POSIXct(t.sec, origin = "1900-01-01")

# stamp as character names
timeStamp <-  strptime(t.names,"%Y-%m-%d %H:%M:%S")

# round to days
t.names <- as.character(round(timeStamp, 'days'))

# convert tmp.array into long file
tmp.vec.long <- as.vector(tmp.array)
length(tmp.vec.long)

# reshape file into matrix
tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
dim(tmp.mat)

# create second data frame
lonlat <- expand.grid(lon, lat)
tmp.df02 <- data.frame(cbind(lonlat, tmp.mat)) ## important line
names(tmp.df02) <- c('long','lat',t.names)
head(colnames(tmp.df02))

# Fixing longitude values so range is -180 to 180, not 0 to 360
global_lon_lat.df = read.csv("../../data/lon_lat/global_lon_lat.csv", header=TRUE)
tmp.df02$long <- global_lon_lat.df$long
tmp.df02$lat <- global_lon_lat.df$lat

# Clipping off last row because it starts the next year
end <- ncol(tmp.df02)
tmp.df02[end] <- NULL

# write to csv file with naming according to year
csv.name <- paste0('../../data/csv/',"worldwide_t2m_", year, ".csv")
write.csv(tmp.df02, file=csv.name)

rm(tmp.array)
rm(tmp.df02)
rm(tmp.mat)
rm(tmp.vec.long)
rm(lat)
rm(lon)

#########

# Col names after reading this back in were like "X1982.01.02" "X1982.01.02.1"
year.df <- read.csv(csv.name, nrows=5) # Use for loop to cycle through names?
names(year.df) <- c('long','lat',t.names[1:3])

# extract csv file of latitude longitude for USA
#USA.lonlat <- lonlat
#names(USA.lonlat) <- c('lon','lat')
#USA.lonlat$lon <- as.numeric(USA.lonlat$lon)
#USA.lonlat$lat <- as.numeric(USA.lonlat$lat)
##USA.lonlat <- subset(lonlat, lon>231)# & lon<296 & lat>23 & lat<50)
##USA.lonlat <- subset(lonlat, lon<296)
##USA.lonlat <- subset(lonlat, lat>23)
##USA.lonlat <- subset(lonlat, lat<50)
#write.csv(USA.lonlat,'global_lon_lat.csv')
