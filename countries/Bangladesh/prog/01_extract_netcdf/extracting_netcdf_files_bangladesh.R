# Extracting NetCDF files for ERA-5

# From:
# https://github.com/rmp15/climate/blob/master/extract_netcdf_data.R
# http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm

rm(list=ls())

library(RColorBrewer)
library(lattice)
library(ncdf4)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# year of interest
year <- as.numeric(args[1])

print(paste0('running extracting_netcdf_files_bangladesh.R for ',year))

# names for files
#dname <- 'tmax' ; freq <- 'daily'
dname <- as.character(args[2])
freq <- as.character(args[3])

# generate name of file
ncname <- paste0(dname,'_mrg_',year,'0101_ALL.nc')

# open NetCDF file
ncin <- nc_open(paste0('~/data/climate/net_cdf/',dname,'/MRG_',toupper(dname),'_',freq,'/',ncname))

# get long and lat data
lon <- ncvar_get(ncin, 'Lon')
nlon <- dim(lon)

lat <- ncvar_get(ncin, "Lat", verbose = F)
nlat <- dim(lat)

# variables may be called something else in file itself, so dictionary to get that
if(dname%in%c('tmax')){dname.2='temp'}

# extract climate variable
tmp.array <- ncvar_get(ncin, dname.2)
# dlname <- ncatt_get(ncin, dname.2, "long_name")
# dunits <- ncatt_get(ncin, dname.2, "units")
# fillvalue <- ncatt_get(ncin, dname.2, "_FillValue")

# global attributes

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
t.names <- as.character(floor_date(timeStamp, 'day'))

# convert tmp.array into long file
tmp.vec.long <- as.vector(tmp.array)

# get number of days in year
is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}
nt = ifelse(is.leapyear(year)==TRUE,366,365)

# reshape file into matrix
tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)

# FROM HERE ESTABLISH WHICH WAY ROUND LON AND LAT ARE COUNTED

# create second data frame
lonlat <- expand.grid(lon, lat)
tmp.df02 <- data.frame(cbind(lonlat, tmp.mat))
names(tmp.df02) <- c('lon','lat',t.names)
head(colnames(tmp.df02))

# fixing longitude values so range is -180 to 180, not 0 to 360
# transform makes something weird happen with names, so fix
dat <- as.data.frame(tmp.df02)
dat.names <- names(dat)
dat <- transform(dat,lon=ifelse(lon>180,lon-360,lon))
names(dat) <- dat.names

# find average of temperatures which occur on the same days
dat.average <- as.data.frame(lapply(split(as.list(dat),f = colnames(dat)),function(x) Reduce(`+`,x) / length(x)))
names(dat.average) <- c(unique(t.names),'lat','lon')

# fix this
dat.min <- as.data.frame(lapply(split(as.list(dat),f = colnames(dat)),function(x) Reduce(`pmin`,x)))
names(dat.min) <- c(unique(t.names),'lat','lon')

# fix this
dat.max <- as.data.frame(lapply(split(as.list(dat),f = colnames(dat)),function(x) Reduce(`pmax`,x)))
names(dat.max) <- c(unique(t.names),'lat','lon')

# write to rds file with naming according to year
ifelse(!dir.exists("../../output/extracting_netcdf_files"), dir.create("../../output/extracting_netcdf_files"), FALSE)
ifelse(!dir.exists(paste0('~/data/climate/net_cdf/',dname,'/processed_era5/')), dir.create(paste0('~/data/climate/net_cdf/',dname,'/processed_era5/')), FALSE)

file.name <- paste0('~/data/climate/net_cdf/',dname,'/processed_era5/','worldwide_',dname,'_',freq,'_',num,'_',year,'.rds')
saveRDS(dat.average, file.name)

# file.name.min <- paste0('~/data/climate/net_cdf/',dname,'/processed_era5/','worldwide_',dname,'_',freq,'_',num,'_',year,'_min.rds')
# saveRDS(dat.min, file.name.min)
#
# file.name.max <- paste0('~/data/climate/net_cdf/',dname,'/processed_era5/','worldwide_',dname,'_',freq,'_',num,'_',year,'_max.rds')
# saveRDS(dat.max, file.name.max)

# extract csv file of latitude longitude for USA if required
# USA.lonlat <- lonlat
# names(USA.lonlat) <- c('lon','lat')
# USA.lonlat$lon <- as.numeric(USA.lonlat$lon)
# USA.lonlat$lat <- as.numeric(USA.lonlat$lat)
# #USA.lonlat <- subset(lonlat, lon>231)# & lon<296 & lat>23 & lat<50)
# #USA.lonlat <- subset(lonlat, lon<296)
# #USA.lonlat <- subset(lonlat, lat>23)
# #USA.lonlat <- subset(lonlat, lat<50)
# write.csv(USA.lonlat,'global_lon_lat_era5.csv')