# Extracting daily NetCDF files for ERA-5

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

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# names for files
year <- as.character(args[1])
dname <- as.character(args[2])
freq <- as.character(args[3])
num <- as.character(args[4])
space.res <- as.character(args[5])
country.id <- as.character(args[6])

# for code testing
# dname <- 't2m' ; freq <- 'daily' ; num <- 'four' ; year <- '2010' ; space.res='adm2' ; country.id = 'NOR'

# create directory to place output files into
dir.output = paste0("../../output/grid_county_intersection_raster/",country.id,'/',space.res,'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

# load shapefile of entire United Kingdom originally from http://geoportal.statistics.gov.uk/datasets/ae90afc385c04d869bc8cf8890bd1bcd_1
shapefile <- readOGR(dsn="../../data/shapefiles/BEL_adm",layer="BEL_adm2")

# transform into WSG84 (via https://rpubs.com/nickbearman/r-google-map-making)
shapefile <- sp::spTransform(shapefile, CRS("+init=epsg:4326"))

# get projection of shapefile
original.proj = proj4string(shapefile)

print(paste0('running extracting_netcdf_files.R for ',country.id,' ',space.res,' ',year))

# function to perform analysis for entire country
country.analysis = function(shapefile,raster.input,output=0) {

    if(space.res=='adm2'){
        # obtain a list of names in a particular state
        ids = sort(unique(as.numeric(shapefile$ID_2)))
    }

    # dataframe with values for each region for particular day
    weighted.area = extract(x=raster.input,weights = TRUE,normalizeWeights=TRUE,y=shapefile,fun=mean,df=TRUE,na.rm=TRUE)

    # convert to centigrade
    weighted.area$layer = round((weighted.area$layer - 273.15),2)
    names(weighted.area) = c('id',dname)
    return(weighted.area)

}

# perform analysis across every day of selected year
# loop through each raster file for each day and summarise
if(year<2020){dates <- seq(as.Date(paste0('0101',year),format="%d%m%Y"), as.Date(paste0('3112',year),format="%d%m%Y"), by=1)}
if(year==2020){dates <- seq(as.Date(paste0('0101',year),format="%d%m%Y"), as.Date(paste0('0505',year),format="%d%m%Y"), by=1)}
dates = as.character(dates)

# empty dataframe to load summarised national daily values into
weighted.area.national.total = data.frame()

# loop through each day of the year and perform analysis
print(paste0('Processing dates in ',year))
# for(date in dates){
for(date in dates[1:10]){

    print(as.character(date))

    # load raster for relevant date and change co-ordinates to -180 to 180
    raster.full <- raster(paste0('~/data/climate/net_cdf/',dname,'/raw_era5_daily/','worldwide_',dname,'_',freq,'_',num,'_',as.character(date),'.nc'))
    raster.full <- rotate(raster.full)

    # projet to be the same as the chosen country map
    raster.full = projectRaster(raster.full, crs=original.proj)

    # flatten the raster's x values per day
    raster.full <- calc(raster.full, fun = mean)

    # plot(raster.full)

    # create empty dataframe to fill with zip code summary information
    weighted.area.national = data.frame()

    # perform analysis
    analysis.dummy =  country.analysis(shapefile,raster.full)
    analysis.dummy$date = format(as.Date(date), "%Y-%m-%d")
    weighted.area.national = rbind(weighted.area.national,analysis.dummy)

    weighted.area.national = weighted.area.national[,c(3,1,2)]
    weighted.area.national.total = rbind(weighted.area.national.total,weighted.area.national)
}


# save file
saveRDS(weighted.area.national.total,paste0(dir.output,'weighted_area_raster_',country.id,'_',space.res,'_',dname,'_',freq,'_',as.character(year),'.rds'))
write.csv(weighted.area.national.total,paste0(dir.output,'weighted_area_raster_',country.id,'_',space.res,'_',dname,'_',freq,'_',as.character(year),'.csv'))