# NOT STARTED YET

# Extracting daily NetCDF files for ERA-5

# From:
# https://github.com/rmp15/climate/blob/master/extract_netcdf_data.R
# http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm

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
dname <- as.character(args[2])
freq <- as.character(args[3])
num <- as.character(args[4])
year <- as.character(args[5])
space.res <- as.character(args[6])

# create directory to place output files into
dir.output = "../../output/grid_county_intersection_raster/"
if(space.res=='lad'){dir.output=paste0(dir.output,'lads/',state,'/')}
ifelse(!dir.exists(dir.output), dir.create(dir.output), FALSE)

# load shapefile of entire United Kingdom originally from http://geoportal.statistics.gov.uk/datasets/ae90afc385c04d869bc8cf8890bd1bcd_1
uk.national <- readOGR(dsn="../../data/shapefiles/Local_Authority_Districts_December_2017_Full_Clipped_Boundaries_in_Great_Britain",layer="Local_Authority_Districts_December_2017_Full_Clipped_Boundaries_in_Great_Britain")

# get projection of shapefile
original.proj = proj4string(uk.national)

# fix long to match raster (is this right?)
# uk.national$long = ifelse(uk.national$long<0, uk.national$long + 360, uk.national$long)

dname <- 't2m' ; freq <- 'daily' ; num <- 'four' ; year <- '2010' ; space.res='lad'

print(paste0('running extracting_netcdf_files.R for ',year))

# function to perform analysis for entire country
uk.analysis = function(uk.national,raster.input,output=0) {

    if(space.res=='lad'){
        # obtain a list of zip codes in a particular state
        lads = sort(unique(as.character(uk.national$lad17cd)))
    }

    # create empty dataframe to fill with zip code summary information
    weighted.area = data.frame()

    for(lad in lads) {

        # process lad preamble
        lad = as.character(lad)

        # isolate zip to highlight
        uk.lad = uk.national[uk.national$lad17cd %in% lad,]

        current.value = extract(x=raster.input,weights = TRUE,normalizeWeights=TRUE,y=uk.lad,fun=mean,df=TRUE,na.rm=TRUE)

        to.add = data.frame(zip,value=current.value[1,2])
        weighted.area = rbind(weighted.area,to.add)

        plot(uk.lad)
    }

    names(weighted.area) = c('lad',dname)

    return(weighted.area)

}

# FINISH LATER
# perform analysis across every day of selected year
# loop through each raster file for each day and summarise
dates <- seq(as.Date(paste0('0101',year),format="%d%m%Y"), as.Date(paste0('3112',year),format="%d%m%Y"), by=1)

# empty dataframe to load summarised national daily values into
weighted.area.national.total = data.frame()

# loop through each day of the year and perform analysis
print(paste0('Processing dates in ',year))
for(date in dates){

    print(format(as.Date(date), "%Y-%m-%d"))

    # load raster for relevant date
    raster.full <- raster(paste0('~/data/climate/net_cdf/',dname,'/raw_era5_daily/','worldwide_',dname,'_',freq,'_',num,'_',date,'.nc'))
    raster.full = projectRaster(raster.full, crs=original.proj) # ERROR HERE WHY???

    # flatten the raster's x values per day TO FINISH
    # raster.full <- calc(raster.full, fun = mean)

    # create empty dataframe to fill with zip code summary information
    weighted.area.national = data.frame()

    # perform loop across all states
    system.time(
    analysis.dummy = uk.analysis(uk.national,raster.full)
    analysis.dummy$date = format(as.Date(date), "%d/%m/%Y")
    analysis.dummy$day = day
    analysis.dummy$month = month
    analysis.dummy$year = year

    weighted.area.national = rbind(weighted.area.national,analysis.dummy)
    }
    )

    # weighted.area.national = weighted.area.national[,c(3,1,2)]
    weighted.area.national.total = rbind(weighted.area.national.total,weighted.area.national)
}

# save file
# dir.output = ???
saveRDS(weighted.area.national.total,paste0(dir.output,'weighted_area_raster_lads_',dname,'_',freq,'_',as.character(year),'.rds'))
