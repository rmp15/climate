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

# create dataframe with hospitals and countries in
country = c('South Africa','Zambia','Zimbabwe','Lesotho','Botswana','South Africa','Uganda','Ethiopia','Ethiopia','Sierra Leone','Liberia','Liberia','Kenya','Zimbabwe','Zambia','Nigeria','Nigeria','Malawi','Tanzania','Kenya','Cameroon','Rwanda','Kenya','Ghana')
city = c('Johannesburg','Lusaka','Harare','Maseru','Gaborone','Cape Town','Kampala','Addis Ababa','Addis Ababa','Freetown','Monrovia','Monrovia','Nairobi','Harare','Ndola','Benin City','Ibadan','Blantyre','Mwanza','Eldoret','Yaounde','Kigali','Nairobi','Accra')
hospital_name = c('*Rahima Moosa Mother and Child Hospital',
                 '*University Teaching Hospital',
                 '*Parirenyatwa group of Hospitals',
                 'Queen Mamohato Memorial Hospital',
                 'Princess Marina Hospital',
                 'Red Cross War Memorial Childrens Hospital',
                 'Mulago National Referral Hospital',
                 'St Paul’s Hospital',
                 'Black Lion (Tikur Anbessa) Hospital',
                 'Ola During Childrens Hospital',
                 'John F Kennedy Medical Center',
                 'Redemption Medical Center',
                 'Aga Khan University Hospital',
                 'Harare Childrens Hospital',
                 'Arthur Davison Childrens Hospital',
                 'University of Benin Teaching Hospital',
                 'University College Hospital Ibadan',
                 'Queen Elizabeth Central Hospital',
                 'Bugando Medical Centre',
                 'Moi University Referral Hospital',
                 'Mother and Child Center of Chantal BIYAs Foundation',
                 'University Teaching Hospital of Kigali (CHUK)',
                 'University of Nairobi Hospital',
                 'Korle-Bu Teaching Hospital')
location_lat = c(-26.187398,-15.431543,-17.811184,-29.341818,-24.656016,-33.953836,0.339008,0.363727,9.021039,8.490749,6.287651,6.388209,-1.257323,-17.851081,-12.946553,6.391495,7.357835,-15.802134,-2.527098,0.517979,3.871420,-1.955097,-1.272102,5.537633)
location_lon = c(27.973990,28.313470,31.042651,27.528745,25.923976,18.487381,32.576174,32.569650,38.749581,-13.216649,-10.773950,-10.792348,36.824570,31.012784,28.647463,5.611804,3.874229,35.021825,32.907712,35.280811,11.511161,30.060561,36.808693,-0.226440)
dat_hospital = data.frame(country=country, city=city,hospital_name=hospital_name,lon=location_lon,lat=location_lat)

# project co-ordinates to 4326 (from https://geocompr.robinlovelace.net/reproj-geo-data.html)
dat_hospital = sf::st_as_sf(dat_hospital, coords = c("lon", "lat"))
dat_hospital = st_set_crs(dat_hospital, 4326)
dat_hospital = as_Spatial(dat_hospital)

# shapefiles downloaded from
# http://www.diva-gis.org/gdata

# load shapefiles of chosen countries
#plot(dat_hospital,col='red')
shapefile_matched_master = SpatialPolygons(list())
countries = c('ZAF','ZMB', 'ZWE', 'BWA', 'UGA', 'ETH', 'SLE', 'LBR', 'KEN', 'NGA', 'MWI', 'TZA', 'CMR', 'RWA','GHA', 'LSO')
for(country.id in countries){
    print(paste0('finding overlap for ',country.id,' administrative level ',space.res))


    # load shapefile
    if(country.id=='LSO'){ # have to do this because LSO hasn't got a adm2 level
        shapefile <- readOGR(dsn=paste0("~/data/climate/shapefiles/",country.id,"_adm"),layer=paste0(country.id,"_adm1"))
        shapefile@data$ID_2 = shapefile@data$ID_1
    }
    if(country.id!='LSO'){shapefile <- readOGR(dsn=paste0("~/data/climate/shapefiles/",country.id,"_adm"),layer=paste0(country.id,"_adm",space.res))}

    # transform into WSG84 (via https://rpubs.com/nickbearman/r-google-map-making)
    shapefile <- sp::spTransform(shapefile, CRS("+init=epsg:4326"))

    # get projection of shapefile
    original.proj = proj4string(shapefile)

    # perform intersection test to establish which grid points intersect with administrative region
    shapefile_matched_ids <- as.vector(which(gIntersects(shapefile,dat_hospital,byid=TRUE)==TRUE,arr.ind=TRUE)[,c(2)])
    shapefile_matched = shapefile[shapefile@data$ID_2%in%shapefile_matched_ids,]

    #plot(shapefile);
    #plot(shapefile[shapefile@data$ID_2%in%shapefile_matched_ids,],border='blue',lwd=2,add=TRUE)

    # bind to master shapefile
    shapefile_matched_master = bind(shapefile_matched_master, shapefile_matched)

}

# create unique ID across countries
shapefile_matched_master@data$NAME_3 = with(shapefile_matched_master,paste0(shapefile_matched_master$NAME_0, '_',shapefile_matched_master$NAME_1))
shapefile_matched_master@data$ID_3 = 1:nrow(shapefile_matched_master)

# save shapefile
dir.output = "~/data/climate/shapefiles/AFR_adm/"
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)
writeOGR(obj=shapefile_matched_master, dsn=dir.output, layer="AFR_adm3", driver="ESRI Shapefile") # this is in geographical projection
#st_write(shapefile_matched_master,paste0(dir.output,'AFR_adm3.shp'),driver="ESRI Shapefile")

