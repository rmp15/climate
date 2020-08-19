#!/bin/bash

# this script
# identifies overlap between administrative regions and temperature grids and outputs administrative summaries
# shapefiles downloaded from
# http://www.diva-gis.org/gdata

clear

# arguments for running script

# if updating 2020
#declare -a countries=("NZL" "ESP" "SWE" "ISL" "BEL" "FRA" "NOR" "DNK" "AUT" "NLD" "ITA" "CAN" "GBR" "BGR" "CZE" "FIN" "HUN" "PRT" "CHE" "POL" "SVK" "AUS")
#declare -a years=($(seq 2020 2020))

# if new country and need 2010 to 2019
declare -a countries=("AUS")
declare -a years=($(seq 2010 2019))

declare -a dnames=("t2m")
declare -a times=("daily")
declare -a nums=("four")
declare -a admin_levels=("0" "1")

#################################################
# WEIGHTED MEAN CALCULATION
#################################################

for country in "${countries[@]}"; do
for year in "${years[@]}"; do
for dname in "${dnames[@]}"; do
for time in "${times[@]}"; do
for num in "${nums[@]}"; do
for admin_level in "${admin_levels[@]}"; do

echo "Identifying overlap between rasters and admin codes for temperature processing";

:
# identifies the overlap between grids and administrative codes and creates weighted means
Rscript ~/git/climate/countries/Worldwide/prog/01_extract_netcdf/extracting_netcdf_files_era5.R $year $dname $time $num $admin_level $country &

done; done; done; done; done; done;