#!/bin/bash

# this script
# identifies overlap between administrative regions and temperature grids and outputs administrative summaries
# shapefiles downloaded from
# http://www.diva-gis.org/gdata

clear

# arguments for running script

# if updating 2021
#declare -a countries=("ALB" "ARM" "AUT" "AUS" "BEL" "BGR" "CAN" "CHE" "CYP" "CZE"
#                      "DEU" "DNK" "ESP" "EST" "FIN" "FRA" "GBR" "GEO" "GRC" "HRV"
#                      "HUN" "ISL" "ITA" "KOR" "LTU" "LVA" "MLT" "MNE" "NLD" "NOR"
#                      "NZL" "POL" "PRT" "ROU" "SRB" "SVK" "SVN" "SWE" "USA")
#declare -a years=($(seq 2021 2021))
#declare -a admin_levels=("1")

# if new country and need 2010 to 2020 (by default should be commented out)
declare -a countries=("ALB" "ARM" "CYP") # "EST" "GEO" "HRV" "LTU" "LVA" "MLT" "MNE" "SRB" "SVN")
declare -a years=($(seq 2010 2020))
declare -a admin_levels=("1")
##declare -a admin_levels=("0" "2")

# For Africa children hospital study (by default should be commented out)
#declare -a countries=("AFR")
#declare -a years=($(seq 2010 2019))
#declare -a admin_levels=("3")

declare -a dnames=("t2m")
declare -a times=("daily")
declare -a nums=("four")

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