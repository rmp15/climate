#!/bin/bash

# this script
# identifies overlap between counties and temperature grids and outputs county-level summaries

clear

# arguments for running script
declare -a countries=("BEL" "NOR" "FRA") # also "CAN" UK Denmark Austria Netherlands USA
declare -a years=($(seq 2010 2010))
declare -a dnames=("t2m")
declare -a times=("daily")
declare -a nums=("four")
declare -a spaces=("1")

# location of home location TO FINISH
#root_directory = $HOME
#next_level = "/git/climate/countries/Worldwide/"
#combined = "$root_directory$next_level"

#################################################
# 1. WEIGHTED MEAN CALCULATION
#################################################

for country in "${countries[@]}"; do
for year in "${years[@]}"; do
for dname in "${dnames[@]}"; do
for time in "${times[@]}"; do
for num in "${nums[@]}"; do
for space in "${spaces[@]}"; do

echo "Identifying overlap between rasters and admin codes for temperature processing";
echo $year;

:
# identifies the overlap between grids and LADs codes and creates weighted means
Rscript ~/git/climate/countries/Worldwide/prog/01_extract_netcdf/extracting_netcdf_files_era5.R $year $dname $time $num $space $country &

done; done; done; done; done; done;