#!/bin/bash

# this script
# identifies overlap between counties and temperature grids and outputs county-level summaries

clear

# arguments for running script
declare -a countries=("BEL" "NOR" "FRA" "DNK" "GBR") # also "CAN" Austria Netherlands USA
declare -a years=($(seq 2010 2020))
declare -a dnames=("t2m")
declare -a times=("daily")
declare -a nums=("four")
declare -a admin_levels=("0")

#################################################
# 1. WEIGHTED MEAN CALCULATION
#################################################

for country in "${countries[@]}"; do
for year in "${years[@]}"; do
for dname in "${dnames[@]}"; do
for time in "${times[@]}"; do
for num in "${nums[@]}"; do
for admin_level in "${admin_levels[@]}"; do

echo "Identifying overlap between rasters and admin codes for temperature processing";
echo $year;

:
# identifies the overlap between grids and administrative codes and creates weighted means
Rscript ~/git/climate/countries/Worldwide/prog/01_extract_netcdf/extracting_netcdf_files_era5.R $year $dname $time $num $admin_level $country &

done; done; done; done; done; done;