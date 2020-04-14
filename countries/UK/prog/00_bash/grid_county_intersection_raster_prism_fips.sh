#!/bin/bash

# this script
# identifies overlap between counties and temperature grids and outputs county-level summaries

clear

declare -a years=($(seq 2010 2019))
declare -a dnames=("t2m")
declare -a times=("daily")
declare -a nums=("four")
declare -a spaces=("lad")

#################################################
# 1. WEIGHTED MEAN CALCULATION
#################################################

for year in "${years[@]}"; do
for dname in "${dnames[@]}"; do
for time in "${times[@]}"; do
for num in "${nums[@]}"; do
for space in "${spaces[@]}"; do

echo "Identifying overlap between rasters and LADs codes for temperature processing";
echo $year;

:
# identifies the overlap between grids and LADs codes and creates weighted means
Rscript ~/git/climate/countries/UK/prog/01_extract_netcdf/extrac $year $dname $time $num $space &

echo "Hello";

done; done; done; done; done;