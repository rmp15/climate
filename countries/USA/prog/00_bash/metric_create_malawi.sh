#!/bin/bash

# this script
# processes monthly temperature metrics at state level using population-weighted means 
# processes monthly precipitation metrics at state level using population-weighted means

clear

declare -a years=($(seq 1995 2017))
declare -a dnames=("t2m" "2d")

declare freq="daily"
declare num="four"

# go to correct location
# cd # IF DESIRED OUTPUT

#################################################
# 1. 2-METRE TEMPERATURE PROCESSING (t2m)
#################################################

for dname in "${dname[@]}"; do
for year in "${years[@]}"; do

echo "processing temperature variables for $year";

echo "converting temperature netcdf file for $year";

# processes net_cdf files
Rscript /c:/Users/hamil/Desktop/ecmwf/temp-95-17/prog/rscript/extracting_netcdf_files_malawi.R $year $dname $freq $num &

done; done;
