#!/bin/bash

# this script
# downloads netcdf files

clear

declare -i start=1979
declare -i end=2015
declare -a dname=("t2m" "other")

(

for dname in "${dnames[@]}"; do

echo "processing for $dname years $start - $end"

python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files.py $start $end $dname ;

done;

) &