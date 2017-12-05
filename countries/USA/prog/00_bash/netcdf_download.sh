#!/bin/bash

# this script
# downloads netcdf files

clear

declare -i start=1990
declare -i end=1990
declare -a dname=("t2m")

for dname in "${dnames[@]}"; do

(

echo "processing for $dname years $start - $end"

python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_years_sep.py $start $end dname

) &

done;
