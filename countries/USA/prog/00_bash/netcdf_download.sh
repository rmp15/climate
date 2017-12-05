#!/bin/bash

# this script
# downloads netcdf files

clear

declare -i start=1981
declare -i end=2015
declare -a dnames=("t2m" "d2m")

#for dname in "${dnames[@]}"; do

#echo "processing for $dname years $start - $end"

(

python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_years_sep.py $start $end 't2m';
python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_years_sep.py $start $end 'd2m';

) &

#done;
