#!/bin/bash

# this script
# downloads netcdf files

clear

declare -i start='01012020'
declare -i end='01012020'
declare -a dnames=("t2m")


python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_worldwide_years_sep_era5_cdsapi.py $start $end 't2m';
#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_worldwide_years_sep.py $start $end 'd2m';