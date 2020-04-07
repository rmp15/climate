#!/bin/bash

# this script
# downloads netcdf files

clear

declare -i start_day='01'
declare -i start_month='01'
declare -i start_year='2020'
declare -i end_day='01'
declare -i end_month='02'
declare -i end_year='2020'
declare -a dnames=("t2m")


python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_uk_day_sep_era5_cdsapi.py $start_day $start_month $start_year $end_day $end_month $end_year 't2m';
