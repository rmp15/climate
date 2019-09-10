#!/bin/bash

# this script
# downloads netcdf files

clear

declare -i start=1981
declare -i end=1981
declare -a dnames=("t2m" "d2m")

#for dname in "${dnames[@]}"; do

#echo "processing for $dname years $start - $end"

# ERA-Interim

#(

#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_worldwide_years_sep.py $start $end 't2m';
#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_worldwide_years_sep.py $start $end 'd2m';
#python ~/git/climate/countries/USA/prog/01_extract_netcdf/sophie_test.py $start $end '2d';

#) &

#done;

#(

#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_usa_years_sep.py $start $end 't2m';
#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_usa_years_sep.py $start $end 'd2m';

#) &

#(
#:
#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_usa_years_together.py $start $end 't2m';
#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_usa_years_together.py $start $end 'd2m';

#) &

# ERA5

python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_worldwide_years_sep_era5.py $start $end 't2m';
#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_worldwide_years_sep.py $start $end 'd2m';