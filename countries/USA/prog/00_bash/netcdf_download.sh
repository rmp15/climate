#!/bin/bash

# this script
# downloads netcdf files

clear

declare -i start=1979
declare -i end=2015
declare -a dnames=("t2m" "d2m")

#for dname in "${dnames[@]}"; do

#echo "processing for $dname years $start - $end"

(

python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_worldwide_years_sep.py $start $end 't2m';
python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_worldwide_years_sep.py $start $end 'd2m';

#) &

#done;

#(

#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_usa_years_sep.py $start $end 't2m';
#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_usa_years_sep.py $start $end 'd2m';

#) &

#(

#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_usa_years_together.py $start $end 't2m';
#python ~/git/climate/countries/USA/prog/01_extract_netcdf/downloading_netcdf_files_usa_years_together.py $start $end 'd2m';

) &
