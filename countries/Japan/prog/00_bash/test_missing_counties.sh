#!/bin/bash

# this script
# identifies counties which don't match up when trying to match up map and population county files

clear

declare -a years=($(seq 1982 2013))

#################################################
# 1. IDENTIFY MISMATCHING COUNTIES
#################################################

for year in "${years[@]}"; do

echo "identifying mismatching counties in $year";

# identifies the overlap between grids and counties and creates weighted means
Rscript /home/rmp15/git/climate/countries/USA/prog/09_test_missing_counties/test_missing_counties.R $year

done;

