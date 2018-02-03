#!/bin/bash

# this script
# creates longterm normals of variables for states and counties in the USA

# go to correct location
cd ~/git/climate/countries/USA/prog/00_bash

clear

#################################################
# 1. 2-METRE TEMPERATURE PROCESSING (t2m)
#################################################

declare dname="tapp"

# SEE ABOVE ABOUT LOCATION OF VARIABLES FOR PLOTTING FUNCTION
#declare -a metrics=("mean" "days_below_10" "days_above_30"
# "sd" "days_changing_by_5" "days_increasing_by_5"
# "days_decreasing_by_5"
# "number_of_min_3_day_above_99_upwaves"
# "number_of_min_3_day_below_99_downwaves")

declare -a metrics=("mean")

for metric in "${metrics[@]}"; do

echo "processing multiyear normals for $dname $metric";

Rscript ~/git/climate/countries/USA/prog/11_multiyear_normals/multiyear_normals.R 1980 2009 $dname $metric

done; 

#################################################
# 2. TOTAL PRECIPITATION PROCESSING (tp)
#################################################

declare dname="tp"


