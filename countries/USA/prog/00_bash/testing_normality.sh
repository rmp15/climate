#!/bin/bash

# this script
# test normality of variables generated from ERA-Interim

clear

declare -i start=1979
declare -i end=2015
declare dname='t2m'
declare -a metrics=("mean" "meanc" "meanc2" "days_below_10" "days_above_30" "sd" "days_changing_by_5" "days_increasing_by_5" "days_decreasing_by_5" "number_of_min_3_day_above_99_upwaves" "number_of_min_3_day_above_95_upwaves" "number_of_min_3_day_above_90_upwaves" "number_of_min_3_day_below_99_downwaves" "number_of_min_3_day_below_95_downwaves" "number_of_min_3_day_below_90_downwaves")

for metric in "${metrics[@]}"; do

Rscript ~/git/climate/countries/USA/prog/12_testing_normality/testing_normality.R $start $end $dname $metric

done;
