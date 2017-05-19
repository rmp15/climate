#!/bin/bash

# this script
# test normality of variables generated from ERA-Interim

clear

declare -i start=1979
declare -i end=2015
declare dname='t2m'
declare -a metrics=("meanc" "days_changing_by_5" "number_of_min_3_day_above_nonnormal_90_upwaves" "number_of_min_3_day_above_+5_jumpupwaves" "number_of_days_above_nonnormal_90")

for metric in "${metrics[@]}"; do

Rscript ~/git/climate/countries/USA/prog/12_testing_normality/testing_normality.R $start $end $dname $metric

done;
