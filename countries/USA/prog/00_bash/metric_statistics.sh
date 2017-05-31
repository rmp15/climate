#!/bin/bash

# this script
# processes statistics of climate variables

clear

declare -i start=1979
declare -i end=2015
declare dname="t2m"
declare -a metrics=('10percc3' '90percc3' 'days_changing_by_5' 'mean' 'meanc3' 'number_of_min_3_day_above_+5_jumpupwaves' 'number_of_min_3_day_above_nonnormal_90_upwaves' 'number_of_min_3_day_below_+5_jumpdownwaves' 'number_of_min_3_day_below_nonnormal_90_downwaves' 'number_of_min_5_day_above_+5_jumpupwaves_2' 'days_above_30' 'days_below_10' 'number_of_days_above_nonnormal_90' 'number_of_min_3_day_above_+5_jumpupwaves_2' 'number_of_min_5_day_below_+5_jumpdownwaves_2')

for metric in "${metrics[@]}"; do

# climate statistics script
Rscript ~/git/climate/countries/USA/prog/13_metrics_statistics/metrics_statistics.R $start $end $dname $metric

done;
