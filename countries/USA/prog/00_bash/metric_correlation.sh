#!/bin/bash

# this script
# examines the co-linearity between different climate statistics

clear

declare -i start=1980
declare -i end=2013
declare dname="t2m"
declare metrics -a = ("meanc" "days_changing_by_5" "number_of_min_3_day_above_nonnormal_90_upwaves" "number_of_min_3_day_above_+5_jumpupwaves")

for metric1 in "${metrics[@]}"; do
for metric2 in "${metrics[@]}"; do

echo "processing correlation between $metric1 and $metric2 for $dname $start - $end";

# correlations
Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_correlation.R $start $end $dname $metric1 $metric2

done; done;
