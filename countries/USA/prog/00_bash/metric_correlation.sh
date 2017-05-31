#!/bin/bash

# this script
# examines the co-linearity between different climate statistics

clear

declare -i start=1979
declare -i end=2015
declare dname="t2m"
declare -a metrics=("meanc3" "10percc3" "90percc3")

for metric1 in "${metrics[@]}"; do
for metric2 in "${metrics[@]}"; do

echo "processing correlation between $metric1 and $metric2 for $dname $start - $end";

# correlations
Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_correlation.R $start $end $dname $metric1 $metric2

done; done;
