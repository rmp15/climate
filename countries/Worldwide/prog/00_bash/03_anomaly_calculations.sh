#!/bin/bash

# this script
# identifies overlap between counties and temperature grids and outputs county-level summaries

clear

# arguments for running script
declare -a countries=("BEL" "NOR" "FRA" "DNK" "GBR") # also "CAN" Austria Netherlands USA
declare -i year_start=2010
declare -i year_end=2020
declare -a dnames=("t2m")
declare -a times=("daily")
declare -a nums=("four")
declare -a admin_levels=("0")

#################################################
# 1. ANOMALY CALCULATION
#################################################

for dname in "${dnames[@]}"; do
for time in "${times[@]}"; do
for num in "${nums[@]}"; do
for admin_level in "${admin_levels[@]}"; do

echo "Calculating anomalies by LAD for week or month scale";

:
# identifies the overlap between grids and LADs codes and creates weighted means
Rscript ~/git/climate/countries/Worldwide/prog/02_anomalies/anomalies.R $year_start $year_end $dname $time $num $space &

done; done; done; done;