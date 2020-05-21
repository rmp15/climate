#!/bin/bash

# this script
# makes anomaly summaries for the chosen administrative level

clear

# arguments for running script
#declare -a countries=("FRA" "NOR" "BEL" "DNK" "AUT" "NLD" "ITA" "CAN" "GBR")
declare -a countries=("BEL")
declare -i year_start=2011 # normally 2010
declare -i year_end=2020
declare -a dnames=("t2m")
declare -a times=("daily")
declare -a nums=("four")
declare -a admin_levels=("0" "1")

#################################################
# ANOMALY CALCULATION
#################################################

for country in "${countries[@]}"; do
for dname in "${dnames[@]}"; do
for time in "${times[@]}"; do
for num in "${nums[@]}"; do
for admin_level in "${admin_levels[@]}"; do

echo "Calculating anomalies by admin units for week or month scale";

:
# identifies the overlap between grids and LADs codes and creates weighted means
Rscript ~/git/climate/countries/Worldwide/prog/02_anomalies/anomalies.R $year_start $year_end $dname $time $num $admin_level $country &

done; done; done; done; done;