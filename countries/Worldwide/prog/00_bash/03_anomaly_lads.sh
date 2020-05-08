#!/bin/bash

# this script
# identifies overlap between counties and temperature grids and outputs county-level summaries

clear

declare -a dnames=("t2m")
declare -a times=("daily")
declare -a nums=("four")
declare -a spaces=("lad")

#################################################
# 1. WEIGHTED MEAN CALCULATION
#################################################

for dname in "${dnames[@]}"; do
for time in "${times[@]}"; do
for num in "${nums[@]}"; do
for space in "${spaces[@]}"; do

echo "Calculating anomalies by LAD for week or month scale";

:
# identifies the overlap between grids and LADs codes and creates weighted means
Rscript ~/git/climate/countries/UK/prog/02_anomaly_lads/anomaly_lads.R 2010 2020 $dname $time $num $space &

done; done; done; done;