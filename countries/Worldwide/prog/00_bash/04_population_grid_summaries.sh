#!/bin/bash

# this script
# identifies overlap between administrative regions and population grids and outputs administrative summaries

clear

# arguments for running script
declare -a countries=("BEL" "DNK"  "FRA" "GBR" "NOR") # also "AUS" "AUT" "CAN" "DEU" "ESP" "NLD" "NZL" "USA"
declare -i year_pop=2010
declare -a admin_levels=("0")

#################################################
# 1. ANOMALY CALCULATION
#################################################

for country in "${countries[@]}"; do
for admin_level in "${admin_levels[@]}"; do

echo "Calculating anomalies by admin units for week or month scale";

:
# identifies the overlap between grids and LADs codes and creates weighted means
Rscript ~/git/climate/countries/Worldwide/prog/03_populaltion_grid/population_grid_summary.R $year_pop $admin_level $country &

done; done;