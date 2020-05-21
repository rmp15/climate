#!/bin/bash

# this script
# identifies overlap between administrative regions and population grids and outputs administrative summaries
# shapefiles downloaded from
# http://www.diva-gis.org/gdata

clear

# arguments for running script
declare -a years=("2010")
declare -a countries=("FRA") # "NOR" "BEL" "DNK" "AUT" "NLD" "ITA" "CAN" "GBR")
declare -i year_pop=2010
declare -a admin_levels=("0") # "1")

#################################################
# POPULATION GRID SUMMARY
#################################################

for year in "${years[@]}"; do
for country in "${countries[@]}"; do
for admin_level in "${admin_levels[@]}"; do

echo "Calculating anomalies by admin units for week or month scale";

:
# identifies the overlap between grids and LADs codes and creates weighted means
Rscript ~/git/climate/countries/Worldwide/prog/03_population_grid/population_grid_summary.R $year $admin_level $country &

done; done; done;