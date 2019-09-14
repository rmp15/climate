#!/bin/bash

# this script
# processes monthly temperature metrics at state level using population-weighted means 
# processes monthly precipitation metrics at state level using population-weighted means

# I NEED TO TIDY THIS UP. BREAK UP INTO THESIS FORMATTING, PAPERS ETC.

clear

declare -a years=($(seq 2017 2017))
declare freq="daily"
declare num="four"
declare -i start=1980
declare -i end=2017

# go to correct location
cd ~/git/climate/countries/USA/prog/00_bash

declare dname="t2m"

#################################################
# 1. PROCESSING OF NETCDF FILES
#################################################

for year in "${years[@]}"; do

#echo "converting netcdf file for $year";

# processes net_cdf files ERA5
#Rscript ~/git/climate/countries/USA/prog/01_extract_netcdf/extracting_netcdf_files_era5.R $year $dname $freq $num &

#################################################
# 2. CREATE WEIGHTED MEAN SUMMARY OF COUNTIES
#################################################

#echo "creating metric for counties for $year";

declare -a types=('mean')

for type in "${types[@]}"; do

:

# creates a weighted mean from grid county intersection of temperature per day per county for year ERA5
#Rscript ~/git/climate/countries/USA/prog/04_county_weighted_mean_summary/county_weighted_mean_summary_era5.R $year $dname $freq $num $type

#################################################
# 3. CREATE WEIGHTED MEAN SUMMARY OF STATES
#################################################

echo "creating temperature metric for states for $year";

# creates metrics from the temperature values processed
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development_era5.R $year $dname $start $end

done; done;

#################################################
# 4. PLOT VALUES FOR EACH YEAR
#################################################

declare -a metrics=('mean')

for year in "${years[@]}"; do
for metric in "${metrics[@]}"; do

:
# bind together data for each variable and metric statistics
Rscript ~/git/climate/countries/USA/prog/06_plots/plots_era5.R $year $dname $metric

done; done;

#################################################
# 5. BIND RESULTS
#################################################

declare -a metrics=('meanc4')

for metric in "${metrics[@]}"; do

for type in "${types[@]}"; do

:
# plot each year of chosen variable
Rscript ~/git/climate/countries/USA/prog/08_bind_results/bind_results_era5.R 1980 2017 $dname $metric $type

done; done;
