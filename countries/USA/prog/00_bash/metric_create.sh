#!/bin/bash

# this script
# processes monthly temperature metrics at state level using population-weighted means 
# processes monthly precipitation metrics at state level using population-weighted means

clear

declare -a years=($(seq 1979 2015))
declare freq="daily"
declare num="four"
declare -i start=1982
declare -i end=2013

#################################################
# 1. 2-METRE TEMPERATURE PROCESSING (t2m)
#################################################

declare dname="t2m"

for year in "${years[@]}"; do

echo "processing temperature variables for $year";

echo "converting temperature netcdf file for $year";

# processes net_cdf files
#Rscript ~/git/climate/countries/USA/prog/01_extract_netcdf/extracting_netcdf_files.R $year $dname $freq $num

echo "creating temperature metric for counties for $year";

# creates a weighted mean from grid county intersection of temperature per day per county for year 
#Rscript ~/git/climate/countries/USA/prog/04_county_weighted_mean_summary/county_weighted_mean_summary.R $year $dname $freq $num &

#echo "creating temperature metric for states for $year";

# creates metrics from the temperature values processed
# NEEDS TO OUTPUT VARIABLES TO A LOCATION WHERE THEY CAN BE READ BY THE PLOTTING FUNCTION
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development.R $year $dname $start $end
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development_2.R $year $dname $start $end

# SEE ABOVE ABOUT LOCATION OF VARIABLES FOR PLOTTING FUNCTION
declare -a metrics=("mean" "meanc" "meanc2" "days_below_10" "days_above_30" "sd" "days_changing_by_5" "days_increasing_by_5" "days_decreasing_by_5" "number_of_min_3_day_above_99_upwaves" "number_of_min_3_day_below_99_downwaves")

#echo "plotting temperature metric results for $year";

for metric in "${metrics[@]}"; do

:
# plots
#Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year $dname $metric

done; done;

for metric in "${metrics[@]}"; do

:
# bind together data for each variable
#Rscript ~/git/climate/countries/USA/prog/08_bind_results/bind_results.R ${years[0]} ${years[-1]} $dname $metric

done;

# SEE ABOVE ABOUT LOCATION OF VARIABLES FOR PLOTTING FUNCTION
declare -a metrics=("meanc")

for metric in "${metrics[@]}"; do

:
# create climate region values
Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development_climate_regions.R 1979 2015 1982 2013 $dname $metric

done;

for metric in "${metrics[@]}"; do

:
# create seasonality index climate values
#Rscript ~/git/climate/countries/USA/prog/10_seasonality_index/seasonality_index_climate_regions.R 1982 2013 1982 2013 $dname $metric

done;

