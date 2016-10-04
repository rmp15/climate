#!/bin/bash

# this script
# processes monthly temperature metrics at state level using population-weighted means 
# processes monthly precipitation metrics at state level using population-weighted means

clear

declare -a years=($(seq 1982 1991))

#################################################
# 1. 2-METRE TEMPERATURE PROCESSING (t2m)
#################################################

for year in "${years[@]}"; do

echo "processing temperature variables for $year";

echo "converting temperature netcdf file for $year";

# processes net_cdf files
#Rscript ~/git/climate/countries/USA/prog/01_extract_netcdf/extracting_netcdf_files.R $year t2m daily four

echo "creating temperature metric for counties for $year";

# creates a weighted mean from grid county intersection of temperature per day per county for year 
#Rscript ~/git/climate/countries/USA/prog/04_county_weighted_mean_summary/county_weighted_mean_summary.R $year t2m daily four

echo "creating temperature metric for states for $year";

# creates metrics from the temperature values processed
# NEEDS TO OUTPUT VARIABLES TO A LOCATION WHERE THEY CAN BE READ BY THE PLOTTING FUNCTION
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development.R $year t2m

# SEE ABOVE ABOUT LOCATION OF VARIABLES FOR PLOTTING FUNCTION
declare -a metrics=("mean" "days_below_10" "days_above_30" "sd" "days_changing_by_5" "days_increasing_by_5" "days_decreasing_by_5" "number_of_min_3_day_above_25_upwaves" "number_of_min_3_day_below_5_downwaves")

echo "plotting temperature metric results for $year";

for metric in "${metrics[@]}"; do

# plots
Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year t2m $metric

done;

done;

for metric in "${metrics[@]}"; do

# bind together data for each variable
Rscript ~/git/climate/countries/USA/prog/08_bind_results/bind_results.R ${years[0]} ${years[-1]} t2m $metric

done;

#################################################
# 2. TOTAL PRECIPITATION PROCESSING (tp)
#################################################

#for year in "${years[@]}"; do

#do

#echo "processing precipitation variables for $year";

#echo "converting precipitation netcdf file for $year";

# processes net_cdf files
#Rscript ~/git/climate/countries/USA/prog/01_extract_netcdf/extracting_netcdf_files.R $year tp daily four

#echo "creating precipitation metric for counties for $year";

# creates a weighted mean from grid county intersection of temperature per day per county for year
#Rscript ~/git/climate/countries/USA/prog/04_county_weighted_mean_summary/county_weighted_mean_summary.R $year tp daily four

#echo "creating precipitation metric for states for $year";

# creates metrics from the temperature values processed
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development.R $year tp

#echo "plotting precipitation metric results for $year";

#declare -a metrics=('mean' 'days_below_10' 'days_above_30' 'sd' 'days_changing_by_5' 'days_increasing_by_5' 'days_decreasing_by_5' 'number_of_min_3_day_above_25_upwaves' 'number_of_min_3_day_below_5_downwaves')

#for metric in "${metrics[@]}"; do

# delete this line when up and running
:
#Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year tp $metric

#done; done;
