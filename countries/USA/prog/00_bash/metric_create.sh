#!/bin/bash

# this script
# processes monthly temperature metrics at state level using population-weighted means 
# processes monthly precipitation metrics at state level using population-weighted means

clear

declare -a years=($(seq 1979 2015))
declare freq="daily"
declare num="four"
declare -i start=1979
declare -i end=1980

# go to correct location
cd ~/git/climate/countries/USA/prog/00_bash

#################################################
# 1. 2-METRE TEMPERATURE PROCESSING (t2m)
#################################################

declare dname="t2m"

for year in "${years[@]}"; do

echo "processing temperature variables for $year";

echo "converting temperature netcdf file for $year";

# processes net_cdf files
#Rscript ~/git/climate/countries/USA/prog/01_extract_netcdf/extracting_netcdf_files.R $year $dname $freq $num &

echo "creating temperature metric for counties for $year";

# creates a weighted mean from grid county intersection of temperature per day per county for year 
#Rscript ~/git/climate/countries/USA/prog/04_county_weighted_mean_summary/county_weighted_mean_summary.R $year $dname $freq $num &

#echo "creating temperature metric for states for $year";

# creates metrics from the temperature values processed
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development.R $year $dname $start $end &
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development_county.R $year $dname $start $end

declare -a metrics=('meanc4')

echo "plotting temperature metric results for $year";

for metric in "${metrics[@]}"; do
#declare -a metrics=("mean" "meanc" "meanc2" "days_below_10" "days_above_30" "sd" "days_changing_by_5" "days_increasing_by_5" "days_decreasing_by_5" "number_of_min_3_day_above_99_upwaves" "number_of_min_3_day_above_95_upwaves" "number_of_min_3_day_above_90_upwaves" "number_of_min_3_day_below_99_downwaves" "number_of_min_3_day_below_95_downwaves" "number_of_min_3_day_below_90_downwaves")

:
# plots
#Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year $dname $metric

done; done;

for metric in "${metrics[@]}"; do

:
# bind together data for each variable and metric statistics
#Rscript ~/git/climate/countries/USA/prog/08_bind_results/bind_results.R ${years[0]} ${years[-1]} $dname $metric
Rscript ~/git/climate/countries/USA/prog/08_bind_results/bind_results.R 1979 2015 $dname $metric
#Rscript ~/git/climate/countries/USA/prog/13_metrics_statistics/metrics_statistics.R ${years[0]} ${years[-1]} $dname $metric

done;

# SEE ABOVE ABOUT LOCATION OF VARIABLES FOR PLOTTING FUNCTION

for metric in "${metrics[@]}"; do

:
# create climate region values
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development_climate_regions.R 1979 2015 1982 2013 $dname $metric

done;

for metric in "${metrics[@]}"; do

:
# create seasonality index climate values
#Rscript ~/git/climate/countries/USA/prog/10_seasonality_index/seasonality_index_climate_regions.R 1982 2013 1982 2013 $dname $metric

done;
