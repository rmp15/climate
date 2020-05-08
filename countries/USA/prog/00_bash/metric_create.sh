#!/bin/bash

# this script
# processes monthly temperature metrics at state level using population-weighted means 
# processes monthly precipitation metrics at state level using population-weighted means

# I NEED TO TIDY THIS UP. BREAK UP INTO THESIS FORMATTING, PAPERS ETC.

clear

declare -a years=($(seq 1980 2017))
#declare -a years=(1981 1984 1994 2008)
#declare -a years=(1984)
declare freq="daily"
declare num="four"
declare -i start=1980
declare -i end=2009

# go to correct location
cd ~/git/climate/countries/USA/prog/00_bash

#################################################
# 1. PROCESSING OF FILES (choose variable below)
#################################################

declare dname="t2m"

for year in "${years[@]}"; do

echo "converting netcdf file for $year";

# processes net_cdf files
#Rscript ~/git/climate/countries/USA/prog/01_extract_netcdf/extracting_netcdf_files.R $year $dname $freq $num &

# processes net_cdf files ERA5
#Rscript ~/git/climate/countries/USA/prog/01_extract_netcdf/population_weighted_mean.R $year $dname $freq $num &

echo "creating metric for counties for $year";

declare -a types=('mean')

for type in "${types[@]}"; do

:
# creates a weighted mean from grid county intersection of temperature per day per county for year 
#Rscript ~/git/climate/countries/USA/prog/04_county_weighted_mean_summary/county_weighted_mean_summary.R $year $dname $freq $num $type &

# creates a weighted mean from grid county intersection of temperature per day per county for year ERA5
#Rscript ~/git/climate/countries/USA/prog/04_county_weighted_mean_summary/county_weighted_mean_summary_era5.R $year $dname $freq $num $type &

echo "creating temperature metric for states for $year";

# creates metrics from the temperature values processed
##Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development.R $year $dname $start $end &
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development_max_min.R $year $dname $start $end $type &
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development_county.R $year $dname $start $end

# creates yearly metrics for counties (Helen's pollution paper)
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development_county_yearly.R $year $dname

# (revisions to injury paper)

# creates monthly metrics for counties
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development_county_monthly.R $year $dname

done; done;

# (to place somewhere for Helen's pollution paper)
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development_county_yearly_supercounty.R 1999 2015 $dname 'ymean'

declare -a metrics=('meanc3')

echo "plotting temperature metric results for $year";

for metric in "${metrics[@]}"; do
#declare -a metrics=("mean" "meanc" "meanc2" "days_below_10" "days_above_30" "sd" "days_changing_by_5" "days_increasing_by_5" "days_decreasing_by_5" "number_of_min_3_day_above_99_upwaves" "number_of_min_3_day_above_95_upwaves" "number_of_min_3_day_above_90_upwaves" "number_of_min_3_day_below_99_downwaves" "number_of_min_3_day_below_95_downwaves" "number_of_min_3_day_below_90_downwaves")

: # plots
#Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year $dname $metric
#Rscript ~/git/climate/countries/USA/prog/06_plots/plots_against_time.R 1979 2016 $dname $metric
#Rscript ~/git/climate/countries/USA/prog/15_anomaly_summaries/anomaly_summaries.R 1979 2016 $dname $metric

done;

for metric in "${metrics[@]}"; do

for type in "${types[@]}"; do

:
# bind together data for each variable and metric statistics
#Rscript ~/git/climate/countries/USA/prog/08_bind_results/bind_results.R ${years[0]} ${years[-1]} $dname $metric $type
#Rscript ~/git/climate/countries/USA/prog/08_bind_results/bind_results.R 1979 2016 $dname $metric $type
#Rscript ~/git/climate/countries/USA/prog/08_bind_results/bind_results_county.R 1979 2016 $dname "mmean" $type

done;

# detrend monthly metrics for counties
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/detrend_county_monthly.R $year $dname

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
