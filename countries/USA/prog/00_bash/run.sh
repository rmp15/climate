#!/bin/bash

#################################################
# 1. WEIGHTED MEAN PREPARATION
#################################################

# identifies the overlap between grids and counties and creates weighted means
#Rscript ~/git/climate/countries/USA/prog/02_grid_county_intersection/grid_county_intersection_unproj_function.R

# creates a weighted mean per state based on county populations
#Rscript ~/git/climate/countries/USA/prog/03_population_weighted_mean/population_weighted_mean.R

#################################################
# 2. TEMPERATURE PROCESSING
#################################################

for year in $(seq 1982 1982);

do

echo $year;

# processes net_cdf files
#Rscript ~/git/climate/countries/USA/prog/01_extract_netcdf/extracting_netcdf_files.R $year t2m daily four

# creates a weighted mean from grid county intersection of temperature per day per county for year 
#Rscript ~/git/climate/countries/USA/prog/04_county_weighted_mean_summary/county_weighted_mean_summary.R $year t2m daily four

# creates metrics from the temperature values processed
#Rscript ~/git/climate/countries/USA/prog/05_metrics_development/metrics_development.R $year t2m

# plots
Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year t2m mean
Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year t2m days_below_10
Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year t2m days_above_30
Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year t2m sd
Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year t2m days_changing_by_5
Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year t2m days_increasing_by_5
Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year t2m days_decreasing_by_5
Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year t2m number_of_min_3_day_above_25_upwaves
Rscript ~/git/climate/countries/USA/prog/06_plots/plots.R $year t2m number_of_min_3_day_below_5_downwaves

done;

#################################################
# 3. PRECIPITATION PROCESSING
#################################################

