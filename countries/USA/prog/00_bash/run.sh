#!/bin/bash

#Rscript ~/git/climate/countries/USA/prog/02_grid_county_intersection/grid_county_intersection_unproj_function.R

#Rscript ~/git/climate/countries/USA/prog/03_population_weighted_mean/population_weighted_mean.R

for year in $(seq 1982 1983);

do 

echo $year;

#Rscript ~/git/climate/countries/USA/prog/01_extract_netcdf/extracting_netcdf_files.R $year

#Rscript ~/git/climate/countries/USA/prog/04_state_weighted_mean_summary/state_weighted_mean_summary.R $year

#Rscript ~/git/climate/countries/USA/prog/05_plots/plots.R $year

for month in $(seq 1 12);

Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R $month $year

done

done
