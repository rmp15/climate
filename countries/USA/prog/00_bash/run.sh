#!/bin/bash

#Rscript ~/git/climate/countries/USA/prog/02_grid_county_intersection/grid_county_intersection_unproj_function.R

#Rscript ~/git/climate/countries/USA/prog/03_population_weighted_mean/population_weighted_mean.R

for year in $(seq 1982);

do 

echo $year;

#Rscript ~/git/climate/countries/USA/prog/01_extract_netcdf/extracting_netcdf_files.R $year

#Rscript ~/git/climate/countries/USA/prog/04_state_weighted_mean_summary/state_weighted_mean_summary.R $year

#Rscript ~/git/climate/countries/USA/prog/05_plots/plots.R $year

#for month in $(seq 2 12);

Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R 2 $year
Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R 3 $year
Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R 4 $year
Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R 5 $year
Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R 6 $year
Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R 7 $year
Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R 8 $year
Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R 9 $year
Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R 10 $year
Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R 11 $year
Rscript ~/git/climate/countries/USA/prog/06_bil_files/bil_files.R 12 $year


#done

done
