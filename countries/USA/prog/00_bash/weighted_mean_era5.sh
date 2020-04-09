#!/bin/bash

# this script
# identifies overlap between counties and temperature grids
# for ERA5

clear

#################################################
# 1. WEIGHTED MEAN PREPARATION
#################################################

echo "indentifying overlap between grids and counties for climate variable processing";

# identifies the overlap between grids and counties and creates weighted means
#Rscript ~/git/climate/countries/USA/prog/02_grid_county_intersection/grid_lsoa_intersection_unproj_function_era5.R

echo "creating weighted mean per county from grid overlap";

# creates a weighted mean per state based on county populations
Rscript ~/git/climate/countries/USA/prog/03_population_weighted_mean/population_weighted_mean.R
