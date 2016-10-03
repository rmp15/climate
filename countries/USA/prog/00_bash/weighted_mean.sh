#!/bin/bash

# this script
# identifies overlap between counties and temperature grids

clear

#################################################
# 1. WEIGHTED MEAN PREPARATION
#################################################

echo "indentifying overlap between grids and counties for climate variable processing";

# identifies the overlap between grids and counties and creates weighted means
Rscript ~/git/climate/countries/USA/prog/02_grid_county_intersection/grid_county_intersection_unproj_function.R

echo "creating weighted mean per county from grid overlap";

# creates a weighted mean per state based on county populations
Rscript ~/git/climate/countries/USA/prog/03_population_weighted_mean/population_weighted_mean.R
