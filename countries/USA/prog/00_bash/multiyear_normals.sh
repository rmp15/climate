#!/bin/bash

# this script
# creates longterm normals of variables for states and counties in the USA

# go to correct location
cd ~/git/climate/countries/USA/prog/00_bash

clear

#################################################
# 1. 2-METRE TEMPERATURE PROCESSING (t2m)
#################################################

declare dname="t2m"

declare -a metrics=("sd")

for metric in "${metrics[@]}"; do

echo "processing multiyear normals for $dname $metric";

#Rscript ~/git/climate/countries/USA/prog/11_multiyear_normals/multiyear_nonnormals.R 1980 2009 $dname $metric
Rscript ~/git/climate/countries/USA/prog/11_multiyear_normals/multiyear_nonnormals_plots.R 1980 2009 $dname $metric


done; 


