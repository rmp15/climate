rm(list=ls())

library(foreign)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])
dname <- as.character(args[2])
var <- as.character(args[3])

print(year)

# load county summary by day (for single year)
dir.input = paste0("../../output/metrics_development_county/",dname,'/',var,'_',dname,'/')
dat.county <- readRDS(paste0(dir.input,'county_summary_',var,'_',dname,'_',year,'.rds'))
print('weather data loaded')

# load population data
dat_nchs = read.csv('~/git/pollution/countries/USA/data/population/raw/nchs_raw_annotated_withag_1990_to_2016')
pop_nchs_allage <- as.data.frame(summarise(group_by(subset(dat_nchs,sex==id_sex),year,fips,sex),popsum=sum(popsum)))
popsum_nchs <- subset(pop_nchs_allage,sex==id_sex)
ap_pop_nchs <- left_join(dat.county,popsum_nchs)

# account for missing data due to merging/splitting counties
ap_pop_nchs[ap_pop_nchs$fips == '08014' & ap_pop_nchs$year <= 1999,'popsum'] <- popsum_nchs[popsum_nchs$fips == '08014' & popsum_nchs$year == 2000,'popsum']
ap_pop_nchs[ap_pop_nchs$fips == '46113' & ap_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '46102' & popsum_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum']
ap_pop_nchs[ap_pop_nchs$fips == '51515' & ap_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '51515' & popsum_nchs$year %in% c(2009),'popsum']

# merging counties with population-weighted pollution
ap_pop_nchs_sctag <- combine_sc(ap_pop_nchs,scloc.df.sc) # WHAT IS combine_sc ?
ap_pop_nchs_sctag$appop <- ap_pop_nchs_sctag$pred.wght*ap_pop_nchs_sctag$popsum
ap_pop_nchs_sc <- data.frame(summarise(group_by(ap_pop_nchs_sctag,fips,year),apsc.wght=sum(appop)/sum(popsum),popsum=sum(popsum)))
ap_pop_nchs_mctag <- combine_mc(ap_pop_nchs_sc,scloc.df) # WHAT IS combine_mc ?
ap_pop_nchs_mctag$appop <- ap_pop_nchs_mctag$apsc.wght*ap_pop_nchs_mctag$popsum
ap_pop_nchs_mc <- data.frame(summarise(group_by(ap_pop_nchs_mctag,fips,year),apmc.wght=sum(appop)/sum(popsum)))
