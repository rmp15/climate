rm(list=ls())

library(foreign)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
var <- as.character(args[4])

# for testing code
year.start = 1999 ; year.end = 2015 ; dname = 't2m' ; var = 'ymean'

# years of study
years = c(year.start:year.end)

# load county summary
dir.input = paste0("../../output/metrics_development_county/",dname,'/',var,'_',dname,'/')

# cycle through years adding all data together
dat.county = data.frame()
for(year in years) {
    print(year)
    dat.county.current <- readRDS(paste0(dir.input,'county_summary_',var,'_',dname,'_',year,'.rds'))
    dat.county = rbind(dat.county,dat.county.current)
}
print('weather data loaded')

# load population data
load('~/git/pollution/countries/USA/data/population/raw/nchs_raw_annotated_withag_1990_to_2016')
# pop_nchs_allage <- as.data.frame(summarise(group_by(subset(dat_nchs,sex==id_sex),year,fips,sex),popsum=sum(popsum)))
id_sex=1
dat_nchs = as.data.frame(dat_nchs)
pop_nchs_allage = ddply(subset(dat_nchs,sex==id_sex),.(year,fips,sex),summarize,popsum=sum(popsum))
popsum_nchs <- subset(pop_nchs_allage,sex==id_sex)
# ap_pop_nchs <- left_join(dat.county,popsum_nchs)

# fix county codes for transient fips codes
# 46 113 to 46 102
# 08 013 to 08 014 (?)
popsum_nchs$fips[popsum_nchs$fips=='46113'] <- '46102'
# merge county yearly data with population
ap_pop_nchs <- merge(dat.county,popsum_nchs,by.x=c('year','state.county.fips'),by.y=c('year','fips'),all.x=TRUE)
ap_pop_nchs$fips = ap_pop_nchs$state.county.fips ; ap_pop_nchs$state.county.fips = NULL
ap_pop_nchs[ap_pop_nchs$fips == '08014' & ap_pop_nchs$year <= 1999,'popsum'] <- popsum_nchs[popsum_nchs$fips == '08014' & popsum_nchs$year == 2000,'popsum']

# examine NAs in above if desired to check
# ap_popnchs_na = ap_pop_nchs[rowSums(is.na(ap_pop_nchs))>0,]

# load myserious functions
source('~/git/pollution/countries/USA/prog/04_supercounty_functions/combine_sc.R')
source('~/git/pollution/countries/USA/prog/04_supercounty_functions/combine_mc.R')

# load information for merged/super counties
scloc.sc <- read.dta('~/git/pollution/countries/USA/data/super_counties/scfips.dta')
scloc.df.sc <- data.frame(lapply(scloc.sc, as.character), stringsAsFactors=FALSE)

scloc <- readRDS('~/git/pollution/countries/USA/data/super_counties/mfips_25000')
scloc.df <- data.frame(lapply(scloc, as.character), stringsAsFactors=FALSE)

# merging counties with population-weighted pollution TO FINISH FROM HERE
#1. make consistent over time
ap_pop_nchs$fips.old = ap_pop_nchs$fips
ap_pop_nchs_sctag <- combine_sc(ap_pop_nchs,scloc.df.sc)


# # create summary table to get state populations
pop.county.wm <- ddply(ap_pop_nchs_sctag,.(year,fips),summarize,popsum.merged=sum(popsum))
pop.county.wm <- merge(ap_pop_nchs_sctag,pop.county.wm,by=c('year','fips'),all.x=1)
pop.county.wm$pred.wght <- with(pop.county.wm,popsum/popsum.merged)
pop.county.wm = pop.county.wm[,c(1,3,8,10)]

# examine NAs in above if desired to check
# pop.county.wm.na = ap_pop_nchs[rowSums(is.na(pop.county.wm))>0,]

#
# # TO FINISH BELOW
# ap_pop_nchs_sctag_test = merge(ap_pop_nchs_sctag,pop.county.wm,by.x=c('year','state.fips','fips.old'),by.y=c('year','state.fips','fips.old'),all.x=TRUE)
ap_pop_nchs_sctag$appop <- ap_pop_nchs_sctag$pred.wght*ap_pop_nchs_sctag$popsum

# ap_pop_nchs_sc <- data.frame(summarise(group_by(ap_pop_nchs_sctag,fips,year),apsc.wght=sum(appop)/sum(popsum),popsum=sum(popsum)))
ap_pop_nchs_sc = ddply(ap_pop_nchs_sctag,.(fips,year),summarize,apsc.wght=sum(appop)/sum(popsum),popsum=sum(popsum))


#2. make merged counties
ap_pop_nchs_mctag <- combine_mc(ap_pop_nchs_sc,scloc.df)
ap_pop_nchs_mctag$appop <- ap_pop_nchs_mctag$apsc.wght*ap_pop_nchs_mctag$popsum
# ap_pop_nchs_mc <- data.frame(summarise(group_by(ap_pop_nchs_mctag,fips,year),apmc.wght=sum(appop)/sum(popsum)))
ap_pop_nchs_mc = ddply(ap_pop_nchs_mctag,.(fips,year),summarize,apmc.wght=sum(appop)/sum(popsum))

# save
