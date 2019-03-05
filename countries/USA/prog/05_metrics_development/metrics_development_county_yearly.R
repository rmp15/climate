rm(list=ls())

#library(ggplot2)
library(foreign)
library(plyr)
#library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])
dname <- as.character(args[2])

print(year)

ifelse(!dir.exists(paste0("../../output/metrics_development/",dname)), dir.create(paste0("../../output/metrics_development/",dname)), FALSE)

# load county summary by day (for single year)
dat.county <- readRDS(paste0('../../output/county_weighted_mean_summary/county_daily/',dname,'/county_daily_',dname,'_',year,'.rds'))

# fix temperature if loaded
if(dname=='t2m'){
dat.county$t2m <- dat.county$t2m - 273.15
}

# fix precipitation if loaded
if(dname=='tp'){
    dat.county$tp <- dat.county$tp * 1000
}

# load weightings by county for state summary based on population
# state.weighting <- readRDS('~/data/climate/population_weightings/state_population_weightings.rds')

# filter for a year of interest
year.selected <- year
# state.weighting.filter <- subset(state.weighting,year %in% year.selected)

# leap year test
is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

dat.county$leap <- as.integer(is.leapyear(dat.county$year))

print('data loaded')

########################################################################################################
# 1. AVERAGE YEARLY COUNTY VALUES
########################################################################################################

print('processing ymean')

var <- paste0('ymean_',dname)

# process for finding average temperature
dat.at <- dat.county
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.at <- ddply(dat.at,.(year,state.county.fips),summarize,ymean=round(mean(variable),2))

dat.at$state.fips = substr(dat.at$state.county.fips,1,2)

# get rid of alaska and hawaii
dat.at = subset(dat.at,!(state.fips%in%c('02','15')))

# add state long-name
state.lookup = read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
state.lookup = state.lookup[,c(1,2)]
dat.at$state.fips = as.numeric(dat.at$state.fips)
dat.at = merge(dat.at,state.lookup,by.x='state.fips',by.y='fips')

# save output
dir.output = paste0("../../output/metrics_development_county/",dname,'/',var,'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output,recursive=TRUE), FALSE)
saveRDS(dat.at,paste0(dir.output,'county_summary_',var,'_',year.selected,'.rds'))

# plot values to check for weird ones

# combine state average deviation from county-based mean with county deviations from county-based means
dir.output = paste0("../../output/metrics_development_county/",dname,'/',var,'/plots/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

library(ggplot2)
pdf(paste0(dir.output,year,'.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(dat.at,!(state.fips%in%c('02','15')))) +
    geom_point(aes(x=state.county.fips,y=ymean)) +
    # geom_point(aes(x=state.fips,y=t2m.meanc4.y),color='red',size=2) +
    geom_hline(yintercept=0,linetype='dotted') +
    facet_wrap(~full_name,scales='free_x')
dev.off()

print(paste0(year,' done'))