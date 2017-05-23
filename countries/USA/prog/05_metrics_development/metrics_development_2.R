rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])
dname <- as.character(args[2])
year.start <- as.numeric(args[3])
year.end <- as.numeric(args[4])

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
state.weighting <- readRDS('~/data/climate/population_weightings/state_population_weightings.rds')

# filter for a year of interest
year.selected <- year
state.weighting.filter <- subset(state.weighting,year %in% year.selected)

# leap year test
is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

dat.county$leap <- as.integer(is.leapyear(dat.county$year))

####################################################
# 18. NUMBER OF UPWAVES 4 (ABSOLUTE THRESHOLD 90th PERCENTILE NOT ASSUMING NORMALITY) (1980-2009)
####################################################
num.days <- 3
var <- paste0('number_of_min_',num.days,'_day_above_nonnormal_90_upwaves_2_',dname)

# load 90th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_nonnormals_mean_t2m_1980_2009.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# process for counting number of upwaves
dat.uw <- dat.county

# merge 99th percentile data with county temperature data
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))

# process for counting upwaves
dat.uw$above.threshold <- ifelse(dat.uw$variable>dat.uw$variable.30yr.ul,1,0)
dat.uw <- ddply(dat.uw, .(month,leap,year,state.county.fips), summarize, up.waves=length(rle(above.threshold)$lengths[rle(above.threshold)$values==1 & rle(above.threshold)$lengths>=num.days]))

# merge and create weighted mean for state
dat.temp <-merge(dat.uw,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*up.waves))
temp.state <- na.omit(temp.state)
temp.state <- temp.state[complete.cases(temp.state),]

# adjust to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
temp.state$var.adj <- ifelse(temp.state$month %in% c(1,3,5,7,8,10,12), temp.state$var.adj,
ifelse(temp.state$month %in% c(4,6,9,11), temp.state$var.adj*(31/30),
ifelse((temp.state$month==2 & temp.state$leap==0), temp.state$var.adj*(31/28),
ifelse((temp.state$month==2 & temp.state$leap==1), temp.state$var.adj*(31/29),
'ERROR'
))))
temp.state$var.adj <- round(as.numeric(temp.state$var.adj),2)

# round (is this right?) NO!
#temp.state$days.above.threshold <- round(temp.state$days.above.threshold)

# rename variable
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.uwo.',num.days,'.nn.d')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 19. NUMBER OF DOWNWAVES 4 (ABSOLUTE THRESHOLD 90th PERCENTILE NOT ASSUMING NORMALITY) (1980-2009)
####################################################
num.days <- 3
var <- paste0('number_of_min_',num.days,'_day_below_nonnormal_90_downwaves_2_',dname)

# load 90th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_nonnormals_mean_t2m_1980_2009.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# process for counting number of upwaves
dat.uw <- dat.county

# merge 99th percentile data with county temperature data
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))

# process for counting upwaves
dat.uw$above.threshold <- ifelse(dat.uw$variable<dat.uw$variable.30yr.ll,1,0)
dat.uw <- ddply(dat.uw, .(month,leap,year,state.county.fips), summarize, up.waves=length(rle(above.threshold)$lengths[rle(above.threshold)$values==1 & rle(above.threshold)$lengths>=num.days]))

# merge and create weighted mean for state
dat.temp <-merge(dat.uw,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*up.waves))
temp.state <- na.omit(temp.state)
temp.state <- temp.state[complete.cases(temp.state),]

# adjust to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
temp.state$var.adj <- ifelse(temp.state$month %in% c(1,3,5,7,8,10,12), temp.state$var.adj,
ifelse(temp.state$month %in% c(4,6,9,11), temp.state$var.adj*(31/30),
ifelse((temp.state$month==2 & temp.state$leap==0), temp.state$var.adj*(31/28),
ifelse((temp.state$month==2 & temp.state$leap==1), temp.state$var.adj*(31/29),
'ERROR'
))))
temp.state$var.adj <- round(as.numeric(temp.state$var.adj),2)

# round (is this right?) NO!
#temp.state$days.above.threshold <- round(temp.state$days.above.threshold)

# rename variable
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.dwb.',num.days,'.nn.d')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 27. NUMBER OF JUMPUPWAVES 2 (JUMP UP TO OVER 5 FOR 3 DAYS ABOVE LONGRUN MEAN (1980-2009) NOT ASSUMING NORMALITY)
####################################################

num.days <- 3
jump = 5
var <- paste0('number_of_min_',num.days,'_day_above_+',jump,'_jumpupwaves_2_',dname)

# load 90th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_95_nonnormals_mean_t2m_1980_2009.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# establish the 'jump' limits
dat.perc$variable.30yr.ul <- dat.perc$variable.30yr.mean + jump
dat.perc$variable.30yr.ll <- dat.perc$variable.30yr.mean - jump

# process for counting number of upwaves
dat.uw <- dat.county

# merge jump limit data with county temperature data
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))

# process for counting upwaves
dat.uw$above.threshold <- ifelse(dat.uw$variable>dat.uw$variable.30yr.ul,1,0)
dat.uw <- ddply(dat.uw, .(month,leap,year,state.county.fips), summarize, up.waves=length(rle(above.threshold)$lengths[rle(above.threshold)$values==1 & rle(above.threshold)$lengths>=num.days]))

# merge and create weighted mean for state
dat.temp <-merge(dat.uw,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*up.waves))
temp.state <- na.omit(temp.state)
temp.state <- temp.state[complete.cases(temp.state),]

# adjust to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
temp.state$var.adj <- ifelse(temp.state$month %in% c(1,3,5,7,8,10,12), temp.state$var.adj,
ifelse(temp.state$month %in% c(4,6,9,11), temp.state$var.adj*(31/30),
ifelse((temp.state$month==2 & temp.state$leap==0), temp.state$var.adj*(31/28),
ifelse((temp.state$month==2 & temp.state$leap==1), temp.state$var.adj*(31/29),
'ERROR'
))))
temp.state$var.adj <- round(as.numeric(temp.state$var.adj),2)

# rename variable
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.juwo.',num.days,'d.jo5')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 28. NUMBER OF JUMPUPWAVES 3 (JUMP UP TO OVER 5 FOR 5 DAYS ABOVE LONGRUN MEAN (1980-2009) NOT ASSUMING NORMALITY)
####################################################

num.days <- 5
jump = 5
var <- paste0('number_of_min_',num.days,'_day_above_+',jump,'_jumpupwaves_2_',dname)

# load 90th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_95_nonnormals_mean_t2m_1980_2009.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# establish the 'jump' limits
dat.perc$variable.30yr.ul <- dat.perc$variable.30yr.mean + jump
dat.perc$variable.30yr.ll <- dat.perc$variable.30yr.mean - jump

# process for counting number of upwaves
dat.uw <- dat.county

# merge jump limit data with county temperature data
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))

# process for counting upwaves
dat.uw$above.threshold <- ifelse(dat.uw$variable>dat.uw$variable.30yr.ul,1,0)
dat.uw <- ddply(dat.uw, .(month,leap,year,state.county.fips), summarize, up.waves=length(rle(above.threshold)$lengths[rle(above.threshold)$values==1 & rle(above.threshold)$lengths>=num.days]))

# merge and create weighted mean for state
dat.temp <-merge(dat.uw,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*up.waves))
temp.state <- na.omit(temp.state)
temp.state <- temp.state[complete.cases(temp.state),]

# adjust to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
temp.state$var.adj <- ifelse(temp.state$month %in% c(1,3,5,7,8,10,12), temp.state$var.adj,
ifelse(temp.state$month %in% c(4,6,9,11), temp.state$var.adj*(31/30),
ifelse((temp.state$month==2 & temp.state$leap==0), temp.state$var.adj*(31/28),
ifelse((temp.state$month==2 & temp.state$leap==1), temp.state$var.adj*(31/29),
'ERROR'
))))
temp.state$var.adj <- round(as.numeric(temp.state$var.adj),2)

# rename variable
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.juwo.',num.days,'d.jo5')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 30. NUMBER OF JUMPDOWNWAVES (JUMP DOWN TO OVER 5 FOR 3 DAYS BELOW LONGRUN MEAN (1980-2009) NOT ASSUMING NORMALITY)
####################################################

num.days <- 3
jump = 5
var <- paste0('number_of_min_',num.days,'_day_below_+',jump,'_jumpdownwaves_2_',dname)

# load 90th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_95_nonnormals_mean_t2m_1980_2009.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# establish the 'jump' limits
dat.perc$variable.30yr.ul <- dat.perc$variable.30yr.mean + jump
dat.perc$variable.30yr.ll <- dat.perc$variable.30yr.mean - jump

# process for counting number of upwaves
dat.uw <- dat.county

# merge jump limit data with county temperature data
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))

# process for counting upwaves
dat.uw$above.threshold <- ifelse(dat.uw$variable<dat.uw$variable.30yr.ll,1,0)
dat.uw <- ddply(dat.uw, .(month,leap,year,state.county.fips), summarize, up.waves=length(rle(above.threshold)$lengths[rle(above.threshold)$values==1 & rle(above.threshold)$lengths>=num.days]))

# merge and create weighted mean for state
dat.temp <-merge(dat.uw,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*up.waves))
temp.state <- na.omit(temp.state)
temp.state <- temp.state[complete.cases(temp.state),]

# adjust to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
temp.state$var.adj <- ifelse(temp.state$month %in% c(1,3,5,7,8,10,12), temp.state$var.adj,
ifelse(temp.state$month %in% c(4,6,9,11), temp.state$var.adj*(31/30),
ifelse((temp.state$month==2 & temp.state$leap==0), temp.state$var.adj*(31/28),
ifelse((temp.state$month==2 & temp.state$leap==1), temp.state$var.adj*(31/29),
'ERROR'
))))
temp.state$var.adj <- round(as.numeric(temp.state$var.adj),2)

# rename variable
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.jdwb.',num.days,'d.jo5')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 31. NUMBER OF JUMPDOWNWAVES (JUMP DOWN TO OVER 5 FOR 5 DAYS BELOW LONGRUN MEAN (1980-2009) NOT ASSUMING NORMALITY)
####################################################

num.days <- 5
jump = 5
var <- paste0('number_of_min_',num.days,'_day_below_+',jump,'_jumpdownwaves_2_',dname)

# load 90th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_95_nonnormals_mean_t2m_1980_2009.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# establish the 'jump' limits
dat.perc$variable.30yr.ul <- dat.perc$variable.30yr.mean + jump
dat.perc$variable.30yr.ll <- dat.perc$variable.30yr.mean - jump

# process for counting number of upwaves
dat.uw <- dat.county

# merge jump limit data with county temperature data
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))

# process for counting upwaves
dat.uw$above.threshold <- ifelse(dat.uw$variable<dat.uw$variable.30yr.ll,1,0)
dat.uw <- ddply(dat.uw, .(month,leap,year,state.county.fips), summarize, up.waves=length(rle(above.threshold)$lengths[rle(above.threshold)$values==1 & rle(above.threshold)$lengths>=num.days]))

# merge and create weighted mean for state
dat.temp <-merge(dat.uw,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*up.waves))
temp.state <- na.omit(temp.state)
temp.state <- temp.state[complete.cases(temp.state),]

# adjust to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
temp.state$var.adj <- ifelse(temp.state$month %in% c(1,3,5,7,8,10,12), temp.state$var.adj,
ifelse(temp.state$month %in% c(4,6,9,11), temp.state$var.adj*(31/30),
ifelse((temp.state$month==2 & temp.state$leap==0), temp.state$var.adj*(31/28),
ifelse((temp.state$month==2 & temp.state$leap==1), temp.state$var.adj*(31/29),
'ERROR'
))))
temp.state$var.adj <- round(as.numeric(temp.state$var.adj),2)

# rename variable
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.jdwb.',num.days,'d.jo5')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))


