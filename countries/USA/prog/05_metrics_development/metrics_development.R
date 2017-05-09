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
# 1. AVERAGE VALUE
####################################################
var <- paste0('mean_',dname)

# process for finding average temperature
dat.at <- dat.county
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.at <- ddply(dat.at,.(year,month,state.county.fips),summarize,var.weighted=round(mean(variable),1))

# merge and create weighted mean for state
dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
temp.state <- na.omit(temp.state)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.mean')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 1a. AVERAGE VALUE CENTRED BY LONGTERM NORMAL (1986-2005)
####################################################
var <- paste0('meanc_',dname)

# process for finding average temperature
dat.at <- dat.county
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.at <- ddply(dat.at,.(year,month,state.county.fips),summarize,var.weighted=round(mean(variable),1))

# merge and create weighted mean for state
dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
temp.state <- na.omit(temp.state)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.mean')

# load multiyear normal for 1986-2005
dat.multi <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/state_longterm_normals_mean_',dname,'_1986_2005.rds'))

# merge state-month mean values just calculated and subtract multiyear normal
temp.state <- merge(temp.state,dat.multi,by=c('month','state.fips','sex','age'))
names(temp.state)[grep(paste0(dname,'.mean') ,names(temp.state))] <- 'var.adj'
names(temp.state)[grep(paste0(dname,'.20yr.mean') ,names(temp.state))] <- '20yr.mean'
temp.state$var.adj <- with(temp.state,var.adj-`20yr.mean`)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.meanc')

# ONLY SAVE THE FIRST 6 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
temp.state <- temp.state[,c(1:6)]

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))


####################################################
# 1b. AVERAGE VALUE CENTRED BY LONGTERM NORMAL (entire period of study)
####################################################

var <- paste0('meanc2_',dname)

# process for finding average temperature
dat.at <- dat.county
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.at <- ddply(dat.at,.(year,month,state.county.fips),summarize,var.weighted=round(mean(variable),1))

# merge and create weighted mean for state
dat.temp <-merge(dat.at,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
temp.state <- na.omit(temp.state)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.mean')

# load multiyear normal for period of study
dat.multi <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/state_longterm_normals_mean_',dname,'_',year.start,'_',year.end,'.rds'))

# establish number of years of study
num.years <- length(year.start:year.end)

# merge state-month mean values just calculated and subtract multiyear normal
temp.state <- merge(temp.state,dat.multi,by=c('month','state.fips','sex','age'))
names(temp.state)[grep(paste0(dname,'.mean') ,names(temp.state))] <- 'var.adj'
names(temp.state)[grep(paste0(dname,'.',num.years,'yr.mean') ,names(temp.state))] <- paste0(num.years,'yr.mean')
temp.state$var.adj <- with(temp.state,var.adj-`32yr.mean`)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.meanc2')

# ONLY SAVE THE FIRST 6 COLUMNS!!!! I HAVE DONE THIS MANUALLY AND NEED TO FIX
temp.state <- temp.state[,c(1:6)]

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 2. NUMBER OF DAYS ABOVE A THRESHOLD
####################################################
threshold.upper <- 30
var <- paste0('days_above_',threshold.upper,'_',dname)

# process for counting days above threshold
# do i need to make days above threshold as a proportion of number of days in month? YES!
dat.th.up <- dat.county
names(dat.th.up)[grep(dname,names(dat.th.up))] <- 'variable'
dat.th.up$count <- ifelse(dat.th.up$variable> threshold.upper,1,0)
dat.th.up <- ddply(dat.th.up,.(year,leap,month,state.county.fips),summarize,days.above.threshold=sum(count))

# merge and create weighted mean for state
dat.temp <-merge(dat.th.up,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,days.above.threshold=sum(pop.weighted*days.above.threshold))
temp.state <- na.omit(temp.state)
temp.state$days.above.threshold <- as.numeric(temp.state$days.above.threshold)

# adjust to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
temp.state$days.above.threshold <- ifelse(temp.state$month %in% c(1,3,5,7,8,10,12), temp.state$days.above.threshold,
ifelse(temp.state$month %in% c(4,6,9,11), temp.state$days.above.threshold*(31/30),
ifelse((temp.state$month==2 & temp.state$leap==0), temp.state$days.above.threshold*(31/28),
ifelse((temp.state$month==2 & temp.state$leap==1), temp.state$days.above.threshold*(31/29),
'ERROR'
))))
temp.state$days.above.threshold <- as.numeric(temp.state$days.above.threshold)

# round (is this right?) NO!
#temp.state$days.above.threshold <- round(temp.state$days.above.threshold)

# rename variable
names(temp.state)[grep('days.above.threshold',names(temp.state))] <- paste0(dname,'.da.',threshold.upper)

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 3. NUMBER OF DAYS BELOW A THRESHOLD
####################################################
threshold.lower <- 10
var <- paste0('days_below_',threshold.lower,'_',dname)

# process for counting days below threshold
# do i need to make days above threshold as a proportion of number of days in month? YES!
dat.th.do <- dat.county
names(dat.th.do)[grep(dname,names(dat.th.do))] <- 'variable'
dat.th.do$count <- ifelse(dat.th.do$variable<threshold.lower,1,0)
dat.th.do <- ddply(dat.th.do,.(year,leap,month,state.county.fips),summarize,days.below.threshold=sum(count))

# merge and create weighted mean for state
dat.temp <-merge(dat.th.do,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,days.below.threshold=sum(pop.weighted*days.below.threshold))
temp.state <- na.omit(temp.state)
temp.state$days.below.threshold <- as.numeric(temp.state$days.below.threshold)

# adjust to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
temp.state$days.below.threshold <- ifelse(temp.state$month %in% c(1,3,5,7,8,10,12), temp.state$days.below.threshold,
ifelse(temp.state$month %in% c(4,6,9,11), temp.state$days.below.threshold*(31/30),
ifelse((temp.state$month==2 & temp.state$leap==0), temp.state$days.below.threshold*(31/28),
ifelse((temp.state$month==2 & temp.state$leap==1), temp.state$days.below.threshold*(31/29),
'ERROR'
))))
temp.state$days.below.threshold <- as.numeric(temp.state$days.below.threshold)

# round (is this right?) NO!
#temp.state$days.below.threshold <- round(temp.state$days.below.threshold)

# rename variable
names(temp.state)[grep('days.below.threshold',names(temp.state))] <- paste0(dname,'.db.',threshold.upper)

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 4. STANDARD DEVIATION
####################################################
var <- paste0('sd_',dname)

# process for finding sd of temperature
dat.sd <- dat.county
names(dat.sd)[grep(dname,names(dat.sd))] <- 'variable'
dat.sd <- ddply(dat.sd,.(year,month,state.county.fips),summarize,var.weighted=round(sd(variable),1))

# merge and create weighted mean for state
dat.temp <-merge(dat.sd,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*var.weighted))
temp.state <- na.omit(temp.state)
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.sd')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 5. NUMBER OF DAYS CHANGED BY A VALUE FROM DAY BEFORE
####################################################
threshold <- 5
var <- paste0('days_changing_by_',threshold,'_',dname)

# process for counting days changing by threshold
# do i need to make days above threshold as a proportion of number of days in month?
dat.ch <- dat.county
names(dat.ch)[grep(dname,names(dat.ch))] <- 'variable'
dat.ch <- ddply(dat.ch, .(month,leap,year,state.county.fips), transform, diff=c(0,diff(variable)))
dat.ch <- ddply(dat.ch,.(month,leap,year,state.county.fips),summarize,day.changed.by.threshold=sum(abs(diff)>threshold))

# merge and create weighted mean for state
dat.temp <-merge(dat.ch,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*day.changed.by.threshold))
temp.state <- na.omit(temp.state)

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
temp.state$var.adj <- as.numeric(temp.state$var.adj)

# round (is this right?) NO!
#temp.state$days.above.threshold <- round(temp.state$days.above.threshold)

# rename variable
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.dcb.',threshold)

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 6. NUMBER OF DAYS INCREASING BY A VALUE FROM DAY BEFORE
####################################################
threshold <- 5
var <- paste0('days_increasing_by_',threshold,'_',dname)

# process for counting days changing by threshold
# do i need to make days above threshold as a proportion of number of days in month?
dat.ch <- dat.county
names(dat.ch)[grep(dname,names(dat.ch))] <- 'variable'
dat.ch <- ddply(dat.ch, .(month,leap,year,state.county.fips), transform, diff=c(0,diff(variable)))
dat.ch <- ddply(dat.ch,.(month,leap,year,state.county.fips),summarize,day.increased.by.threshold=sum(diff>threshold))

# merge and create weighted mean for state
dat.temp <-merge(dat.ch,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*day.increased.by.threshold))
temp.state <- na.omit(temp.state)

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
temp.state$var.adj <- as.numeric(temp.state$var.adj)

# round (is this right?) NO!
#temp.state$days.above.threshold <- round(temp.state$days.above.threshold)

# rename variable
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.dib.',threshold)

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 7. NUMBER OF DAYS DECREASING BY A VALUE FROM DAY BEFORE
####################################################
threshold <- 5
var <- paste0('days_decreasing_by_',threshold,'_',dname)

# process for counting days changing by threshold
# do i need to make days above threshold as a proportion of number of days in month?
dat.ch <- dat.county
names(dat.ch)[grep(dname,names(dat.ch))] <- 'variable'
dat.ch <- ddply(dat.ch, .(month,leap,year,state.county.fips), transform, diff=c(0,diff(variable)))
dat.ch <- ddply(dat.ch,.(month,leap,year,state.county.fips),summarize,day.decreased.by.threshold=sum(diff<(-1*threshold)))

# merge and create weighted mean for state
dat.temp <-merge(dat.ch,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*day.decreased.by.threshold))
temp.state <- na.omit(temp.state)

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
temp.state$var.adj <- as.numeric(temp.state$var.adj)

# round (is this right?) NO!
#temp.state$days.above.threshold <- round(temp.state$days.above.threshold)

# rename variable
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.ddb.',threshold)

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 8. NUMBER OF UPWAVES 1 (ABSOLUTE THRESHOLD 99th PERCENTILE)
####################################################
num.days <- 3
var <- paste0('number_of_min_',num.days,'_day_above_99_upwaves_',dname)

# load 99th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_normals_mean_t2m_1986_2005.rds'))

# process for counting number of upwaves
dat.uw <- dat.county

# merge 99th percentile data with county temperature data
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))

# process for counting upwaves
colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))
dat.uw$above.threshold <- ifelse(dat.uw$variable>dat.uw$variable.20yr.ul,1,0)
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
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.uwo.',num.days,'d')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 9. NUMBER OF DOWNWAVES 1 (ABSOLUTE THRESHOLD 99th PERCENTILE)
####################################################
num.days <- 3
var <- paste0('number_of_min_',num.days,'_day_below_99_downwaves_',dname)

# load 99th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_normals_mean_t2m_1986_2005.rds'))

# process for counting number of downwaves
dat.dw <- dat.county

# merge 99th percentile data with county temperature data
dat.dw <- merge(dat.dw,dat.perc,by=c('month','state.county.fips'))

# process for counting downwaves
colnames(dat.dw) = gsub(dname, "variable", colnames(dat.dw))
dat.dw$below.threshold <- ifelse(dat.dw$variable<dat.dw$variable.20yr.ll,1,0)
dat.dw <- ddply(dat.dw, .(month,leap,year,state.county.fips), summarize, down.waves=length(rle(below.threshold)$lengths[rle(below.threshold)$values==1 & rle(below.threshold)$lengths>=num.days]))

# merge and create weighted mean for state
dat.temp <-merge(dat.dw,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*down.waves))
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
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.dwu.',num.days,'d')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 10. NUMBER OF UPWAVES 2 (ABSOLUTE THRESHOLD 95th PERCENTILE)
####################################################
num.days <- 3
var <- paste0('number_of_min_',num.days,'_day_above_95_upwaves_',dname)

# load 99th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_normals_mean_t2m_1986_2005.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# get 95 percentiles from assuming normal dist
dat.perc$variable.20yr.ul <- dat.perc$variable.20yr.mean + 1.96 * dat.perc$variable.20yr.sd
dat.perc$variable.20yr.ll <- dat.perc$variable.20yr.mean - 1.96 * dat.perc$variable.20yr.sd

# process for counting number of upwaves
dat.uw <- dat.county

# merge 99th percentile data with county temperature data
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))

# process for counting upwaves
dat.uw$above.threshold <- ifelse(dat.uw$variable>dat.uw$variable.20yr.ul,1,0)
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
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.uwo.',num.days,'d')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 11. NUMBER OF DOWNWAVES 2 (ABSOLUTE THRESHOLD 95th PERCENTILE)
####################################################
num.days <- 3
var <- paste0('number_of_min_',num.days,'_day_below_95_downwaves_',dname)

# load 99th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_normals_mean_t2m_1986_2005.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# get 95 percentiles from assuming normal dist
dat.perc$variable.20yr.ul <- dat.perc$variable.20yr.mean + 1.96 * dat.perc$variable.20yr.sd
dat.perc$variable.20yr.ll <- dat.perc$variable.20yr.mean - 1.96 * dat.perc$variable.20yr.sd

# process for counting number of downwaves
dat.dw <- dat.county

# merge 99th percentile data with county temperature data
dat.dw <- merge(dat.dw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.dw) = gsub(dname, "variable", colnames(dat.dw))

# process for counting downwaves
dat.dw$below.threshold <- ifelse(dat.dw$variable<dat.dw$variable.20yr.ll,1,0)
dat.dw <- ddply(dat.dw, .(month,leap,year,state.county.fips), summarize, down.waves=length(rle(below.threshold)$lengths[rle(below.threshold)$values==1 & rle(below.threshold)$lengths>=num.days]))

# merge and create weighted mean for state
dat.temp <-merge(dat.dw,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*down.waves))
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
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.dwu.',num.days,'d')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 12. NUMBER OF UPWAVES 3 (ABSOLUTE THRESHOLD 90th PERCENTILE)
####################################################
num.days <- 3
var <- paste0('number_of_min_',num.days,'_day_above_90_upwaves_',dname)

# load 99th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_normals_mean_t2m_1986_2005.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# get 95 percentiles from assuming normal dist
dat.perc$variable.20yr.ul <- dat.perc$variable.20yr.mean + 1.65 * dat.perc$variable.20yr.sd
dat.perc$variable.20yr.ll <- dat.perc$variable.20yr.mean - 1.65 * dat.perc$variable.20yr.sd

# process for counting number of upwaves
dat.uw <- dat.county

# merge 99th percentile data with county temperature data
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))

# process for counting upwaves
dat.uw$above.threshold <- ifelse(dat.uw$variable>dat.uw$variable.20yr.ul,1,0)
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
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.uwo.',num.days,'d')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 13. NUMBER OF DOWNWAVES 3 (ABSOLUTE THRESHOLD 90th PERCENTILE)
####################################################
num.days <- 3
var <- paste0('number_of_min_',num.days,'_day_below_90_downwaves_',dname)

# load 99th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_normals_mean_t2m_1986_2005.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# get 95 percentiles from assuming normal dist
dat.perc$variable.20yr.ul <- dat.perc$variable.20yr.mean + 1.65 * dat.perc$variable.20yr.sd
dat.perc$variable.20yr.ll <- dat.perc$variable.20yr.mean - 1.65 * dat.perc$variable.20yr.sd

# process for counting number of downwaves
dat.dw <- dat.county

# merge 99th percentile data with county temperature data
dat.dw <- merge(dat.dw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.dw) = gsub(dname, "variable", colnames(dat.dw))

# process for counting downwaves
dat.dw$below.threshold <- ifelse(dat.dw$variable<dat.dw$variable.20yr.ll,1,0)
dat.dw <- ddply(dat.dw, .(month,leap,year,state.county.fips), summarize, down.waves=length(rle(below.threshold)$lengths[rle(below.threshold)$values==1 & rle(below.threshold)$lengths>=num.days]))

# merge and create weighted mean for state
dat.temp <-merge(dat.dw,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*down.waves))
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
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.dwu.',num.days,'d')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 14. NUMBER OF DAYS ABOVE ABSOLUTE 90th PERCENTILE THRESHOLD
####################################################
var <- paste0('number_of_days_above_90_',dname)

# load 99th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_normals_mean_t2m_1986_2005.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# get 95 percentiles from assuming normal dist
dat.perc$variable.20yr.ul <- dat.perc$variable.20yr.mean + 1.65 * dat.perc$variable.20yr.sd
dat.perc$variable.20yr.ll <- dat.perc$variable.20yr.mean - 1.65 * dat.perc$variable.20yr.sd

# process for counting number of days above 90th percentile
dat.uw <- dat.county

# merge 90th percentile data with county temperature data
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))

# process for counting upwaves
dat.uw$above.threshold <- ifelse(dat.uw$variable>dat.uw$variable.20yr.ul,1,0)
dat.uw <- ddply(dat.uw, .(month,leap,year,state.county.fips), summarize, days.above.threshold=sum(above.threshold))

# merge and create weighted mean for state
dat.temp <-merge(dat.uw,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*days.above.threshold))
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
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.da.90')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 15. NUMBER OF DAYS BELOW 90th PERCENTILE THRESHOLD
####################################################
var <- paste0('number_of_days_below_90_',dname)

# load 99th percentile data for state
dat.perc <- readRDS(paste0('../../output/multiyear_normals/',dname,'/mean/county_longterm_normals_mean_t2m_1986_2005.rds'))

colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# get 95 percentiles from assuming normal dist
dat.perc$variable.20yr.ul <- dat.perc$variable.20yr.mean + 1.65 * dat.perc$variable.20yr.sd
dat.perc$variable.20yr.ll <- dat.perc$variable.20yr.mean - 1.65 * dat.perc$variable.20yr.sd

# process for counting number of days above 90th percentile
dat.uw <- dat.county

# merge 90th percentile data with county temperature data
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))

colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))

# process for counting upwaves
dat.uw$above.threshold <- ifelse(dat.uw$variable<dat.uw$variable.20yr.ll,1,0)
dat.uw <- ddply(dat.uw, .(month,leap,year,state.county.fips), summarize, days.above.threshold=sum(above.threshold))

# merge and create weighted mean for state
dat.temp <-merge(dat.uw,state.weighting.filter,by=c('year','month','state.county.fips'))
temp.state <- ddply(dat.temp,.(year,leap,month,state.fips,sex,age),summarize,var.adj=sum(pop.weighted*days.above.threshold))
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
names(temp.state)[grep('var.adj',names(temp.state))] <- paste0(dname,'.db.90')

# save output
ifelse(!dir.exists(paste0("../../output/metrics_development/",dname,'/',var)), dir.create(paste0("../../output/metrics_development/",dname,'/',var)), FALSE)
saveRDS(temp.state,paste0('../../output/metrics_development/',dname,'/',var,'/state_weighted_summary_',var,'_',year.selected,'.rds'))

####################################################
# 16. NUMBER OF UPWAVES (JUMP)
####################################################

# THIS IS UPWAVES BY USING A JUMP (SAY UP BY 5 FROM DAY BEFORE)
# AND THEN HOW LONG IT SUSTAINS IT

####################################################
# 17. NUMBER OF DOWNWAVES (JUMP)
####################################################

# THIS IS DOWNWAVES BY USING A JUMP (SAY DOWN BY 5 FROM DAY BEFORE)
# AND THEN HOW LONG IT SUSTAINS IT


