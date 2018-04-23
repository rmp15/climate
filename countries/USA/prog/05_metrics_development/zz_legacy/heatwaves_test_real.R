rm(list=ls())

library(plyr)

year.start = 1979
year.end = 2015
year.normal.start = 1986
year.normal.end = 2005
dname = 't2m'
metric = 'mean'

# create directory
dir = paste0("../../output/heatwaves_test/",dname)
ifelse(!dir.exists(dir), dir.create(dir,recursive=TRUE), FALSE)

# normal years
years <- year.start:year.end

# loop to load years which are involved
dat <- data.frame()
for(i in years) {
    
    # load county summary by day (for single year)
    dat.county <- readRDS(paste0('../../output/county_weighted_mean_summary/county_daily/',dname,'/county_daily_',dname,'_',i,'.rds'))
    
    # fix temperature to celsius
    if(dname=='t2m'){
        dat.county$t2m <- dat.county$t2m - 273.15
    }

    dat <- rbind(dat,dat.county)
    
}

# only take one state
dat$state.fips = substr(dat$state.county.fips,1,2)
state = '06'
dat = subset(dat,state.fips==state)

# load weightings by county for state summary based on population and only take one age
state.weighting <- readRDS('~/data/climate/population_weightings/state_population_weightings.rds')
state.weighting.filter = subset(state.weighting,state.fips==state&sex==2&age==85)

# 90 PERCENTILE (10% ABOVE OR 10% BELOW)

# define normal years
normal.years = c(year.normal.start:year.normal.end)

# process for finding average and 90th percentile upper and lower limits of a 'standard' period, say 1986-2005 (as per Lancet Countdown)
dat.at <- subset(dat, year %in% normal.years)
names(dat.at)[grep(dname,names(dat.at))] <- 'variable'
dat.mean <- ddply(dat.at,.(month,state.county.fips),summarize,var.weighted=round(mean(variable),1))
dat.perc <- ddply(dat.at, .(month,state.county.fips),function(x) round(quantile(x$variable,c(0.1,0.9)),1))
dat.at <- merge(dat.mean,dat.perc,by=c('month','state.county.fips'))

# reorder by month
dat.at <- dat.at[order(dat.at$month,dat.at$state.county.fips),]

# rename variables
names(dat.at)[grep('var.weighted',names(dat.at))] <- paste0(dname,'.mean')
names(dat.at)[grep('90',names(dat.at))] <- paste0(dname,'.ul')
names(dat.at)[grep('10',names(dat.at))] <- paste0(dname,'.ll')

####################################################
# 14. NUMBER OF UPWAVES 4 (ABSOLUTE THRESHOLD 90th PERCENTILE NOT ASSUMING NORMALITY)
####################################################
num.days <- 3
var <- paste0('number_of_min_',num.days,'_day_above_nonnormal_90_upwaves_',dname)

# load 90th percentile data for counties
dat.perc <- dat.at

# rename to generalise process
colnames(dat.perc) = gsub(dname, "variable", colnames(dat.perc))

# merge 90th percentile data with county climate data
dat.uw <- dat
#dat.uw$state.county.fips.num = as.numeric(as.character(dat.uw$state.county.fips))
#dat.perc$state.county.fips.num = as.numeric(as.character(dat.perc$state.county.fips))
dat.uw <- merge(dat.uw,dat.perc,by=c('month','state.county.fips'))
colnames(dat.uw) = gsub(dname, "variable", colnames(dat.uw))

# PROCESS FOR COUNTING UPWAVES BY COUNTY

# is daily temperature above long-term 90% percentile?
dat.uw$above.threshold <- ifelse(dat.uw$variable>dat.uw$variable.ul,1,0)

# do we have days in a row above 90% percentile? If so, there is an upwave
dat.summary <- ddply(dat.uw, .(month,year,state.county.fips), summarize, up.waves=length(rle(above.threshold)$lengths[rle(above.threshold)$values==1 & rle(above.threshold)$lengths>=num.days]))

# statistics on days over threshold and upwaves for counties
stats.dot <- count(dat.uw,'above.threshold')
stats.dot$percentage <- with(stats.dot, round(100*freq/nrow(dat.uw),1))

# histogram of over or under threshold
pdf(paste0(dir,'/days_over_threshold.pdf'))
hist(dat.uw$above.threshold,nclass=2)
dev.off()

stats.uw <- count(dat.summary,'up.waves')
stats.uw$percentage <- with(stats.uw, round(100*freq/nrow(dat.summary),1))

# histogram of heatwaves
pdf(paste0(dir,'/heatwaves_counties.pdf'))
hist(dat.summary$up.waves,nclass=10)
dev.off()

# WEIGHTED MEAN OF HEATWAVES FOR STATE

# merge and create weighted mean for state
dat.temp <- merge(dat.summary,state.weighting.filter,by=c('month','year','state.county.fips'))
dat.temp <- dat.temp[order(dat.temp$year,dat.temp$month,dat.temp$state.county.fips),]
rownames(dat.temp) = 1:nrow(dat.temp)
temp.state <- ddply(dat.temp,.(year,month,state.fips,sex,age),summarize,up.waves=sum(pop.weighted*up.waves))

# statistics on upwaves for state
temp.state$up.waves.rounded <- round(temp.state$up.waves)
stats.state <- count(temp.state,'up.waves.rounded')
stats.state$percentage <- with(stats.state, round(100*freq/nrow(temp.state),1))

# histogram of heatwaves
pdf(paste0(dir,'/heatwaves_state.pdf'))
hist(temp.state$up.waves,nclass=10)
dev.off()
