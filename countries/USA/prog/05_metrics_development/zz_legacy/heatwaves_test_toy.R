rm(list=ls())

library(plyr)

year.start = 1986
year.end = 2005
dname = 't2m'
metric = 'mean'

#sdfdsgsdalkdnfdfksanflkdsnflkn

cat('processing for years',year.start,year.end)

# normal years
years <- year.start:year.end

# create grid with states and years (only 30-day months)
state.county.fipss = c('01005','01007','01009')
years = c(1980:2013)
months = c(1:12)
days = c(1:30)

dat = expand.grid(year=years,month=months,day=days,state.county.fips=state.county.fipss)

# generate random climate variables
set.seed(13234)
dat$t2m = rnorm(n=nrow(dat),sd=1) + runif(n=nrow(dat),min=15,max=20)

# create grid of state weighting filter (assuming static population proportions here over the time period)
sexes = c(1)
ages = c(75)

state.weighting = expand.grid(sex=sexes,age=ages,state.county.fips=state.county.fipss,state='01')

# create made-up state population weightings (i.e. fraction of state which lives in particular county)
state.weighting$pop.weighted = c(0.3,0.5,0.2)

# 90 PERCENTILE (10% ABOVE OR 10% BELOW)

# process for finding average and 90th percentile upper and lower limits of a 'standard' period, say 1986-2005 (as per Lancet Countdown)
dat.at <- subset(dat, year %in% years)
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
#pdf
hist(dat.uw$above.threshold,nclass=2)
#dev.off()

stats.uw <- count(dat.summary,'up.waves')
stats.uw$percentage <- with(stats.uw, round(100*freq/nrow(dat.summary),1))

# histogram of heatwaves
#pdf
hist(dat.summary$up.waves,nclass=5)
#dev.off()

# WEIGHTED MEAN OF HEATWAVES FOR STATE

# merge and create weighted mean for state
dat.temp <- merge(dat.summary,state.weighting,by=c('state.county.fips'))
dat.temp <- dat.temp[order(dat.temp$year,dat.temp$year,dat.temp$month,dat.temp$state.county.fips),]
rownames(dat.temp) = 1:nrow(dat.temp)
temp.state <- ddply(dat.temp,.(year,month,state,sex,age),summarize,up.waves=sum(pop.weighted*up.waves))

# statistics on upwaves for state
temp.state$up.waves.rounded <- round(temp.state$up.waves)
stats.state <- count(temp.state,'up.waves.rounded')

# histogram of heatwaves
#pdf
hist(temp.state$up.waves,nclass=10)
#dev.off()
