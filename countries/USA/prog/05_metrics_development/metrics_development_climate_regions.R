rm(list=ls())

library(ggplot2)
library(plyr)
library(dplyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
year.start.2 <- as.numeric(args[3])
year.end.2 <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])

# year.start = 1979 ; year.end = 2015 ; year.start.2 = 1980 ; year.end.2 = 2013 ; dname = 't2m' ; metric = 'mean'

print(args)

# declare years
years <- year.start.2:year.end.2

# load climate data
dat.climate <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
names(dat.climate)[names(dat.climate) == 'state.fips'] <- 'fips'
dat.climate$fips <- as.integer(as.character(dat.climate$fips))

# load region data
dat.region <- readRDS(paste0('~/git/mortality/USA/state/output/mapping_posterior/INLA/type1a/1982_2013/maps/USA_state_data'))

# fix climate region names
dat.region$climate_region <- 	c('Northwest','West North Central','Northeast','West North Central','West North Central',
'West North Central','East North Central','Northwest','Northeast','East North Central',
'Northwest','Northeast','East North Central','Northeast','West North Central',
'Northeast','Northeast','Northeast','Northeast','Northeast',
'Central','West','Southwest','West','Central',
'Central','Northeast','Northeast','Central','Northeast',
'Southwest','Central','South','Southeast','Central',
'Southwest','South','Southeast','Central','South',
'Southwest','Southeast','South','Southeast','Southeast',
'South','South','Southeast','East North Central','Northwest',
'West')

names(dat.region)[names(dat.region) == 'STATE_FIPS'] <- 'fips'
dat.region <- dat.region[,c('fips','climate_region')]
dat.region$fips <- as.integer(as.character(dat.region$fips))
dat.region <- dat.region[order(dat.region$fips),]

# load population data
dat.pop <- readRDS('~/git/mortality/USA/state/output/pop_us_infer/statePopulations_infer_by_days_new_years')
dat.pop <- dat.pop[,c('year','month','sex','age','fips','pop.adj')]
dat.pop <- na.omit(dat.pop)
dat.pop$fips <- as.integer(as.character(dat.pop$fips))

# merge data
dat.merged <- merge(dat.climate,dat.region)
dat.merged <- merge(dat.merged,dat.pop, all.x=0)

# isolate years of interest
dat.merged <- subset(dat.merged, year %in% years)

# rename climate variable to generic name
names(dat.merged)[grep(dname,names(dat.merged))] <- 'variable'

# create weighted mean for each climate region by month across time period
dat.wm <- ddply(dat.merged,.(year,month,sex,age,climate_region),function(x) data.frame(variable=weighted.mean(x$variable,x$pop.adj)))

# create weighted mean for each climate region by month across entire period
dat.wm.entire <- ddply(dat.wm,.(month,sex,age,climate_region),function(x) data.frame(variable=mean(x$variable)))

# DIRECTORY CREATION

# create directories for output
file.loc <- paste0('../../output/metrics_climate_regions/',dname,'/',metric,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# PLOTTING

# graph of state values across period
pdf(paste0(file.loc,'state_values_male_',dname,'_',metric,'_',year.start.2,'_',year.end.2,'.pdf'),height=0,width=0,paper='a4r')
for(i in sort(unique(dat.merged$fips))){
    print(ggplot(data=subset(dat.merged,sex==1 & fips==i)) +
    geom_point(aes(x=month,y=variable,color=as.factor(year))) +
    ylab(paste0(dname,'.',metric)) +
    facet_wrap(~fips))
}
dev.off()

# graph of climate region values across period
pdf(paste0(file.loc,'climate_values_',dname,'_',metric,'_',year.start.2,'_',year.end.2,'.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(dat.wm,sex==1)) +
geom_point(aes(x=month,y=variable,color=as.factor(year))) +
ylab(paste0(dname,'.',metric)) +
facet_wrap(~climate_region)
dev.off()

# graph of climate region values averaged across entire period
pdf(paste0(file.loc,'climate_values_average_',dname,'_',metric,'_',year.start.2,'_',year.end.2,'.pdf'),height=0,width=0,paper='a4r')
ggplot(subset(dat.wm.entire,sex==1)) +
geom_line(aes(x=month,y=variable,color=climate_region)) +
ylab(paste0(dname,'.',metric)) +
facet_wrap(~age)
dev.off()

# rename generic climate name back to original name
names(dat.merged)[grep('variable',names(dat.merged))] <- paste0(dname,'.',metric)
names(dat.wm)[grep('variable',names(dat.wm))] <- paste0(dname,'.',metric)
names(dat.wm.entire)[grep('variable',names(dat.wm.entire))] <- paste0(dname,'.',metric)

# output the weighted mean over time
saveRDS(dat.wm,paste0(file.loc,'climate_region_values_',dname,'_',metric,'_',year.start.2,'_',year.end.2))


