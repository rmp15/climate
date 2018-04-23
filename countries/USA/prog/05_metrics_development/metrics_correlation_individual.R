# this script
# loads 2 particular metrics
# plots them against each other
# establishes r^2 values

# go to correct directory
setwd('~/git/climate/countries/USA/prog/00_bash')

rm(list=ls())

library(ggplot2)
library(plyr)
library(RColorBrewer)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname.1 <- as.character(args[3])
dname.2 <- as.character(args[4])
metric.1 <- as.character(args[5])
metric.2 <- as.character(args[6])

year.start = 1979 ; year.end = 2015 ;
dname.1 = 't2m' ; dname.2 = 't2m'
metric.1 = 'mean'
metric.2 = 'sd'

# create output directory
dir = paste0("../../output/metrics_correlation/",dname.1,"/",metric.1,"/")
ifelse(!dir.exists(dir), dir.create(dir,recursive=TRUE), FALSE)

# load climate metrics
source('../../data/objects/objects.R')

# load metric of interest
dat.1 <- readRDS(paste0('../../output/metrics_development/',dname.1,'/',metric.1,'_',dname.1,'/state_weighted_summary_',metric.1,'_',dname.1,'_',year.start,'_',year.end,'.rds'))
dat.2 <- readRDS(paste0('../../output/metrics_development/',dname.2,'/',metric.2,'_',dname.2,'/state_weighted_summary_',metric.2,'_',dname.2,'_',year.start,'_',year.end,'.rds'))

# subset for only one age-sex group
dat.1 <- subset(dat.1,age==85&sex==2) ; dat.1 = subset(dat.1,!(state.fips%in%c('02','15')))
dat.2 <- subset(dat.2,age==85&sex==2) ; dat.2 = subset(dat.2,!(state.fips%in%c('02','15')))

# sort columns
dat.1 = with(dat.1,dat.1[order(sex,age,state.fips,month),])
dat.2 = with(dat.2,dat.2[order(sex,age,state.fips,month),])

# establish linear fit for R-squared value
dat.plot = data.frame(month=dat.1$month,state.fips=dat.1$state.fips,metric.1=dat.1[,ncol(dat.1)],metric.2=dat.2[,ncol(dat.2)])
lm = lm(metric.2~metric.1,data=dat.plot)
r.squared = round(summary(lm)$r.squared,2)

# plot metric 1 against metric 2 for all states and all months
pdf(paste0(dir,dname.1,'_',metric.2,'_against_',dname.2,'_',metric.2,'_',year.start,'_',year.end,'.pdf'),paper='a4r',height=0,width=0)
print(ggplot() + geom_point(data=dat.plot,aes(x=metric.1,y=metric.2)) +
geom_abline(a=1,b=0,lintype='dotted') +
xlab(paste0(dname.1,'_',metric.1)) + ylab(paste0(dname.2,'_',metric.2)) +
ggtitle(paste0(metric.2,' against ',metric.1,' with R^2=',r.squared)))
dev.off()

# load state fips lookup
state.lookup = read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')

# plot metric 1 against metric 2 for an individual state
pdf(paste0(dir,dname.1,'_',metric.2,'_against_',dname.2,'_',metric.2,'_against_',metric.1,'_by_state_',year.start,'_',year.end,'.pdf'), paper='a4r',height=0,width=0)

for (i in (unique(dat.plot$state.fips))) {

state.name = state.lookup[state.lookup$fips==as.numeric(i),][1,1]

print(ggplot(data=subset(dat.plot,state.fips==i),aes(x=metric.1,y=metric.2)) + geom_point() +
xlab(paste0(dname.1,'_',metric.1)) + ylab(paste0(dname.2,'_',metric.2)) +
geom_abline(a=1,b=0,lintype='dotted') +
stat_smooth(method='loess') +
ggtitle(paste0(state.name,' ',metric.2,' against ',metric.1)))

}

dev.off()

# plot metric 1 against metric 2 for an individual state and facetted by month
pdf(paste0(dir,dname.1,'_',metric.2,'_against_',dname.2,'_',metric.2,'_against_',metric.1,'_facet_by_state_',year.start,'_',year.end,'.pdf'), paper='a4r',height=0,width=0)

print(ggplot(data=subset(dat.plot),aes(x=metric.1,y=metric.2)) + geom_point() +
xlab(paste0(dname.1,'_',metric.1)) + ylab(paste0(dname.2,'_',metric.2)) +
geom_abline(a=1,b=0,lintype='dotted') +
stat_smooth(method='loess') +
ggtitle(paste0(state.name,' ',metric.2,' against ',metric.1)) +
facet_wrap(~state.fips))

dev.off()


# plot metric 1 against metric 2 for an individual state and facetted by month
pdf(paste0(dir,dname.1,'_',metric.2,'_against_',dname.2,'_',metric.2,'_against_',metric.1,'_by_state_and_month_',year.start,'_',year.end,'.pdf'), paper='a4r',height=0,width=0)
for (i in (unique(dat.plot$state.fips))) {

state.name = state.lookup[state.lookup$fips==as.numeric(i),][1,1]

print(ggplot(data=subset(dat.plot,state.fips==i),aes(x=metric.1,y=metric.2)) + geom_point() +
xlab(paste0(dname.1,'_',metric.1)) + ylab(paste0(dname.2,'_',metric.2)) +
geom_abline(a=1,b=0,lintype='dotted') +
stat_smooth(method='loess') +
ggtitle(paste0(state.name,' ',metric.2,' against ',metric.1)) +
facet_wrap(~month))

}
dev.off()