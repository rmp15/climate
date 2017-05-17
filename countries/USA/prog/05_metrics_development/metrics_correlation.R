# this script
# loads 2 particular metrics
# plots them against each other
# establishes r^2 values

rm(list=ls())

library(ggplot2)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric.1 <- as.character(args[4])
metric.2 <- as.character(args[5])

# create output directory
dir = paste0("../../output/metrics_correlation/",dname,"/",metric.1,"/")
ifelse(!dir.exists(dir), dir.create(dir,recursive=TRUE), FALSE)

# load climate metrics
source('../../data/objects/objects.R')

# load metric of interest
dat.1 <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric.1,'_',dname,'/state_weighted_summary_',metric.1,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat.2 <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric.2,'_',dname,'/state_weighted_summary_',metric.2,'_',dname,'_',year.start,'_',year.end,'.rds'))

# establish linear fit for R-squared value
dat.plot = data.frame(metric.1=dat.1[,ncol(dat.1)],metric.2=dat.2[,ncol(dat.2)])
lm = lm(metric.2~metric.1,data=dat.plot)
r.squared = round(summary(lm)$r.squared,2)

# plot metric 1 against metric 2
pdf(paste0(dir,dname,'_',metric.2,'_against_',metric.1,'_',year.start,'_',year.end,'.pdf'),paper='a4r',height=0,width=0)
ggplot() + geom_point(data=dat.plot,aes(x=metric.1,y=metric.2)) +
xlab(metric.1) + ylab(metric.2) +
ggtitle(paste0(metric.2,' against ',metric.1,' with R^2=',r.squared))
dev.off()

# LEGACY CODE

# load all other climate variables
#dat.metrics <- dat[,c(1:5)]
#for(i in metrics) {
#    dat.temp <- readRDS(paste0('../../output/metrics_development/',dname,'/',i,'_',dname,'/state_weighted_summary_',i,'_',dname,'_',year.start,'_',year.end,'.rds'))
#    dat.metrics <- merge(dat.metrics,dat.temp)
#}

# remove first 6 columns
#dat.metrics[,c(1:6)] <- NULL

# plot all against all
#for(i in c(1:ncol(dat.metrics))){
#    for j in c(1:ncol(dat.metrics))) {
#        plot(dat.metrics[,i],dat.metrics[,j])
#    }
#}
