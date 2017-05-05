rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric <- as.character(args[4])

# create directory
ifelse(!dir.exists(paste0("../../output/testing_normality/",dname,"/",metric)), dir.create(paste0("../../output/testing_normality/",dname,"/",metric), recursive=TRUE), FALSE)

# load file with climate data
dat <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat$state.fips <- as.numeric(as.character(dat$state.fips))

# load state names
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')

# merge state names
dat <- merge(dat,state.lookup,by.x='state.fips',by.y='fips')

names(dat)[grep(dname,names(dat))] <- 'variable'

# qqplot to test for normality
pdf(paste0("../../output/testing_normality/",dname,"/",metric,"/","qqplot_",dname,"_",metric,"_",year.start,"_",year.end,".pdf"),height=0,width=0,paper='a4r')

    ggplot(subset(dat,sex==2 & age==85), aes(variable))+ geom_histogram() + xlab(paste0(dname,'_',metric))

    ggplot(subset(dat,sex==2 & age==85), aes(variable))+ geom_histogram() + facet_wrap(~full_name) + xlab(paste0(dname,'_',metric))

    ggplot(subset(dat,sex==2 & age==85), aes(sample=variable))+stat_qq() +
#geom_abline(slope=1,intercept=0) +
    ggtitle('Q-Q plot') +
    xlab(paste0(dname,'_',metric)) +
    scale_colour_discrete(guide=FALSE) +
    facet_wrap(~age)

    ggplot(subset(dat,sex==2 & age==85), aes(sample=variable,color=full_name))+stat_qq() +
#geom_abline(slope=1,intercept=0) +
    ggtitle('Q-Q plot') +
    xlab(paste0(dname,'_',metric)) +
    scale_colour_discrete(guide=FALSE)

dev.off()

# histogram to test for normality by state and month
pdf(paste0("../../output/testing_normality/",dname,"/",metric,"/","histogram_by_month_state_",dname,"_",metric,"_",year.start,"_",year.end,".pdf"),height=0,width=0,paper='a4r')

for(i in unique(dat$state.fips)){
   print(ggplot(subset(dat,sex==2 & age==85 & state.fips==i), aes(variable))+ geom_histogram() + facet_wrap(~month) + xlab(paste0(dname,'_',metric)) + ggtitle(state.lookup[state.lookup$fips==i,1]))
}

dev.off()

# qqplot to test for normality by state and month
pdf(paste0("../../output/testing_normality/",dname,"/",metric,"/","qqplot_by_month_state_",dname,"_",metric,"_",year.start,"_",year.end,".pdf"),height=0,width=0,paper='a4r')

for(i in c(1:12)){
    print(ggplot(subset(dat,sex==2 & age==85 & month==i), aes(sample=variable,color=full_name))+stat_qq() +
    #geom_abline(slope=1,intercept=0) +
    ggtitle('Q-Q plot') +
    xlab(paste0(dname,'_',metric)) +
    scale_colour_discrete(guide=FALSE) + ggtitle(i))

}

dev.off()






