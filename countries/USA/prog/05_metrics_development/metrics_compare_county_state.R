rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
dname <- as.character(args[1])
year.start <- as.numeric(args[2])
year.end <- as.numeric(args[3])
metric.1 <- as.character(args[4])
metric.2 <- as.character(args[4])

# create directory
dir = paste0("../../output/metrics_compare_county_state/",dname,'/',metric.1,'/',metric.2)
ifelse(!dir.exists(dir), dir.create(dir, recursive=TRUE), FALSE)

# load file with climate data
dat.1 <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric.1,'_',dname,'/state_weighted_summary_',metric.1,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat.1$state.fips <- as.numeric(as.character(dat.1$state.fips))
dat.2 <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric.2,'_',dname,'/state_weighted_summary_',metric.2,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat.2$state.fips <- as.numeric(as.character(dat.2$state.fips))

dat.both = merge(dat.1,dat.2)
dat.both$diff = with(dat.both,t2m.meanc3-t2m.meanc4)
dat.both = subset(dat.both,!(state.fips%in%c(2,15)))

pdf(paste0(dir,'/comparison_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')

ggplot(data=subset(dat.both,!(state.fips%in%c(2,15))),aes(x=t2m.meanc3,y=t2m.meanc4)) + facet_wrap(~month) + geom_point() + geom_abline(slope=1)

ggplot(data=subset(dat.both,!(state.fips%in%c(2,15,32))),aes(x=t2m.meanc3,y=t2m.meanc4)) + facet_wrap(~month) + geom_point() + geom_abline(slope=1)

# load file with difference data
dat = readRDS(paste0('../../output/metrics_development/',dname,'/',metric.1,'_',dname,'/diff/',year.start,'_',year.end,'.rds'))

dat.summary = ddply(dat,.(month,sex,age,state.fips,state.county.fips),summarize,diff=mean(diff),pop.weighted=mean(pop.weighted))

pdf(paste0(dir,'/county_deviations',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')

# attach state names
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
dat.summary$fips = as.numeric(dat.summary$state.fips)
dat.summary = merge(dat.summary,state.lookup,by='fips',all.x=TRUE)

ggplot(data=subset(dat.summary,age==65&sex==2&!(state.fips%in%c('02','15')))) +
    geom_point(aes(x=state.fips,y=diff),alpha=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    facet_wrap(~month)

ggplot(data=subset(dat.summary,age==65&sex==2&!(state.fips%in%c('02','15')))) +
    geom_point(aes(x=code_name,y=diff,alpha=0.5,size=(pop.weighted))) +
    geom_hline(yintercept=0,linetype='dotted') +
    facet_wrap(~month) +
    ylim(c(-1,1)) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(size=5,angle=90),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

for(i in sort(unique(dat.summary$month))){
    print(
    ggplot(data=subset(dat.summary,month==i&age==65&sex==2&!(state.fips%in%c('02','15')))) +
    geom_point(aes(x=code_name,y=diff,alpha=0.5,size=(pop.weighted))) +
    geom_hline(yintercept=0,linetype='dotted') +
    facet_wrap(~month) +
    ylim(c(-1,1)) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(size=5,angle=90),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    )
}

dev.off()
