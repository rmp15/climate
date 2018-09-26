rm(list=ls())

library(ggplot2)
library(RColorBrewer)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric <- as.character(args[4])

# create output directory
ifelse(!dir.exists("../../output/plots_against_time"), dir.create("../../output/plots_against_time"), FALSE)
ifelse(!dir.exists(paste0("../../output/plots_against_time/",dname)), dir.create(paste0("../../output/plots_against_time/",dname)), FALSE)
ifelse(!dir.exists(paste0("../../output/plots_against_time/",dname,'/',metric)), dir.create(paste0("../../output/plots_against_time/",dname,'/',metric)), FALSE)

# load state fips lookup code
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')

# load dataset with population weighted temperature values
dat <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat$state.fips <- as.numeric(dat$state.fips)
dat <- merge(dat,fips.lookup,by.x='state.fips',by.y='fips',all.x=1)
names(dat)[grep(dname,names(dat))] <- 'variable'

# get rid of Hawaii and Alaska
dat = subset(dat,!(state.fips%in%c(2,15)))

# plot facetting by state and colouring by age
age.filter <- unique(dat$age)
colourCount <- length(age.filter)

# isolate a single age-sex combination
dat = subset(dat,age==65&sex==1)

# 1. over time by month and state
pdf(paste0('../../output/plots_against_time/',dname,'/',metric,'/',dname,'_',metric,'_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
for (i in c(1:12)){
    print(ggplot(data=subset(dat,sex==1&month==i),aes(x=year,y=variable)) +
    geom_point(aes(color=as.factor(month))) +
    geom_smooth(method='lm') +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    xlab('Year') +
    ylab(paste0(dname,'.',metric)) +
    facet_wrap(~full_name) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = guide_legend(title = 'Month')) +
    ggtitle(month.short)) +
    scale_x_discrete(labels=c(1:12)) +
    theme_bw())
}
dev.off()