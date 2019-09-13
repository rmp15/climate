rm(list=ls())

library(ggplot2)
library(RColorBrewer)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])
year.selected <- year
dname <- as.character(args[2])
metric <- as.character(args[3])

# create output directory
ifelse(!dir.exists("../../output/plots"), dir.create("../../output/plots"), FALSE)
ifelse(!dir.exists(paste0("../../output/plots/",dname)), dir.create(paste0("../../output/plots/",dname)), FALSE)
ifelse(!dir.exists(paste0("../../output/plots/",dname,'/',metric)), dir.create(paste0("../../output/plots/",dname,'/',metric)), FALSE)

# load state fips lookup code
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')

# load dataset with population weighted temperature values
dat <- readRDS(paste0('../../output/metrics_development_era5/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year,'.rds'))
dat$state.fips <- as.numeric(dat$state.fips)
dat <- merge(dat,fips.lookup,by.x='state.fips',by.y='fips',all.x=1)
names(dat)[grep(dname,names(dat))] <- 'variable'


# plot facetting by state and colouring by age
age.filter <- unique(dat$age)
colourCount <- length(age.filter)

# 1. male
pdf(paste0('../../output/plots_era5/',dname,'/',metric,'/',dname,'_',metric,'_male_',year,'.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(dat,sex==1),aes(x=month,y=variable)) +
geom_line(aes(color=as.factor(age))) +
geom_hline(yintercept=0, linetype=2,alpha=0.5) +
xlab('month') +
ylab(paste0(dname,'.',metric)) +
facet_wrap(~full_name) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = guide_legend(title = 'age group')) +
ggtitle(paste0('Male: ',dname,'.',metric,' ',year)) +
scale_x_discrete(labels=c(1:12)) +
theme_bw()
dev.off()

# 2. female
pdf(paste0('../../output/plots_era5/',dname,'/',metric,'/',dname,'_',metric,'_female_',year,'.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(dat,sex==2),aes(x=month,y=variable)) +
geom_line(aes(color=as.factor(age))) +
geom_hline(yintercept=0, linetype=2,alpha=0.5) +
xlab('month') +
ylab(paste0(dname,'.',metric)) +
facet_wrap(~full_name) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = guide_legend(title = 'age group')) +
ggtitle(paste0('Female: ',dname,'.',metric,' ',year)) +
scale_x_discrete(labels=c(1:12)) +
theme_bw()
dev.off()
