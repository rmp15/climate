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
fips.lookup <- read.csv('~/git/mortality/Japan/pref/data/pref/pref_lookup.csv')

# load dataset with population weighted temperature values
dat <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year,'.rds'))
dat$state_id <- as.numeric(dat$state_id)
dat <- merge(dat,fips.lookup,by.x='state_id',by.y='state_id',all.x=1)
names(dat)[grep(dname,names(dat))] <- 'variable'

# plot
pdf(paste0('../../output/plots/',dname,'/',metric,'/',dname,'_',metric,'_',year,'.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(dat),aes(x=month,y=variable)) +
geom_line() +
geom_hline(yintercept=0, linetype=2,alpha=0.5) +
xlab('month') +
ylab(paste0(dname,'.',metric)) +
facet_wrap(~pref) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = guide_legend(title = 'age group')) +
ggtitle(paste0('Male: ',dname,'.',metric,' ',year)) +
scale_x_discrete(labels=c(1:12)) +
theme_bw()
dev.off()

