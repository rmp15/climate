rm(list=ls())

library(ggplot2)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])

# create output directory
ifelse(!dir.exists("../../output/plots"), dir.create("../../output/plots"), FALSE)

# load state fips lookup code
fips.lookup <- read.csv('~/git/countries/USA/state/data/name_fips_lookup.csv')

# load dataset with population weighted temperature values
dat <- readRDS(paste0('../../output/state_weighted_mean_summary/state_weighted_summary_',year,'.rds'))
dat$state.fips <- as.numeric(dat$state.fips)
dat <- merge(dat,fips.lookup,by.x='state.fips',by.y='fips',all.x=1)

# load dataset with temperature values from each county
dat.county <- readRDS(paste0('../../output/state_weighted_mean_summary/county_summary_',year,'.rds'))
dat.county$state.fips <- as.numeric(substr(dat.county$state.county.fips,1,2))

# plot temperature against time, facetting by gender,sex,location
# alabama
pdf(paste0('../../output/plots/alabama_temperature_population_weighted_',year,'.pdf'))
ggplot() +
geom_point(data=subset(dat.county,state.fips==1),aes(x=month,y=temp.weighted-273.15)) +
geom_line(data=subset(dat,state.fips==1 & sex==1),aes(x=month,y=temp.cel)) +
geom_hline(yintercept=0, linetype=2,alpha=0.5) +
ggtitle(paste0('Alabama population-weighted temperature (each dot a county) ',year)) +
xlab('month') +
ylab('mean temperature') +
theme_bw()
dev.off()

# plot temperature against time, facetting by gender,sex,location
# doesn't work? Investigate
# florida
#pdf(paste0('../../output/plots/florida_temperature_population_weighted_',year,'.pdf'))
#ggplot() +
#geom_point(data=subset(dat.county,state.fips==12),aes(x=month,y=temp.weighted-273.15)) +
#geom_line(data=subset(dat,state.fips==12 & sex==1),aes(x=month,y=temp.cel)) +
#geom_hline(yintercept=0, linetype=2,alpha=0.5) +
#ggtitle(paste0('Floridapopulation-weighted temperature ',year)) +
#xlab('month') +
#ylab('mean temperature') +
#theme_bw()
#dev.off()