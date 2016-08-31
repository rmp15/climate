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

# load state fips lookup code
fips.lookup <- read.csv('~/git/countries/USA/state/data/name_fips_lookup.csv')

# load dataset with population weighted temperature values
dat <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year,'.rds'))
dat$state.fips <- as.numeric(dat$state.fips)
dat <- merge(dat,fips.lookup,by.x='state.fips',by.y='fips',all.x=1)
names(dat)[grep(dname,names(dat))] <- 'variable'


# plot facetting by state and colouring by age
age.filter <- unique(dat$age)
colourCount <- length(age.filter)

# 1. male
pdf(paste0('../../output/plots/',dname,'/',dname,'_',metric,'_male_',year,'.pdf'),height=0,width=0,paper='a4r')
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
pdf(paste0('../../output/plots/',dname,'/',dname,'_',metric,'_female_',year,'.pdf'),height=0,width=0,paper='a4r')
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

# legacy

# load dataset with temperature values from each county
#dat.county <- readRDS(paste0('../../output/state_weighted_mean_summary/county_summary_',year,'.rds'))
#dat.county$state.fips <- as.numeric(substr(dat.county$state.county.fips,1,2))

# load PRISM dataset (at the moment by county)
#county <- '01101'
#dat.prism <- read.csv(paste0('~/git/climate/countries/USA/data/prism/prism_',county,'.csv'))
#dat.prism$year <- as.numeric(substr(dat.prism$Date,1,4))
#dat.prism$month <- as.numeric(substr(dat.prism$Date,6,7))
#dat.prism <- dat.prism[,c(3,4,2)]
#names(dat.prism) <- c('year','month','temp')

# CREATE A FUNCTION
# plot temperature against time, facetting by gender,sex,location
# alabama
#pdf(paste0('../../output/plots/alabama_temperature_population_weighted_',year,'.pdf'))
#ggplot() +
#geom_point(data=subset(dat.county,state.fips==1),aes(x=month,y=temp.weighted-273.15)) +
#geom_point(data=subset(dat.county,state.county.fips==county),color='red',aes(x=month,y=temp.weighted-273.15)) +
#geom_line(data=subset(dat,state.fips==1 & sex==1),color='blue',aes(x=month,y=temp.cel)) +
#geom_line(data=subset(dat.prism,year==year.selected),aes(x=month,y=temp,color='red')) +
#geom_hline(yintercept=0, linetype=2,alpha=0.5) +
#ggtitle(paste0('Alabama population-weighted temperature (each dot a county).\nBlue is weighted mean ',year)) +
#xlab('month') +
#ylab('mean temperature') +
#theme_bw()
#dev.off()

# plot temperature against time, facetting by gender,sex,location
#ggplot() +
#geom_point(data=subset(dat,sex==1),aes(x=month,y=temp.cel,color=age)) +
#geom_hline(yintercept=0, linetype=2,alpha=0.5) +
#facet_wrap(~state.fips) +
#theme_bw()

# dat.12 <- subset(dat.county,state.fips==12 & year==1982)
# ggplot() + geom_line(data=dat.12,aes(x=month,y=temp.weighted,color=state.county.fips))

