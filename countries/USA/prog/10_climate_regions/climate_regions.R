rm(list=ls())

# LOGICAL STEPS OF CODE:
# LOAD ERA-INTERIM-DERIVED VALUES (DONE)
# LOAD CLIMATE REGION INFO (DONE) AND MERGE WITH TABLE OF VALUES
# LOAD POPULATION TABLES FOR STATES AND MERGE WITH TABLE OF VALUES
# CREATE POPULATION-WEIGHTED AVERAGES OF CLIMATE REGIONS BY YEAR
# CREATE AVERAGE VALUES OF CLIMATE REGIONS BY MONTH ACROSS ENTIRE TIME PERIOD

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

# declare years
years <- year.start.2:year.end.2

# load climate data
dat.climate <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
names(dat.climate)[names(dat.climate) == 'state.fips'] <- 'fips'
dat.climate$fips <- as.integer(as.character(dat.climate$fips))

# load region data
dat.region <- readRDS(paste0('~/git/mortality/USA/state/output/mapping_posterior/INLA/type1a/1982_2013/maps/USA_state_data'))
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
dat.merged <- merge(dat.merged,dat.pop)

# create directories for output
file.loc <- paste0('../../output/climate_regions/',dname,'/',metric,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# TEMP graph
pdf(paste0(file.loc,'seasonality_index_mf_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(dat.merged,sex==1 & fips==12)) +
geom_point(aes(x=month,y=t2m.mean,color=as.factor(year))) +
facet_wrap(~fips)
dev.off()

