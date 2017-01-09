rm(list=ls())

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
year.start.2 <- as.numeric(args[3])
year.end.2 <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])

# length of analysis period
num.years <- year.end - year.start + 1

# load the data
# create directories for output
file.loc.input <- paste0('../../output/metrics_climate_regions/',dname,'/',metric,'/')
dat <- readRDS(paste0(file.loc.input,'climate_region_values_',dname,'_',metric,'_',year.start.2,'_',year.end.2))

# rename climate variable to generic name
names(dat)[grep(dname,names(dat))] <- 'variable'

# fix temperature to absolute scale
if(dname=='t2m'){
    dat$variable <- dat$variable + 273.15
}

###############################################################
# DATA PROCESSING
###############################################################

# 1. CLIMATE REGION

# DYNAMIC MAX MIN

# figure out the ratio of max/min deaths over time by sex, age, year
dat.max.min <-  ddply(dat, .(sex,age,year,climate_region), summarize, max=max(variable),month.max=month[variable==max(variable)],min=min(variable),month.min=month[variable==min(variable)])
dat.max.min$ratio <- with(dat.max.min,abs(max-min)/abs(min))
dat.max.min$percent.change <- round(100*(dat.max.min$ratio),1)

# add time value that starts at 0
dat.max.min$year.centre <- with(dat.max.min,year-year.start)

# apply linear regression to each group by sex, age, month to find gradient
lin.reg.grad.region <- ddply(dat.max.min, .(sex,age,climate_region), function(z)coef(lm(percent.change ~ year.centre, data=z)))
lin.reg.grad.region$start.value <- lin.reg.grad.region$`(Intercept)`
lin.reg.grad.region$end.value <- with(lin.reg.grad.region,`(Intercept)`+year.centre*(num.years-1))

# obtain significance of slopes
lin.reg.sig.region <- ddply(dat.max.min, .(sex,age,climate_region), function(z)coef(summary(lm(percent.change ~ year.centre, data=z))))
lin.reg.sig.region <- lin.reg.sig.region[!c(TRUE,FALSE),]
lin.reg.sig.region$sig.test.10 <- ifelse(lin.reg.sig.region[,6]<0.10,1,0)
lin.reg.sig.region$sig.test.5 <- ifelse(lin.reg.sig.region[,6]<0.05,1,0)

# merge with data about gradients
lin.reg.grad <- merge(lin.reg.grad.region,lin.reg.sig.region,by=c('sex','age','climate_region'))

###############################################################
# DIRECTORY CREATION
###############################################################

# create directories for output
file.loc <- paste0('../../output/seasonality_index_climate_region/',dname,'/',metric,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

###############################################################
# EXPORT DATA
###############################################################

saveRDS(lin.reg.grad,paste0(file.loc,'seasonality_index_',dname,'_',metric,'_',year.start.2,'_',year.end.2))
