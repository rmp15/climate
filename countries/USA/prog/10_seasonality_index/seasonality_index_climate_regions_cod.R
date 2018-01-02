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
cod <- as.character(args[7])

# year.start = 1979 ; year.end = 2015 ; year.start.2 = 1980 ; year.end.2 = 2013 ; dname = 't2m' ; metric = 'mean' ;
#  cod = 'Cancer'

# length of analysis period
num.years <- year.end - year.start + 1

# load the data
# create directories for output
file.loc.input <- paste0('~/git/climate/countries/USA/output/metrics_climate_regions/',dname,'/',metric,'/')
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

# STATIC MAX MIN DEFINED BY COM

# load com data to establish max min locations
file.loc.nat.input <- paste0("~/git/mortality/USA/state/output/com/",year.start.2,'_',year.end.2,"/national/values/combined_results/")
dat.COM <- readRDS(paste0(file.loc.nat.input,'com_inv_com_rates_national_values_method_2_entire_',cod,'_',year.start.2,'_',year.end.2))

# round to get month required for merging
dat.COM$COM.mean <- round(dat.COM$COM.mean)
dat.COM$COM.mean <- ifelse(dat.COM$COM.mean==0,12,dat.COM$COM.mean)
dat.COM$month <- dat.COM$COM.mean
levels(dat.COM$sex) <- c(1,2)

# figure out the ratio of max/min temperature over time with fixed max/min by sex, age, year
dat.max.min.fixed <- merge(dat,dat.COM,by=c('age','sex','month'))
dat.max.min.fixed <- ddply(dat.max.min.fixed,.(sex,age,year,climate_region), summarize,max=variable[type=='max'],month.max=month[type=='max'],min=variable[type=='min'],month.min=month[type=='min'])
dat.max.min.fixed$ratio <- with(dat.max.min.fixed,abs(max-min)/abs(min))
dat.max.min.fixed$percent.change <- round(100*(dat.max.min.fixed$ratio),1)
dat.max.min.fixed$diff <- with(dat.max.min.fixed,max-min)

# add time value that starts at 0
dat.max.min.fixed$year.centre <- with(dat.max.min.fixed,year-year.start)

# apply linear regression to each group by sex, age, month to find gradient
#lin.reg.grad <- ddply(dat.max.min.fixed, .(sex,age,climate_region), function(z)coef(lm(percent.change ~ year.centre, data=z)))
lin.reg.grad <- ddply(dat.max.min.fixed, .(sex,age,climate_region), function(z)coef(lm(diff ~ year.centre, data=z)))
lin.reg.grad$end.value <- with(lin.reg.grad,`(Intercept)`+year.centre*(num.years-1))
lin.reg.grad$start.value <- lin.reg.grad$`(Intercept)`

# obtain significance of slopes
#lin.reg.sig <- ddply(dat.max.min.fixed, .(sex,age,climate_region), function(z)coef(summary(lm(percent.change ~ year.centre, data=z))))
lin.reg.sig <- ddply(dat.max.min.fixed, .(sex,age,climate_region), function(z)coef(summary(lm(diff ~ year.centre, data=z))))
lin.reg.sig <- lin.reg.sig[!c(TRUE,FALSE),]
lin.reg.sig$sig.test.10 <- ifelse(lin.reg.sig[,7]<0.10,1,0)
lin.reg.sig$sig.test.5 <- ifelse(lin.reg.sig[,7]<0.05,1,0)

# merge with data about gradients
lin.reg.grad <- merge(lin.reg.grad,lin.reg.sig,by=c('sex','age','climate_region'))
#
# # DYNAMIC MAX MIN
#
# # figure out the ratio of max/min deaths over time by sex, age, year
# dat.max.min <-  ddply(dat, .(sex,age,year,climate_region), summarize, max=max(variable),month.max=month[variable==max(variable)],min=min(variable),month.min=month[variable==min(variable)])
# dat.max.min$ratio <- with(dat.max.min,abs(max-min)/abs(min))
# dat.max.min$percent.change <- round(100*(dat.max.min$ratio),1)
# dat.max.min$diff <- with(dat.max.min,max-min)
#
# # add time value that starts at 0
# dat.max.min$year.centre <- with(dat.max.min,year-year.start)
#
# # apply linear regression to each group by sex, age, month to find gradient
# #lin.reg.grad.region <- ddply(dat.max.min, .(sex,age,climate_region), function(z)coef(lm(percent.change ~ year.centre, data=z)))
# lin.reg.grad.region <- ddply(dat.max.min, .(sex,age,climate_region), function(z)coef(lm(diff ~ year.centre, data=z)))
# lin.reg.grad.region$start.value <- lin.reg.grad.region$`(Intercept)`
# lin.reg.grad.region$end.value <- with(lin.reg.grad.region,`(Intercept)`+year.centre*(num.years-1))
#
# # obtain significance of slopes
# #lin.reg.sig.region <- ddply(dat.max.min, .(sex,age,climate_region), function(z)coef(summary(lm(percent.change ~ year.centre, data=z))))
# lin.reg.sig.region <- ddply(dat.max.min, .(sex,age,climate_region), function(z)coef(summary(lm(diff ~ year.centre, data=z))))
# lin.reg.sig.region <- lin.reg.sig.region[!c(TRUE,FALSE),]
# lin.reg.sig.region$sig.test.10 <- ifelse(lin.reg.sig.region[,7]<0.10,1,0)
# lin.reg.sig.region$sig.test.5 <- ifelse(lin.reg.sig.region[,7]<0.05,1,0)
#
# # merge with data about gradients
# lin.reg.grad.region <- merge(lin.reg.grad.region,lin.reg.sig.region,by=c('sex','age','climate_region'))

###############################################################
# DIRECTORY CREATION
###############################################################

# create directories for output
file.loc <- paste0('~/git/climate/countries/USA/output/seasonality_index_climate_region/',dname,'/',metric,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

###############################################################
# EXPORT DATA
###############################################################

# fixed max/min
saveRDS(lin.reg.grad,paste0(file.loc,'seasonality_index_com_fixed_',dname,'_',metric,'_',cod,'_',year.start.2,'_',year.end.2))

# dynamic max/min
#saveRDS(lin.reg.grad.region,paste0(file.loc,'seasonality_index_',dname,'_',metric,'_',year.start.2,'_',year.end.2))




