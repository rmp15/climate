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

# year.start = 1979 ; year.end = 2016 ; year.start.2 = 1980 ; year.end.2 = 2016 ; dname = 't2m' ; metric = 'mean' ;

# length of analysis period
num.years <- year.end - year.start + 1

# load the data
# create directories for output
file.loc.input <- paste0('~/git/climate/countries/USA/output/metrics_climate_regions/',dname,'/',metric,'/')
dat <- readRDS(paste0(file.loc.input,'climate_region_values_',dname,'_',metric,'_',year.start.2,'_',year.end.2))

# rename climate variable to generic name
names(dat)[grep(dname,names(dat))] <- 'variable'

###############################################################
# DATA PROCESSING
###############################################################

# isolate to one age and sex
dat = subset(dat,age==65&sex==2)
dat$age=NULL ; dat$sex=NULL

# process mean, range, min, max, per climate region and per year
dat.summary = ddply(dat,.(climate_region,year),summarize,mean=mean(variable),min=min(variable),max=max(variable))
dat.summary$range = with(dat.summary,max-min)

# find single values for each climate region by summarising yearly values
dat.summary.summary = ddply(dat.summary,.(climate_region),summarize,mean=mean(mean),min=mean(min),max=mean(max))
dat.summary.summary$range = with(dat.summary.summary,max-min)

###############################################################
# DIRECTORY CREATION
###############################################################

# create directories for output
file.loc <- paste0('~/git/climate/countries/USA/output/seasonality_index_against_mean/',dname,'/',metric,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

###############################################################
# PLOT DATA
###############################################################

# fixed max/min
pdf(paste0(file.loc,'comparison_of_mean_against_range_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
ggplot() +
    # geom_point(data=dat.summary,aes(x=mean,y=range,color=climate_region)) +
    geom_point(data=dat.summary.summary,aes(x=mean,y=range,color=climate_region),size=7) +
    # geom_point(data=dat.summary.summary,aes(x=mean,y=range,color=climate_region),fill='black',size=5) +
    xlab('Mean temperature') + ylab('Temperature range') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()