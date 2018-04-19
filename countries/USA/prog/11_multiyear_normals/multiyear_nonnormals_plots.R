rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric <- as.character(args[4])

cat('processing for years',year.start,year.end)

directory = paste0("../../output/multiyear_normals_plots/",dname,"/",metric)
ifelse(!dir.exists(directory), dir.create(directory, recursive=TRUE), FALSE)

# replace name of var
var <- paste0('mean_',dname)

# load data
dat = readRDS(paste0("../../output/multiyear_normals/",dname,"/",metric,'/state_longterm_95_nonnormals_',var,'_',year.start,'_',year.end,'.rds'))

# isolate one age-sex group and remove alaska and hawaii
dat = subset(dat,age==65&sex==1)
dat = subset(dat, !(state.fips%in%c('02','15')))

library(plyr)

# short names for months
month.short = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
dat$month.short = mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short)
dat$month.short <- reorder(dat$month.short,dat$month)

# attach state names
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
dat$fips = as.numeric(dat$state.fips)
dat = merge(dat,state.lookup,by='fips',all.x=TRUE)

# generalise names of columns
names(dat)[6]='variable'
names(dat)[7]='variable.min'
names(dat)[8]='variable.max'

library(ggplot2)

# plot by average values by month
pdf(paste0(directory,'/longterm_nonnormals.pdf'),height=0,width=0,paper='a4r')
ggplot(data=dat) +
    geom_point(aes(x=code_name,y=variable)) +
    geom_hline(yintercept=0,linetype='dotted') +
    geom_errorbar(aes(x=code_name,ymin=variable.min,ymax=variable.max)) +
    ylab(toupper(metric)) + xlab('State') +
    facet_wrap(~month.short) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(size=5,angle=90),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=dat) +
    geom_point(aes(x=month.short,y=variable)) +
    geom_hline(yintercept=0,linetype='dotted') +
    geom_errorbar(aes(x=month.short,ymin=variable.min,ymax=variable.max)) +
    ylab(toupper(metric)) + xlab('State') +
    facet_wrap(~full_name) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()