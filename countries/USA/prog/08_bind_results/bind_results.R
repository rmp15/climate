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

# declare years
years <- year.start:year.end

# empty data frame to add new data to
dat <- data.frame()

# loop to add each year's data
for (i in seq(length(years))) {
        file.name <- paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',years[i],'.rds')
        current.file <- readRDS(file.name)
        dat <- rbind(dat,current.file)
}

# save output
saveRDS(dat,paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',year.start,'_',year.end,'.rds'))

