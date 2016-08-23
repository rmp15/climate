rm(list=ls())

library(ggplot2)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])

# 1. population-weighted mean temperatures

# load dataset with population weighted temperature values
dat <- readRDS(paste0('../../output/state_weighted_mean_summary/state_weighted_summary_',year,'.rds'))

# plot temperature against time, facetting by gender,sex,location
ggplot(data=dat,aes(x=month,y=temp.cel)) +
geom_line() +
facet_wrap() +
theme_bw()

# overlay some nationalised real data?


# 2. spread of county temperature for each month within state