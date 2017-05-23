rm(list=ls())

library(plyr)
library(ggplot2)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])

# create directory
dir = paste0("../../output/diff_up_down/",dname,"/")
ifelse(!dir.exists(dir), dir.create(dir, recursive=TRUE), FALSE)

dat.up <- readRDS('../../output/metrics_development/t2m/days_increasing_by_5_t2m/state_weighted_summary_days_increasing_by_5_t2m_1979_2015.rds')
dat.down <- readRDS('../../output/metrics_development/t2m/days_decreasing_by_5_t2m/state_weighted_summary_days_decreasing_by_5_t2m_1979_2015.rds')

# merge up and down files and limit to one age-sex group
dat.up.down = merge(dat.up,dat.down)
dat.up.down = subset(dat.up.down,age==85 & sex==1)

# find difference between number of up and down days
dat.up.down$diff = with(dat.up.down,t2m.dib.5-t2m.ddb.5)

# plot histogram
pdf(paste0(dir,'/up_down_hist.pdf'))
hist(dat.up.down$diff)
dev.off()

# scatter plot
pdf(paste0(dir,'/up_down_scatter.pdf'))
plot(dat.up.down$t2m.dib.5,dat.up.down$t2m.ddb.5)
dev.off()
