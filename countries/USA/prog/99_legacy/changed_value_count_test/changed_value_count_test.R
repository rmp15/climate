rm(list=ls())

library(plyr)

# create dataset
dat <- data.frame(day=c(1:30),temp=round(rnorm(30,20,4),1))

# find difference between days' temperatures
dat$diff <- c(0,diff(dat$temp))

# figure out how many values are more than a stated value, lower than a stated value, or change by a stated value
threshold <- 5
num.over.threshold <- length(dat$diff[dat$diff>threshold])
num.under.threshold <- length(dat$diff[dat$diff < (-1*threshold)])
num.from.threshold <- length(dat$diff[abs(dat$diff)>threshold])

# perform the same as above with categories
dat <- data.frame(month=c(rep(1,30),rep(2,30)),day=rep(c(1:30),2),temp=round(rnorm(60,20,4),1))

# find difference between days' temperatures by group
dat <- ddply(dat, .(month), transform, diff=c(0,diff(temp)))
dat.count <- ddply(dat,.(month),summarize,over=sum(diff>threshold),under=sum(diff<(-1*threshold)),from=sum(abs(diff)>threshold))