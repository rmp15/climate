### Heat waves

## Setting up workspace

library("rgdal")
library("rgeos")
library("maptools")
library("ggplot2")
library("plyr")
library("dplyr")
library("maps")
library("RColorBrewer")
library("lattice")
library("ncdf4")

setwd("C:/Users/lmitchis/Documents/R/usa_shapefiles")
global_lon_lat.df = read.csv("global_lon_lat.csv", header=TRUE)
state.points.codes.df = read.csv(file="state.points.codes.csv", header=TRUE)

setwd("~/R/Temp data")

# DF is comes from NetCDF extraction script - have to run that first, until I automate process all toether
head(worldwide_t2m_1983[,1:8])
usa.merge.1983 <- merge(state.points.codes.df, worldwide_t2m_1983, by=c("long","lat"))
head(usa.merge.1983[1:4,1:9])



### Starting with just one point in IL in the year 1983 - it apparently had a lot of heat waves that year

IL.points <- usa.merge.1983[which(usa.merge.1983$STATE_ABBR=="IL"),]
IL.row <- IL.points[which(IL.points$point_id=="31563"),]  # Picked a point near Chicago


## Getting daily max temp values

# This takes out the info at the beginning of the row to put back in later (long, lat, fips, ect.)
dailymax.point <- data.frame(IL.row[1,1:8])
names(dailymax.point) <- names(IL.row[1:8])

# Sequence that for loops will run through
end <- ncol(IL.row)
x <- seq(from=9, to=end, by=2)
# "by=2" so it gets every other column, and skips first date b/c there's only one reading for day 1

# For loop that adds daily MAX values using cbind() to dailymax.point DF
for (day in x) { # "day" = col num in WY.row
  max <- max(c(IL.row[,day], IL.row[,day+1]))
  tempcol <- data.frame(max)
  names(tempcol) <- colnames(IL.row[day])
  dailymax.point <- cbind(dailymax.point, tempcol)
}

## For loop to grab each day where the next day is more than 5 degrees hotter

# Sequence of days to cycle through
end <- ncol(dailymax.point)
y <- seq(from=8, to=end, by=1)

# Based on daily max
for (day in y) {
  #"base" is the temperature value of the starting day
  base <- dailymax.point[,day]
  #"second" is the temperature value of the next day
  second <- dailymax.point[,day+1]
  # Get difference between days' temp values
  change <- second-base
  
  if (change>5) {
    #print(base)
    print(colnames(dailymax.point)[base])
  }
  #colnames(dailymax.point)[base]
}



# while loop?



### Using percentiles to define heat waves (trying out another method)

## Rearranging data so it's all in rows because I find it easier to apply functions to that way

# Making table with the dates and temperature values

maxtemps <- data.frame(matrix(nrow=365, ncol=2))
names(maxtemps) <- c("date", "t.max")
maxtemps$date <- names(dailymax.point[8:ncol(dailymax.point)])
maxtemps$t.max <- t(IL.row[,8:ncol(dailymax.point)])



# Want to be able to compare daily max relative to that month (as opposed to data from the the whole year)
# Creating a vector of months as done for the monthly stats (see "TempData_Automating-combined.R")
datelist <- names(dailymax.point[8:ncol(dailymax.point)])
#datelist[1:5]
datelist <- strsplit(datelist, "-")
month <- data.frame( month=as.character(sapply(datelist, "[[", 2)) )

# Adding column of months
maxtemps <- data.frame(maxtemps$date, month, maxtemps$t.max)
names(maxtemps) <- c("date", "month", "t.max")
row.names(maxtemps) <- NULL

# Getting mean of the max temp values by month (mean max temp, grouped by month)
#m.max_means <- data.frame(m.max_mean=ddply(.data=maxtemps,.(month),summarize,mean.of.max.temps=mean(t.max)))

# Getting 90th percentile mark by month - quantile(x, number)
m.qninety <- data.frame(ddply(.data=maxtemps,.(month),summarize,ninety=quantile(t.max, .9)))


## Merging

# Merging that with maxtemps
temp.df <- merge(maxtemps, m.qninety, by="month")

names(temp.df)
# t.max = daily max for that date
# ninety = 90th percentile of max. temps for that month (each month's "ninety" value differs)
# 90th percentile of summer maximum
# Note: can easily change which percentile, I just chose 90 to start with




for (day in temp.df$date) {
  rownum <- which(temp.df$date==day)
  
  if (temp.df[rownum,3] > temp.df[rownum,4]) {
  
  print(day)
    
  }
}

# how to identify consecutive rows that meet a certain criteria?




