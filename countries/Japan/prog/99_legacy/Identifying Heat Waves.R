### Heat waves

## Setting up workspace
rm(list=ls())

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
global_lon_lat.df <- global_lon_lat.df[-1]
state.points.codes.df = read.csv(file="state.points.codes.csv", header=TRUE)
state.points.codes.df <- state.points.codes.df[-1]

setwd("~/R/Temp data")



### Opening .csv and merging with state.points.codes.df

# Opening .csv
year.csv <- read.csv(file="worldwide_t2m_1983.csv", header = TRUE)
year.csv <- year.csv[-1]

# Merging to connect points with state info
usa.merge <- merge(state.points.codes.df, year.csv, by=c("long","lat"))

#Fixing column names
datelist <- names(usa.merge[7:ncol(usa.merge)])
datelist <- strsplit(datelist, "X")
datelist <- as.character(sapply(datelist, "[[", 2))
names(usa.merge)[7:ncol(usa.merge)] <- datelist



### Starting with just one point in IL in the year 1983 - it apparently had a lot of heat waves that year

IL.points <- usa.merge[which(usa.merge$STATE_ABBR=="IL"),]
IL.row <- IL.points[which(IL.points$point_id=="31563"),]  # Picked a point near Chicago



### Getting daily max temp values

# Creats new DF to bin daily maxes to using cbind
dailymax.point <- data.frame(IL.row[1,1:7])
names(dailymax.point) <- names(usa.merge)[1:7]

# Sequence that for loops will run through
x <- seq(from=8, to=ncol(IL.row), by=2) #gets every other column, and skips first date b/c there's only one reading for day 1

# For loop that adds daily MAX values using cbind() to dailymax.point DF

for (day in x) { # "day" = col num in WY.row
  
  # Max of a day based on the twice-daily measurements
  max <- max(c(IL.row[,day], IL.row[,day+1]))
  tempcol <- data.frame(max)
  names(tempcol) <- colnames(IL.row[day])
  
  # Bins new column to dailymax.point
  dailymax.point <- cbind(dailymax.point, tempcol)
}



### For loop to grab each day where the next day is more than 5 degrees hotter

# Sequence of days to cycle through
y <- seq(from=8, to=ncol(dailymax.point), by=1)

# Based on daily max
#for (day in y) {
  #"base" is the temperature value of the starting day
  base <- dailymax.point[,day]
  #"second" is the temperature value of the next day
  second <- dailymax.point[,day+1]
  # Get difference between days' temp values
  change <- second-base

  if (change>=5) {
    #print(base)
    third <- dailymax.point[,day+2]
    change2 <- third-base
     
    if (change2>=5) {
      print(c(colnames(dailymax.point)[base], "2 days above"))
    }
     
    else {
      print(c(colnames(dailymax.point)[base], "1 day above"))
    }
    #colnames(dailymax.point)[base]
  }
}
#


###

# Sequence of days to cycle through
y <- seq(from=7, to=ncol(dailymax.point), by=1)

## Come back to later:
#for (day in y) {
  #"base" is the temperature value of the starting day
  base <- dailymax.point[,day]
  
  n <- day+1
  
  repeat {
    nextday <- dailymax.point[1,n]
    change <- nextday-base

    
    if (change >= 5) {
      print(c(names(dailymax.point)[day],names(dailymax.point)[n]))
      n <- n+1
    }
    
    if (change < 5) {
      break
    }
  }
}


## Count number of days in a month which are more than 5 degrees different (+/-) from previous

# For loop that gets pairs of days that differ by 5+ degrees
dat.delta.5 <- data.frame(start=c(),end=c())
for (day in y) {
  #"base" is the temperature value of the starting day
  base <- dailymax.point[,day]
  
  n <- day+1
  
    nextday <- dailymax.point[1,n]
    change <- abs(nextday-base)
    
    if (change >= 5) {
      #print(c(names(dailymax.point)[day],names(dailymax.point)[n]))
      temp <- data.frame(start=names(dailymax.point)[day],end=names(dailymax.point)[n])
      dat.delta.5 <- rbind(dat.delta.5,temp)
    }
}

# Adding a month column

endmonth <- as.character(dat.delta.5[,2])
endmonth <- strsplit(endmonth, "[.]")
endmonth <- as.numeric(sapply(endmonth, "[[", 2))
endmonth <- as.data.frame(endmonth)
names(endmonth) <- "month"
dat.delta.5 <- cbind(dat.delta.5, endmonth)

# Counting days by month ('month' based on month of end date)

monthly.counts <- as.data.frame(table(dat.delta.5$month))
names(monthly.counts) <- c("month", "days where delta T > 5")

info.tmp <- data.frame(year=NA, state=NA, state_fips=NA)

# Should also add columns for year, state, and fips code when automating process

#monthly.counts <- merge(dat.delta.5, monthly.counts, by="month")



###########

#
for (day in seq(from=1, to=10, by=1)) {
  #"base" is the temperature value of the starting day
  base <- day
  
  n <- day+1
  
  repeat {
    
    change <- n-day
    print(n)
    n <- n+1
    
    if (change>3) {
      break
    }
  }
}




#
i <- 1

while (i < 6) {
  print(i)
  i = i+1
}

#
a <- 1

repeat {
  print(a)
  a = a+1
  if (a == 6){
    break
  }
}





# percentile work
function {
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
row.names(maxtemps) <- NULL  # Just removing row names

# Getting mean of the max temp values by month (mean max temp, grouped by month)
#m.max_means <- data.frame(m.max_mean=ddply(.data=maxtemps,.(month),summarize,mean.of.max.temps=mean(t.max)))

# Getting 90th percentile mark by month - quantile(x, number)
m.p.90 <- data.frame(ddply(.data=maxtemps,.(month),summarize,percentile.90=quantile(t.max, .9)))


## Merging

# Merging that with maxtemps
temp.df <- merge(maxtemps, m.p.90, by="month")

names(temp.df)
# t.max = daily max for that date
# ninety = 90th percentile of max. temps for that month (each month's "ninety" value differs)
# 90th percentile of summer maximum
# Note: can easily change which percentile, I just chose 90 to start with

# Subsetting the summer months
hot.months <- rbind(  temp.df[which(temp.df$month=="05"),],
                      temp.df[which(temp.df$month=="06"),],
                      temp.df[which(temp.df$month=="07"),],
                      temp.df[which(temp.df$month=="08"),],
                      temp.df[which(temp.df$month=="09"),])

for (day in hot.months$date) {
  rownum <- which(temp.df$date==day)
  
  if (temp.df[rownum,3] > temp.df[rownum,4]) {
  # [rownum,3] is the daily max temp and [rownum,4] is the 95th percentile for the month
    
    print(c(day, 1))
    
  }
}



# how to identify consecutive rows that meet a certain criteria?
} 