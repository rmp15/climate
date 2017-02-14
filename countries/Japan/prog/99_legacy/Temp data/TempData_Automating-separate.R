rm(list=ls())

### Automating whole process

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
state.points.codes.df <- state.points.codes.df[-1]

setwd("~/R/Temp data")

## List of files to run through

statelist <- c("DC", "MD", "NJ", "NH", "RI")

yearlydata.nc <- c(
  "worldwide_t2m_1982.csv",
  "worldwide_t2m_1983.csv",
  "worldwide_t2m_1984.csv",
  "worldwide_t2m_1985.csv",
  "worldwide_t2m_1986.csv",
  "worldwide_t2m_1987.csv",
  "worldwide_t2m_1988.csv",
  "worldwide_t2m_1989.csv",
  "worldwide_t2m_1990.csv",
  "worldwide_t2m_1991.csv",
  "worldwide_t2m_1992.csv",
  "worldwide_t2m_1993.csv",
  "worldwide_t2m_1994.csv",
  "worldwide_t2m_1995.csv",
  "worldwide_t2m_1996.csv",
  "worldwide_t2m_2002.csv",
  "worldwide_t2m_2003.csv",
  "worldwide_t2m_2004.csv",
  "worldwide_t2m_2005.csv",
  "worldwide_t2m_2006.csv",
  "worldwide_t2m_2007.csv",
  "worldwide_t2m_2008.csv",
  "worldwide_t2m_2009.csv",
  "worldwide_t2m_2010.csv"
)

#(yearlydata.csv isn't a csv file, it's just a list of the csv file names)

#yearlydata.csv <- c(
"worldwide_t2m_1982.csv",
"worldwide_t2m_1983.csv",
"worldwide_t2m_1984.csv",
"worldwide_t2m_1985.csv",
"worldwide_t2m_1986.csv")

#yearlydata.csv <- c(
"worldwide_t2m_1987.csv",
"worldwide_t2m_1988.csv",
"worldwide_t2m_1989.csv",
"worldwide_t2m_1990.csv",
"worldwide_t2m_1991.csv")

#yearlydata.csv <- c(
"worldwide_t2m_1992.csv",
"worldwide_t2m_1993.csv",
"worldwide_t2m_1994.csv",
"worldwide_t2m_1995.csv",
"worldwide_t2m_1996.csv")

#yearlydata.csv <- c(
"worldwide_t2m_1997.csv",
"worldwide_t2m_1998.csv",
"worldwide_t2m_1999.csv",
"worldwide_t2m_2000.csv",
"worldwide_t2m_2001.csv")

#yearlydata.csv <- c(
"worldwide_t2m_2002.csv",
"worldwide_t2m_2003.csv",
"worldwide_t2m_2004.csv",
"worldwide_t2m_2005.csv",
"worldwide_t2m_2006.csv")

#yearlydata.csv <- c(
"worldwide_t2m_2007.csv",
"worldwide_t2m_2008.csv",
"worldwide_t2m_2009.csv",
"worldwide_t2m_2010.csv")

## For loop that outputs .csv files

yearlydata.nc <- c("worldwide_t2m_2010.nc")

for (nc in yearlydata.nc) {
  
  # Initial naming
  filename.nc <- nc
  ncname <- filename.nc
  dname <- 't2m'
  
  # Gets year of file from file name
  year <- unlist(strsplit(filename.nc, "_"))
  year <- year[3]
  year <- unlist(strsplit(year, ".nc"))
  
  # open NetCDF file
  ncin <- nc_open(ncname)
  
  # get long and lat data
  lon <- ncvar_get(ncin, 'longitude')
  nlon <- dim(lon)
  lat <- ncvar_get(ncin, "latitude", verbose = F)
  nlat <- dim(lat)
  
  # get time variable and convert to days
  t <- ncvar_get(ncin, "time")
  t.hours <- t / 24
  t.sec <- t * 3600
  tunits <- ncatt_get(ncin, "time", "units")
  nt <- dim(t)
  
  # extract 2-metre temperature
  tmp.array <- ncvar_get(ncin, dname)
  dlname <- ncatt_get(ncin, dname, "long_name")
  dunits <- ncatt_get(ncin, dname, "units")
  fillvalue <- ncatt_get(ncin, dname, "_FillValue")
  
  # global attributes
  title <- ncatt_get(ncin, 0, "title")
  institution <- ncatt_get(ncin, 0, "institution")
  datasource <- ncatt_get(ncin, 0, "source")
  references <- ncatt_get(ncin, 0, "references")
  history <- ncatt_get(ncin, 0, "history")
  Conventions <- ncatt_get(ncin, 0, "Conventions")
  
  # close NetCDF file
  nc_close(ncin)
  
  # split the time units string into fields
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth = as.integer(unlist(tdstr)[2])
  tday = as.integer(unlist(tdstr)[3])
  tyear = as.integer(unlist(tdstr)[1])
  t.names <- as.POSIXct(t.sec, origin = "1900-01-01")
  
  # stamp as character names
  timeStamp <-  strptime(t.names,"%Y-%m-%d %H:%M:%S")
  
  # round to days
  t.names <- as.character(round(timeStamp, 'days'))
  
  # convert tmp.array into long file
  tmp.vec.long <- as.vector(tmp.array)
  #length(tmp.vec.long)
  
  # reshape file into matrix
  tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
  #dim(tmp.mat)
  
  # create second data frame
  lonlat <- expand.grid(lon, lat)
  tmp.df02 <- data.frame(cbind(lonlat, tmp.mat)) ## important line
  names(tmp.df02) <- c('long','lat',t.names)
  head(colnames(tmp.df02))
  
  # Fixing longitude values so range is -180 to 180, not 0 to 360
  tmp.df02$long <- global_lon_lat.df$long
  tmp.df02$lat <- global_lon_lat.df$lat
  
  # Clipping off last row because it starts the next year
  end <- ncol(tmp.df02)
  tmp.df02[end] <- NULL
  
  # write to csv file with naming according to year
  csv.name <- paste(c("worldwide_t2m_", year, ".csv"), collapse="")
  write.csv(tmp.df02, file=csv.name)
}


### Processing the .csv files output by previous for loop

## Setting up emty DF to bin monthly means (organized by year, state, and FIPS)

#Do for first run-through
m.stats.all.yr <- data.frame(matrix(ncol=7))
names(m.stats.all.yr) <- c("year", "state", "fips", "month", "m.mean", "m.max", "m.min")
m.stats.all.yr <- na.omit(m.stats.all.yr)

#Do for all other run throughs
m.stats.all.yr <- read.csv("m.stats.t2m.VT.1982-2006.csv", header=TRUE) #STATE
m.stats.all.yr <- m.stats.all.yr[-1]


## Make sure to change all lines that have "#STATE" accordingly

for (csv in yearlydata.csv) {
  
## Opens .csv and prepares it to be processed
  
  # Reads csv file by year (takes ~4min)
  year.csv <- read.csv(csv)
  
  # Removes first col, which is unnecessary
  year.csv <- year.csv[,-1]


## Monthly stats
  
  # Gets year of file from file name
  year <- unlist(strsplit(csv, "_"))
  year <- year[3]
  year <- unlist(strsplit(year, ".csv"))
  
  # Merging temp data (year.csv) with corresponding state info
  usa.merge <- merge(state.points.codes.df, year.csv, by=c("long","lat"))
  yrend <- ncol(usa.merge)
  # usa.merge[10:20,1:9] #Note: Points from a given state are not necessarily in continuous rows
  
  # Extracts just one state's points
  st.points <- usa.merge[which(usa.merge$STATE_ABBR=='VT'),]  #STATE
  state.abb <- "VT"                                           #STATE
  fips <- st.points$state_fips[1]                             #STATE
  
  # Extracting out date column names
  datelist <- names(st.points[8:yrend])
  datelist <- strsplit(datelist, "[.]")
  months <- as.character(sapply(datelist, "[", 2))
  
  # Extracting date columns (the actual data)
  datecols <- st.points[,8:yrend]  #STATE
  
  # Change column names to "01", "02", ect.
  names(datecols) <- months
  # Columns are days, rows represent individual points
  
  # 2 col DF with month in one and temperature in other
  temp.df <- data.frame(month=months)
  temp.df <- cbind(temp.df, t(datecols))
  # Each row is one day; each column is one point
  
  # Takes mean, max, min of each row, adds it on as a new column
  temp.df$m.mean <- rowMeans(temp.df[,-1])
  temp.df$m.max <- do.call(pmax, temp.df[,-1])
  temp.df$m.min <- do.call(pmax, temp.df[,-1])
  
  # Takes mean, max, min of each month (set of rows)
  m.mean <- ddply(.data=temp.df,.(month),summarize,mean=mean(m.mean))
  m.max <- ddply(.data=temp.df,.(month),summarize,m.max=max(m.max))
  m.min <- ddply(.data=temp.df,.(month),summarize,m.min=min(m.min))
  
  # Putting it all into a DF
  m.stats.info <- data.frame(month=unique(sapply(datelist, "[[", 2)), m.mean=m.mean$mean, m.max=m.max$m.max, m.min=m.min$m.min)  # m.stats.state now has monthly stats for a whole state of a given year

  # Attatching year, state, FIPS info to monthly stats of input year
  m.stats.one.yr <- data.frame(year=year, state=state.abb, FIPS=fips) # based on file year and info from st.points; STATE
  m.stats.one.yr <- cbind(m.stats.one.yr, m.stats.info)
  
  # Binning m.stats.one.yr to DF for all years that the loop cycles through
  m.stats.all.yr <- rbind(m.stats.all.yr, m.stats.one.yr)
  
}

## Saving state stats as a .csv  

# write to csv file with naming according to year
#temp.name <- paste(c("m.stats.t2m_", state.abb, ".csv"), collapse="")   #STATE
write.csv(m.stats.all.yr, file="m.stats.t2m.VT.1982-2010.csv")          #STATE
