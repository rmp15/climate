rm(list=ls())
setwd("C:/Users/lmitchis/Documents/R/usa_shapefiles")
global_lon_lat.df = read.csv("global_lon_lat.csv", header=TRUE)
state.points.codes.df = read.csv(file="state.points.codes.csv", header=TRUE)
state.points.codes.df <- state.points.codes.df[-1]
setwd("~/R/Temp data")

yearlydata.csv <- c(
  "worldwide_t2m_1982.csv",
  "worldwide_t2m_1983.csv",
  "worldwide_t2m_1984.csv",
  "worldwide_t2m_1985.csv",
  "worldwide_t2m_1986.csv")

m.stats.all.yr <- data.frame(matrix(ncol=7))
names(m.stats.all.yr) <- c("year", "state", "fips", "month", "m.mean", "m.max", "m.min")
m.stats.all.yr <- na.omit(m.stats.all.yr)

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
  st.points <- usa.merge[which(usa.merge$STATE_ABBR=='RI'),]  #STATE
  state.abb <- "RI"                                           #STATE
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

write.csv(m.stats.all.yr, file="m.stats.t2m.RI.1982-1986.csv")

#####

rm(list=ls())
setwd("C:/Users/lmitchis/Documents/R/usa_shapefiles")
global_lon_lat.df = read.csv("global_lon_lat.csv", header=TRUE)
state.points.codes.df = read.csv(file="state.points.codes.csv", header=TRUE)
state.points.codes.df <- state.points.codes.df[-1]
setwd("~/R/Temp data")

yearlydata.csv <- c(
  "worldwide_t2m_1987.csv",
  "worldwide_t2m_1988.csv",
  "worldwide_t2m_1989.csv",
  "worldwide_t2m_1990.csv",
  "worldwide_t2m_1991.csv")

m.stats.all.yr <- read.csv("m.stats.t2m.RI.1982-1986.csv", header=TRUE)
m.stats.all.yr <- m.stats.all.yr[-1]

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
  st.points <- usa.merge[which(usa.merge$STATE_ABBR=='RI'),]  #STATE
  state.abb <- "RI"                                           #STATE
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

write.csv(m.stats.all.yr, file="m.stats.t2m.RI.1982-1991.csv")

#####

rm(list=ls())
setwd("C:/Users/lmitchis/Documents/R/usa_shapefiles")
global_lon_lat.df = read.csv("global_lon_lat.csv", header=TRUE)
state.points.codes.df = read.csv(file="state.points.codes.csv", header=TRUE)
state.points.codes.df <- state.points.codes.df[-1]
setwd("~/R/Temp data")

yearlydata.csv <- c(
  "worldwide_t2m_1992.csv",
  "worldwide_t2m_1993.csv",
  "worldwide_t2m_1994.csv",
  "worldwide_t2m_1995.csv",
  "worldwide_t2m_1996.csv")

m.stats.all.yr <- read.csv("m.stats.t2m.RI.1982-1991.csv", header=TRUE)
m.stats.all.yr <- m.stats.all.yr[-1]

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
  st.points <- usa.merge[which(usa.merge$STATE_ABBR=='RI'),]  #STATE
  state.abb <- "RI"                                           #STATE
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

write.csv(m.stats.all.yr, file="m.stats.t2m.RI.1982-1996.csv")

#####

rm(list=ls())
setwd("C:/Users/lmitchis/Documents/R/usa_shapefiles")
global_lon_lat.df = read.csv("global_lon_lat.csv", header=TRUE)
state.points.codes.df = read.csv(file="state.points.codes.csv", header=TRUE)
state.points.codes.df <- state.points.codes.df[-1]
setwd("~/R/Temp data")

yearlydata.csv <- c(
  "worldwide_t2m_1997.csv",
  "worldwide_t2m_1998.csv",
  "worldwide_t2m_1999.csv",
  "worldwide_t2m_2000.csv",
  "worldwide_t2m_2001.csv")

m.stats.all.yr <- read.csv("m.stats.t2m.RI.1982-1996.csv", header=TRUE)
m.stats.all.yr <- m.stats.all.yr[-1]

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
  st.points <- usa.merge[which(usa.merge$STATE_ABBR=='RI'),]  #STATE
  state.abb <- "RI"                                           #STATE
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

write.csv(m.stats.all.yr, file="m.stats.t2m.RI.1982-2001.csv")

#####

rm(list=ls())
setwd("C:/Users/lmitchis/Documents/R/usa_shapefiles")
global_lon_lat.df = read.csv("global_lon_lat.csv", header=TRUE)
state.points.codes.df = read.csv(file="state.points.codes.csv", header=TRUE)
state.points.codes.df <- state.points.codes.df[-1]
setwd("~/R/Temp data")

yearlydata.csv <- c(
  "worldwide_t2m_2002.csv",
  "worldwide_t2m_2003.csv",
  "worldwide_t2m_2004.csv",
  "worldwide_t2m_2005.csv",
  "worldwide_t2m_2006.csv")

m.stats.all.yr <- read.csv("m.stats.t2m.RI.1982-2001.csv", header=TRUE)
m.stats.all.yr <- m.stats.all.yr[-1]

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
  st.points <- usa.merge[which(usa.merge$STATE_ABBR=='RI'),]  #STATE
  state.abb <- "RI"                                           #STATE
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

write.csv(m.stats.all.yr, file="m.stats.t2m.RI.1982-2006.csv")

#####

rm(list=ls())
setwd("C:/Users/lmitchis/Documents/R/usa_shapefiles")
global_lon_lat.df = read.csv("global_lon_lat.csv", header=TRUE)
state.points.codes.df = read.csv(file="state.points.codes.csv", header=TRUE)
state.points.codes.df <- state.points.codes.df[-1]
setwd("~/R/Temp data")

yearlydata.csv <- c(
  "worldwide_t2m_2007.csv",
  "worldwide_t2m_2008.csv",
  "worldwide_t2m_2009.csv",
  "worldwide_t2m_2010.csv")

m.stats.all.yr <- read.csv("m.stats.t2m.RI.1982-2006.csv", header=TRUE)
m.stats.all.yr <- m.stats.all.yr[-1]

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
  st.points <- usa.merge[which(usa.merge$STATE_ABBR=='RI'),]  #STATE
  state.abb <- "RI"                                           #STATE
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

write.csv(m.stats.all.yr, file="m.stats.t2m.RI.1982-2010.csv")
