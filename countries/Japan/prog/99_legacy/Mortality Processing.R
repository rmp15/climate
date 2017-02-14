### Mortality data
rm(list=ls())

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
library("mgcv")

setwd("C:/Users/lmitchis/Documents/R/usa_shapefiles")
state.points.codes.df <- read.csv(file="state.points.codes.csv", header=TRUE)
state.points.codes.df <- state.points.codes.df[-1]

statecodes <- unique(state.points.codes.df[,c('STATE_ABBR','state_fips')])

regional <- read.csv("regional.csv", header=TRUE)
regional <- regional[-1]

setwd("~/R/Mortality data")
#usa.mortality <- readRDS("datus_state_rates_1982_2010")
#usa.mortality <- merge(x=statecodes, y=usa.mortality, by.x="state_fips", by.y="fips")
# Have to use readRDS() from package "mgcv"
# Attaching temperature data comes later


## Re-naming labels for clarification and attaching "sub region" column

#usa.mortality$sex[usa.mortality$sex == "1"] <- "Male"
#usa.mortality$sex[usa.mortality$sex == "2"] <- "Female"
#usa.mortality$age <- factor(usa.mortality$age, labels=c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))

usa.mortality <- merge(usa.mortality, regional)


#write.csv(usa.mortality, "usa.mortality2.csv")

usa.mortality <- read.csv("usa.mortality2.csv", header=TRUE)
usa.mortality$X <- NULL

#usa.mortality$month <- factor(usa.mortality$month, labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

### OK

OK.mortality <- usa.mortality[which(usa.mortality$state_fips==40),]

## Combining with temp. data

# Get WY.means from the Temp Data folder
setwd("~/R/Temp data/Monthly stats")
OK.stats <- read.csv("m.stats.OK.1982-2010.csv", header=TRUE)
OK.stats <- OK.stats[-1]
setwd("~/R/Mortality data")

# Merge
OK.temp.mort.df <- merge(x=OK.stats, y=OK.mortality, by.x=c("year", "month", "FIPS"), by.y=c("year", "month", "state_fips"))

## Plotting
OK.plot <- qplot(x=m.max, y=rate.adj*100000, data=OK.temp.mort.df)

# Both sexes combined
OK.plot +
  aes(color=factor(month)) +
  #aes(color=factor(sex)) +
  #scale_colour_brewer(palette = "Set1") +
  facet_wrap(~age,scales='free') +
  stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2) +
  labs(list(title="OK mortality rate and max. monthly temp by age", x="Monthly max. temperature", y="Adj. mortality rate (deaths per 100,000)")) +
  theme_bw()

qplot(x=m.max, y=rate.adj*100000, data=OK.temp.mort.df[which(OK.temp.mort.df$sex=="Female"),]) +
  aes(color=factor(month)) +
  facet_wrap(~age,scales='free') +
  stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2) +
  labs(list(title="OK female mortality rate and max. monthly temp by age", x="Monthly max. temperature", y="Adj. mortality rate (deaths per 100,000)", colour="Month")) +
  theme_bw()



# Faceted by sex
OK.plot +
  aes(color=factor(month)) +
  facet_wrap(sex~age,scales='free') +
  stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2) +
  labs(list(title="OK mortality rate and max. monthly temp by sex and age", x="Monthly max. temperature", y="Adj. mortality rate (deaths per 100,000)"))

# Facetting one age group by MONTH and sex
qplot(x=m.max, y=rate.adj*100000, data=OK.temp.mort.df[which(OK.temp.mort.df$age=="65-74"),]) +
  aes(color=factor(sex)) +
  facet_wrap(sex~month, scales='free') +
  stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2)


### Function that plots temp and mort data



tempmort.plotting <- function(stateabbr) {
  
# Reading in the monthly temperature statistics
  setwd("~/R/Temp data/Monthly Stats")
  stats.filename <- paste(c("m.stats.", stateabbr, ".1982-2010.csv"), collapse="")
  st.stats <- read.csv(stats.filename, header=TRUE)
  st.stats <- st.stats[-1]
  # st.stats = state stats file with the monthly mean, max, and min temps
  setwd("~/R/Temp data")
  
# Reading in the mortality data
  setwd("~/R/Mortality data")
  st.mortality <- usa.mortality[which(usa.mortality$STATE_ABBR==stateabbr),]
  
# Merge
  st.temp.mort.df <- merge(x=st.stats, y=st.mortality, by.x=c("year", "month", "FIPS"), by.y=c("year", "month", "state_fips"))

# Plotting  
  st.plotbase <- qplot(x=m.max, y=rate.adj*100000, data=st.temp.mort.df)
  
  ptitle <- paste(c(stateabbr, " mortality rate and max. monthly temp by age"))
  
  st.plot <- st.plotbase +
    aes(color=factor(month)) +
    facet_wrap(sex~age,scales='free') +
    stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2) +
    labs(list(title=ptitle, x="Monthly max. temperature", y="Adj. mortality rate (deaths per 100,000)"))  

  return(st.plot)
}

tempmort.plotting("OK")


#### Things for presentation


### st

st.mortality <- usa.mortality[which(usa.mortality$state_fips==25),] #STATE

setwd("~/R/Temp data/Monthly Stats")
st.stats <- read.csv("m.stats.MA.1982-2010.csv", header=TRUE) #STATE
st.stats <- st.stats[-1]
setwd("~/R/Mortality data")

st.temp.mort.df <- merge(x=st.stats, y=st.mortality, by=c("year", "month", "FIPS"), by.y=c("year", "month", "state_fips"))

st.plot <- qplot(x=m.max, y=rate.adj*100000, data=st.temp.mort.df)


# One sex, colored by month
qplot(x=m.max, y=rate.adj*100000, data=st.temp.mort.df[which(st.temp.mort.df$sex=="Female"),]) +
  geom_point(color="green") +
  scale_y_continuous(limits = c(0, 400)) +
  facet_wrap(~age,scales='free_x') +
  stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2) +
  labs(list(title="MA male mortality", x="Monthly max. temperature", y="Adj. mortality rate (deaths per 100,000)", colour="Month")) +
  theme_bw()

# One sex and one age group, colored by month, faceted sex
qplot(x=m.max, y=rate.adj*100000, data=st.temp.mort.df[which(st.temp.mort.df$sex=="Male" & st.temp.mort.df$age=="65-74"),]) +
  aes(color=factor(month)) +
  scale_y_continuous(limits = c(150, 400)) +
  stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2) +
  labs(list(title="GA mortality of males aged 65-74", x="Monthly max. temperature", y="Adj. mortality rate (deaths per 100,000)", colour="Month")) +
  theme_bw()

# One sex and one age group, colored by month, faceted by year
qplot(x=m.max, y=rate.adj*100000, data=st.temp.mort.df[which(st.temp.mort.df$sex=="Male" & st.temp.mort.df$age=="65-74"),]) +
  aes(color=factor(month)) +
  facet_wrap(~year,scales='free_x') +
  stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2) +
  labs(list(title="MA mortality of males aged 65-74", x="Monthly max. temperature", y="Adj. mortality rate (deaths per 100,000)", colour="Month")) +
  theme_bw()

# Both sexes combined 
st.plot +
  #aes(color=factor(month)) +
  aes(color=factor(sex)) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~age,scales='free') +
  stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2) +
  labs(list(title="OK mortality rate and max. monthly temp by age", x="Monthly max. temperature", y="Adj. mortality rate (deaths per 100,000)", colour="Sex")) +
  theme_bw()



